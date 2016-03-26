from flask import abort, render_template
import mandrill
from taa import mandrill_flask
import requests

from taa.core import DBService, db
from models import (
    SelfEnrollmentEmailLog,
    SelfEnrollmentEmailBatch,
    SelfEnrollmentEmailBatchWithEmails,
)
from taa.services.cases.models import SelfEnrollmentSetup
from taa.services import RequiredFeature


class SelfEnrollmentEmailBatchService(DBService):
    __model__ = SelfEnrollmentEmailBatch


class SelfEnrollmentEmailService(DBService):
    __model__ = SelfEnrollmentEmailLog

    agent_service = RequiredFeature('AgentService')
    case_service = RequiredFeature('CaseService')
    self_enrollment_batch_service = RequiredFeature('SelfEnrollmentEmailBatchService')
    self_enrollment_link_service = RequiredFeature('SelfEnrollmentLinkService')

    def delete_batches_for_case(self, case):
        for batch in case.batches:
            for log in batch.email_logs:
                self.delete(log)
            db.session.delete(batch)

    def get_batches_for_case(self, case):
        return db.session.query(SelfEnrollmentEmailBatch
                                ).filter(SelfEnrollmentEmailBatch.case_id == case.id
                                         ).all()

    def get_batch_for_case(self, case, batch_id):
        return db.session.query(SelfEnrollmentEmailBatchWithEmails
                                ).get(batch_id)

    def create_batch_for_case(self, case, census_records, url_root):

        setup = case.self_enrollment_setup
        agent = self.agent_service.get_logged_in_agent()
        if not agent:
            agent = self.case_service.get_case_owner(case)

        batch = self.self_enrollment_batch_service.create(**dict(
            email_from_address=setup.email_sender_email,
            email_from_name=setup.email_sender_name,
            email_subject=setup.email_subject if setup.email_subject else 'Benefit Enrollment - your action needed',
            email_body=setup.email_message,
            agent_id=agent.id,
            case_id=case.id
        ))

        results = []

        for record in census_records:
            if record.employee_email is None or '@' not in record.employee_email:
                # Census record does not has a valid email; TODO insert into email log in as failure.
                # results.append("{} {} did not have a valid email.".format(record.employee_first, record.employee_last))
                continue

            # Get previously generated link if available
            link = self.self_enrollment_link_service.get_for_census_record(record)
            if link is None:
                # Otherwise generate one
                link = self.self_enrollment_link_service.generate_link(url_root,
                                                                       case, record)
            if link is None:
                abort(500, "Could not retrieve or create self-enrollment link")

            name = u'{} {}'.format(record.employee_first, record.employee_last)

            email_log = self.create_pending_email(
                agent, link, record, batch,
                to_email=record.employee_email,
                to_name=name,
                email_body=self.generate_email_body(link, record)
            )
            results.append(email_log)
        return results

    def generate_email_body(self, link, census_record):
        case = census_record.case
        setup = case.self_enrollment_setup

        return render_template(
            "emails/enrollment_email.html",
            custom_message=setup.email_message,
            greeting=self.build_email_greeting(census_record),
            enrollment_url=link.url,
            company_name=case.company_name,
            products=case.products
        )

    def build_email_greeting(self, record):
        setup = record.case.self_enrollment_setup

        salutation = ''
        if setup.email_greeting_salutation:
            salutation = u'{} '.format(setup.email_greeting_salutation)
        greeting_end = ''
        if setup.email_greeting_type == SelfEnrollmentSetup.EMAIL_GREETING_FIRST_NAME:
            greeting_end = u"{},".format(record.employee_first)
        elif setup.email_greeting_type == SelfEnrollmentSetup.EMAIL_GREETING_FULL_NAME:
            greeting_end = u"{} {},".format(record.employee_first, record.employee_last)
        elif setup.email_greeting_type == SelfEnrollmentSetup.EMAIL_GREETING_LAST_NAME:
            greeting_end = u"{},".format(record.employee_last)
        elif setup.email_greeting_type == SelfEnrollmentSetup.EMAIL_GREETING_TITLE_LAST:
            if not record.employee_gender:
                title = 'Mr./Ms.'
            elif record.employee_gender.lower()[0] == 'm':
                title = 'Mr.'
            else:
                title = 'Mrs.'

            greeting_end = u"{} {},".format(title, record.employee_last)
        elif setup.email_greeting_type == SelfEnrollmentSetup.EMAIL_GREETING_BLANK:
            greeting_end = ''
        greeting = u"{}{}".format(salutation, greeting_end)
        return greeting

    def get_for_census_record(self, census_record):
        # record = CaseService.get_census_record(census_record_id)
        # return self.find(census_id=census_record.id)
        return self.find(census_id=census_record.id).all()

    def _send_email(self, from_email, from_name, to_email, to_name, subject,
                    body):
        try:
            mandrill_flask.send_email(
                to=[{'email': to_email, 'name': to_name}],
                from_email=from_email,
                from_name=from_name,
                subject=subject,
                html=body,
                auto_text=True,
            )
        except mandrill.Error as e:
            print("Exception sending email: %s - %s; to %s" % (e.__class__, e, to_email))
            return False
        except requests.exceptions.HTTPError as e:
            print("Exception sending email: %s - %s; to %s" % (e.__class__, e, to_email))
            return False
        except Exception as e:
            print "Exception sending email: %s - %s" % (e.__class__, e)
            return False

        return True

    def send(self, agent, link, census, **kwargs):

        success = self._send_email(**kwargs)

        email_record = self.create(**dict(
            link_id=link.id,
            census_id=census.id,
            agent_id=agent.id,
            email_to_address=kwargs.get('to_email'),
            email_to_name=kwargs.get('to_name'),
            email_from_address=kwargs.get('from_email'),
            email_from_name=kwargs.get('from_name'),
            email_subject=kwargs.get('subject'),
            email_body=kwargs.get('body'),
            is_success=success,
        ))

        db.session.commit()
        return success

    def queue_email(self, email_log_id):
        "Sends a pending email log to the queue for sending"

        # Import the tasks module here so we don't have circular import.
        from taa import tasks
        # Queue up the task to be run in the background
        tasks.send_email.delay(email_log_id)

    def create_pending_email(self, agent, link, census, batch, **kwargs):
        # Create a pending record in the database
        return self.create(**dict(
            link_id=link.id,
            census_id=census.id,
            agent_id=agent.id,
            email_to_address=kwargs.get('to_email'),
            email_to_name=kwargs.get('to_name'),
            email_body=kwargs.get('email_body'),
            batch_id=batch.id,
            is_success=False,
            status=SelfEnrollmentEmailLog.STATUS_PENDING
        ))
