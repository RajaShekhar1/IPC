from flask import render_template
import requests
import json

from taa.core import DBService, db
from models import SummaryEmailLog
from taa.services import RequiredFeature, LookupService
import taa.config_defaults


class SummaryEmailService(DBService):
    __model__ = SummaryEmailLog

    enrollment_submission_service = RequiredFeature('EnrollmentSubmissionService')

    def delete_emails_for_enrollment(self, enrollment):
        for email in enrollment.emails:
            db.session.delete(email)

    def generate_email_body(self, enrollment_application):
        case = enrollment_application.case
        data = json.loads(enrollment_application.standardized_data)
        name = data[0]['employee']['first'] + ' ' + data[0]['employee']['last']
        host = taa.config_defaults.PREFERRED_URL_SCHEME + '://' + taa.config_defaults.HOSTNAME+'/'
        
        greeting = self.build_email_greeting(name)

        return render_template(
            "emails/benefit_notice_email.html",
            greeting=greeting,
            company_name=case.company_name,
            host=host,
        )

    def build_email_greeting(self, name):
        return u'Hello {},'.format(name)

    def _send_email(self, to_email, to_name, body, from_name, from_email, subject, pdf):

        mailer = LookupService('MailerService')

        try:
            mailer.send_email(
                to=["{} <{}>".format(to_name, to_email)],
                from_email=from_email,
                from_name=from_name,
                subject=subject,
                html=body,
                attachments=[{
                    'type': 'application/pdf',
                    'name': 'benefit_summary.pdf',
                    'data': pdf
                }]
            )
        except mailer.Error as e:
            print ("Exception sending email: %s - %s; to %s" % (e.__class__, e, to_email))
            message = "Exception sending email: %s - %s; to %s" % (e.__class__, e, to_email)
            return False, message
        except requests.exceptions.HTTPError as e:
            print ("Exception sending email: %s - %s; to %s" % (e.__class__, e, to_email))
            message = "Exception sending email: %s - %s; to %s" % (e.__class__, e, to_email)
            return False, message
        except Exception as e:
            print ("Exception sending email: %s - %s; to %s" % (e.__class__, e, to_email))
            message = "Exception sending email: %s - %s; to %s" % (e.__class__, e, to_email)
            return False, message

        return True, None

    def send(self, enrollment_application, **kwargs):

        email_record = self.create(**dict(
            enrollment_application_id=enrollment_application.id,
            email_to_address=kwargs.get('to_email'),
            email_to_name=kwargs.get('to_name'),
            email_body=kwargs.get('body'),
            is_success=False,
            status=SummaryEmailLog.STATUS_PENDING,
        ))

        success, message = self._send_email(**kwargs)
        if success:
            status = SummaryEmailLog.STATUS_SUCCESS
        else:
            status = SummaryEmailLog.STATUS_FAILURE
        db.session.commit()

        return email_record, status

    def queue_email(self, email_log_id):
        "Sends a pending email log to the queue for sending"

        # Import the tasks module here so we don't have circular import.
        from taa import tasks
        # Queue up the task to be run in the background
        tasks.send_email.delay(email_log_id)

    def create_pending_email(self, enrollment_application, **kwargs):
        # Create a pending record in the database
        return self.create(**dict(
            enrollment_application_id=enrollment_application.id,
            email_to_address=kwargs.get('to_email'),
            email_to_name=kwargs.get('to_name'),
            email_body=kwargs.get('email_body'),
            is_success=False,
            status=SummaryEmailLog.STATUS_PENDING
        ))

    def send_summary_email(self, standardized_data, wizard_results, enrollment_application, body):
        to_name = standardized_data[0]['employee']['first'] + ' ' + standardized_data[0]['employee']['last']
        to_email = wizard_results[0].get('summaryEmail')
        from_name = u'Enrollment - do not reply'
        from_email = u'enrollment-noreply@5starenroll.com'
        if app.config.get("IS_AFBA"):
            subject = "AFBA Summary of Benefits"
        else:
            subject = u'5Star Summary of Benefits'
        enrollment_submission_service = LookupService('EnrollmentSubmissionService')
        pdf = enrollment_submission_service.get_summary_pdf(enrollment_application)
        email_record, status = self.send(enrollment_application, to_email=to_email, to_name=to_name, body=body,
                                         from_name=from_name, from_email=from_email, subject=subject, pdf=pdf)
        return email_record, status
