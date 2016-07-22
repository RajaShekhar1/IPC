from flask import abort, render_template
import requests
import json

from taa.services.submissions.enrollment_submission import EnrollmentSubmissionProcessor
from taa.core import DBService, db
from models import SummaryEmailLog
import taa.config_defaults
from taa.services import RequiredFeature, LookupService


class SummaryEmailService(DBService):
    __model__ = SummaryEmailLog

    enrollment_application_service = RequiredFeature('EnrollmentApplicationService')

    def delete_emails_for_enrollment(self, enrollment):
        for email in enrollment.emails:
            db.session.delete(email)

    def generate_email_body(self, enrollment_application):
        case = enrollment_application.case
        data = json.loads(enrollment_application.standardized_data)
        name = data[0]['employee']['first'] + ' ' + data[0]['employee']['last']
        hostname = taa.config_defaults.HOSTNAME
        greeting = self.build_email_greeting(name)

        return render_template(
            "emails/benefit_notice_email.html",
            greeting=greeting,
            company_name=case.company_name,
            host=hostname,
        )

    def generate_cover_sheet(self, enrollment_application):
        processor = EnrollmentSubmissionProcessor()
        return processor.generate_cover_sheet(enrollment_application)

    def build_email_greeting(self, name):
        return u'Hello {},'.format(name)

    def _send_email(self, enrollment_application, to_email, to_name, body):

        from_email = taa.config_defaults.EMAIL_FROM_ADDRESS
        from_name = u'5Star Enrollment'
        subject = u'5Star Summary of Benefits'
        pdf = self.generate_cover_sheet(enrollment_application)

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
                    'name': 'enrollment_benefits.pdf',
                    'filename': pdf
                }]
            )
        except mailer.Error as e:
            print("Exception sending email: %s - %s; to %s" % (e.__class__, e, to_email))
            return False
        except requests.exceptions.HTTPError as e:
            print("Exception sending email: %s - %s; to %s" % (e.__class__, e, to_email))
            return False
        except Exception as e:
            print ("Exception sending email: %s - %s" % (e.__class__, e))
            return False

        return True

    def send(self, enrollment_application, **kwargs):

        success = self._send_email(enrollment_application, **kwargs)
        if success:
            status = SummaryEmailLog.STATUS_SUCCESS
        else:
            status = SummaryEmailLog.STATUS_FAILURE

        email_record = self.create(**dict(
            enrollment_application_id=enrollment_application.id,
            email_to_address=kwargs.get('to_email'),
            email_to_name=kwargs.get('to_name'),
            email_body=kwargs.get('body'),
            is_success=success,
            status=status
        ))

        db.session.commit()
        return success

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
