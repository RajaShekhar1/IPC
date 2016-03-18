# Celery tasks

import celery
from taa.services.users import UserService

from taa import db, mandrill_flask, app as taa_app
from taa.services import LookupService
from taa.services.enrollments import SelfEnrollmentEmailLog

app = celery.Celery('tasks')
app.config_from_object('taa.config_defaults')


@app.task
def send_email(email_log_id):
    self_enrollment_email_service = LookupService('SelfEnrollmentEmailService')

    email_log = self_enrollment_email_service.get(email_log_id)
    if email_log.status != SelfEnrollmentEmailLog.STATUS_PENDING:
        print("Background task found email no longer in pending state, aborting")
        return

    success = _send_email_from_batch(email_log)
    _update_log(email_log, success)


def _send_email_from_batch(email_log):
    self_enrollment_email_service = LookupService('SelfEnrollmentEmailService')

    success = self_enrollment_email_service._send_email(
        from_email=email_log.batch.email_from_address,
        from_name=email_log.batch.email_from_name,
        subject=email_log.batch.email_subject,
        to_email=email_log.email_to_address,
        to_name=email_log.email_to_name,
        # use the log's body, which has the rendered email
        body=email_log.email_body,
    )
    return success


def _update_log(email_log, success):
    if success:
        email_log.success = True
        email_log.status = SelfEnrollmentEmailLog.STATUS_SUCCESS
    else:
        email_log.success = False
        email_log.status = SelfEnrollmentEmailLog.STATUS_FAILURE
    db.session.commit()


FIVE_MINUTES = 5 * 60


@app.task(bind=True, default_retry_delay=FIVE_MINUTES)
def process_enrollment_upload(task, batch_id):

    submission_service = LookupService("EnrollmentSubmissionService")
    errors = submission_service.process_import_submission_batch(batch_id)
    if errors:
        send_admin_error_email(u"Error processing submission batch {}".format(batch_id), errors)
        # Go ahead and attempt to reprocess
        task.retry(exc=Exception(errors[0]))


@app.task(bind=True, default_retry_delay=FIVE_MINUTES)
def process_wizard_enrollment(task, enrollment_id):

    submission_service = LookupService("EnrollmentSubmissionService")
    envelope = submission_service.process_wizard_submission(enrollment_id)
    if not envelope:
        send_admin_error_email(u"Error processing wizard enrollment {}".format(enrollment_id), [])
        # Go ahead and attempt to reprocess
        task.retry(exc=Exception("No envelope created"))


def send_admin_error_email(error_message, errors):
    try:
        tracebacks = '\n<br>'.join([err for err in errors])
        body = u"{} <br><br>Tracebacks: <br><br>{}".format(
            error_message, tracebacks.replace('<br>', '\n')
        )

        # Get stormpath admins
        for account in UserService().get_admin_users():
            mandrill_flask.send_email(
                to=[{'email': account.email, 'name': account.full_name}],
                from_email="errors@5StarEnroll.com",
                from_name=u"TAA Error {}".format(taa_app.config['HOSTNAME']),
                subject=u"5Star Import Error ({})".format(taa_app.config['HOSTNAME']),
                html=body,
                auto_text=True,
            )
    except Exception:
        # Swallow this
        pass

