# Celery tasks

import celery

from taa import db
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
def process_enrollment_upload(task, enrollment_import_log_id):
    #try:
    submission_service = LookupService("EnrollmentSubmissionService")
    submission_service.process_import_submission_batch(enrollment_import_log_id)
    #except Exception as exc:
    #    task.retry(exc=exc)

# @app.task(bind=True, default_retry_delay=FIVE_MINUTES)
# def submit_enrollment_import(task, enrollment_application_id):
#     """
#     Takes a validated enrollment application import and submits it to DocuSign or Dell.
#     """
#     try:
#         submission_service = LookupService("EnrollmentSubmissionService")
#         submission_service.process_import_submission(enrollment_application_id)
#     except Exception as exc:
#         task.retry(exc=exc)
#
