# Celery tasks
from datetime import datetime

import celery
from taa.services.users import UserService

from taa import db, mandrill_flask, app as taa_app
from taa.services import LookupService
from taa.services.enrollments import SelfEnrollmentEmailLog
from taa.services.enrollments.models import EnrollmentSubmission, SubmissionLog
from taa.services.enrollments.csv_export import *
import traceback
from taa.errors import email_exception
import time

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


# noinspection PyBroadException
@app.task(bind=True, default_retry_delay=FIVE_MINUTES)
def process_hi_acc_enrollments(task):
    submission_service = LookupService('EnrollmentSubmissionService')

    # Set all the submissions to be processing so they do not get pulled in by another worker thread
    submissions = submission_service.get_pending_submissions()
    for submission in submissions:
        submission.set_status_processing()
    db.session.commit()

    # Create submission logs for each submission and accumulate all the enrollment applications
    applications = list()
    logs = list()
    for submission in submissions:
        # noinspection PyArgumentList
        log = SubmissionLog(enrollment_submission_id=submission.id, status=SubmissionLog.STATUS_PROCESSING)
        logs.append(log)
        db.session.add(log)
        if submission.enrollment_application not in applications:
            applications.append(submission.enrollment_application)
    db.session.commit()

    try:
        csv_data = export_hi_acc_enrollments(applications)
        try:
            # TODO: Send CSV to Dell instead of saving to a file
            today = datetime.today()
            filename = '/Users/mnowak/temp/five-star/enrollment-submissions_%04d-%02d-%02d.csv' % (
                today.year, today.month, today.day)
            with open(filename, 'w+') as csv_file:
                csv_file.write(csv_data)
                csv_file.close()
        except Exception as ex:
            for submission in submissions:
                submission.set_status_failure()
            for log in logs:
                log.set_status_failure()
                log.message = 'Sending to Dell failed with exception: "%s"\n%s' % ex.message, traceback.format_exc()
            db.session.commit()
            email_exception(taa_app, ex)
            return

        for submission in submissions:
            submission.set_status_success()
        for log in logs:
            log.set_status_success()
        db.session.commit()

    except Exception as ex:
        for submission in submissions:
            submission.set_status_failure()
        for log in logs:
            log.set_status_failure()
        db.session.commit()
        email_exception(taa_app, ex)


# noinspection PyBroadException
@app.task(bind=True, default_retry_delay=FIVE_MINUTES)
def submit_csv_to_dell(task, submission_id):
    """
    Task to submit a csv item to dell for processing
    :param task:
    :type task: celery.task
    :param submission_id:
    :type submission_id: int
    """

    submission_service = LookupService('EnrollmentSubmissionService')
    """:type : taa.services.enrollments.enrollment_submission.EnrollmentSubmissionService"""
    submission = submission_service.get_submission_by_id(submission_id)
    # noinspection PyArgumentList
    log = SubmissionLog(enrollment_submission_id=submission_id, status=SubmissionLog.STATUS_PROCESSING)

    try:
        submission_service.submit_hi_acc_export_to_dell(submission.data)
        submission.set_status_success()
        log.set_status_success()
        log.message = time.strftime(
            'HI and ACC enrollment applications were successfully submitted to Dell on %x at %X %Z.')
        db.session.commit()
    except Exception as ex:
        submission.set_status_failure()
        log.set_status_failure()
        log.message = 'Error sending CSV to Dell with message "%s"\n%s' % ex.message, traceback.format_exc()
        db.session.commit()
        task.retry()
