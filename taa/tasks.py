# Celery tasks

import celery
from taa.services.users import UserService

from taa import db, app as taa_app
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


@app.task
def send_summary_email(standardized_data, wizard_results, enrollment_application_id, body):
    from taa.models import SummaryEmailLog
    enrollment_application_service = LookupService("EnrollmentApplicationService")
    enrollment_application = enrollment_application_service.get_enrollment_by_id(enrollment_application_id)
    if not enrollment_application:
        print ("Could not get record for Enrollment. Not sending email.")
        return
    email_summary_service = LookupService("SummaryEmailService")
    record, status = email_summary_service.send_summary_email(
        standardized_data=standardized_data,
        wizard_results=wizard_results,
        enrollment_application=enrollment_application,
        body=body,
    )
    if status == SummaryEmailLog.STATUS_SUCCESS:
        record.is_success = True
        record.status = SummaryEmailLog.STATUS_SUCCESS
    else:
        record.status = SummaryEmailLog.STATUS_FAILURE
    db.session.commit()


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
            mailer = LookupService('MailerService')
            mailer.send_email(
                to=["{name} <{email}>".format(**{'email': account.email, 'name': account.full_name})],
                from_email="errors@5StarEnroll.com",
                from_name=u"TAA Error {}".format(taa_app.config['HOSTNAME']),
                subject=u"5Star Import Error ({})".format(taa_app.config['HOSTNAME']),
                html=body,
            )
    except Exception:
        # Swallow this
        pass


@app.task(bind=True, default_retry_delay=FIVE_MINUTES)
def process_hi_acc_enrollments(task):
    submission_service = LookupService('EnrollmentSubmissionService')

    # Set all the submissions to be processing so they do not get pulled in by another worker thread
    #submissions = submission_service.get_pending_or_failed_csv_submissions()


    submission = submission_service.get_pending_batch_submission(EnrollmentSubmission.TYPE_DELL_CSV_GENERATION)
    if not submission:
       return
    submissions = [submission]

    submission_service.set_submissions_status(EnrollmentSubmission.STATUS_PROCESSING, submissions)

    # Create submission logs for each submission and accumulate all the enrollment applications
    submission_logs = submission_service.create_logs_for_submissions(submissions, SubmissionLog.STATUS_PROCESSING)
    applications = submission_service.get_applications_for_submissions(submissions)
    if applications is None or len(applications) == 0:
        submission_service.set_submissions_status(EnrollmentSubmission.STATUS_SUCCESS, submissions, submission_logs)
        return

    try:
        csv_data = export_hi_acc_enrollments(applications)
        submit_submission = submission_service.create_submission_for_csv(csv_data, applications)
        submission_service.set_submissions_status(EnrollmentSubmission.STATUS_SUCCESS, submissions, submission_logs)
        submit_csv_to_dell.delay(submission_id=submit_submission.id)
    except Exception as ex:
        submission_service.set_submissions_status(EnrollmentSubmission.STATUS_FAILURE, submissions, submission_logs)
        email_exception(taa_app, ex)


@app.task(bind=True, default_retry_delay=FIVE_MINUTES)
def submit_csv_to_dell(task, submission_id):
    """
    Task to submit a csv item to dell for processing
    """

    submission_service = LookupService('EnrollmentSubmissionService')
    submission = submission_service.get_submission_by_id(submission_id)
    log = SubmissionLog()
    log.enrollment_submission_id = submission_id
    log.status = SubmissionLog.STATUS_PROCESSING
    db.session.add(log)

    try:
        submission_service.submit_hi_acc_export_to_dell(submission.data)
        submission.status = EnrollmentSubmission.STATUS_SUCCESS
        log.status = SubmissionLog.STATUS_SUCCESS
        log.message = time.strftime(
            'HI and ACC enrollment applications were successfully submitted to Dell on %x at %X %Z.')
        db.session.commit()
    except Exception as ex:
        submission.status = EnrollmentSubmission.STATUS_FAILURE
        log.status = SubmissionLog.STATUS_FAILURE
        log.message = 'Error sending CSV to Dell with message "%s"\n%s' % (ex.message, traceback.format_exc())
        db.session.commit()
        task.retry()


@app.task(bind=True, default_retry_delay=FIVE_MINUTES)
def process_paylogix_export(task, submission_id):
    submission_service = LookupService('EnrollmentSubmissionService')
    """:type: taa.services.submissions.EnrollmentSubmissionService"""
    submission = None
    # noinspection PyBroadException
    try:
        submission = submission_service.get_submission_by_id(submission_id)
        if not submission:
            return
        submission_service.process_paylogix_export(submission)
    except Exception as ex:
        submission_service.set_submissions_status([submission])
        task.retry()


@app.task(bind=True, default_retry_delay=FIVE_MINUTES)
def process_paylogix_csv_generation(task):
    submission_service = LookupService('EnrollmentSubmissionService')
    """:type: taa.services.submissions.EnrollmentSubmissionService"""
    submission = None
    # noinspection PyBroadException
    try:
        submission = submission_service.get_pending_batch_submission(EnrollmentSubmission.TYPE_PAYLOGIX_CSV_GENERATION)
        if not submission:
            return
        export_submission = submission_service.process_paylogix_csv_generation_submission(submission)
        process_paylogix_export(submission_id=export_submission.id)
    except Exception as ex:
        submission_service.set_submissions_status([submission], error_message=ex.message)
        task.retry()


# Exports that run in the background
@app.task
def export_user_case_enrollments(export_id):
    enrollment_export_service = LookupService('EnrollmentExportService')

    enrollment_export_service.process_export(export_id)


