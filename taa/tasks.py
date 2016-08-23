# Celery tasks

import celery
import json
import time
import traceback

from taa import db, app as taa_app
from taa.services import LookupService
from taa.services.enrollments import SelfEnrollmentEmailLog
from taa.services.enrollments.models import EnrollmentSubmission, SubmissionLog
from taa.services.enrollments.csv_export import *
from taa.errors import email_exception

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
ONE_HOUR = 1 * 60 * 60


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


@app.task(bind=True, default_retry_delay=ONE_HOUR)
def process_wizard_enrollment(task, enrollment_id):
    submission_service = LookupService("EnrollmentSubmissionService")
    try:
        envelope = submission_service.process_wizard_submission(enrollment_id)
    except Exception, ex:
        send_admin_error_email("Error processing wizard submission for enrollment {}".format(enrollment_id), [traceback.format_exc()])
        task.retry()
    # if not envelope:
    #     send_admin_error_email(u"Error processing wizard enrollment {}".format(enrollment_id), [])
    #     # Go ahead and attempt to reprocess
    #     task.retry(exc=Exception("No envelope created"))


def send_admin_error_email(error_message, error_details):

    details = '\n<br>'.join([err for err in error_details])
    body = u"{} <br><br>Errors: <br><br>{}".format(
        error_message, details.replace('<br>', '\n')
    )

    from taa import errors
    mailer = LookupService('MailerService')
    mailer.send_email(
        to=[e for e in errors.error_recipients],
        from_email=u"TAA Error <errors@5StarEnroll.com>",
        subject=u"5Star Processing Error ({})".format(taa_app.config['HOSTNAME']),
        html=body,
    )


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
        send_admin_error_email("Error generating HI/ACC submissions for Dell", [traceback.format_exc()])


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
    db.session.commit()
    
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

        send_admin_error_email("Error submitting HI/ACC CSV file to Dell", [traceback.format_exc()])
        task.retry()


@app.task(bind=True, default_retry_delay=ONE_HOUR)
def submit_stp_xml_to_dell(task, submission_id):
    """
    Task to submit an STP XML item to Dell for processing. XML has already been generated and is ready for delivery.
    """

    submission = db.session.query(EnrollmentSubmission).get(submission_id)
    data = json.loads(submission.data)
    coverage_id = data['coverage_id']
    xml = data['xml']

    # Create log for this attempt at processing
    log = SubmissionLog()
    log.enrollment_submission_id = submission.id
    log.status = SubmissionLog.STATUS_PROCESSING
    db.session.add(log)
    db.session.commit()
    
    if not xml:
        submission.status = EnrollmentSubmission.STATUS_FAILURE
        log.status = SubmissionLog.STATUS_FAILURE
        log.message = "No XML in data to transmit."
        db.session.commit()
        raise ValueError("Submission ID {}: Attempt to transmit empty XML to Dell, aborting.".format(submission_id))
    
    try:
        result = transmit_stp_xml(xml)

        # Success
        submission.status = EnrollmentSubmission.STATUS_SUCCESS
        log.status = SubmissionLog.STATUS_SUCCESS
        log.message = unicode(result)
        db.session.commit()

    except Exception as ex:
        submission.status = EnrollmentSubmission.STATUS_FAILURE
        log.status = SubmissionLog.STATUS_FAILURE
        log.message = 'Error sending STP XML to Dell with traceback "%s"\n%s' % (ex.message, traceback.format_exc())
        db.session.commit()

        send_admin_error_email("Error transmitting STP XML to Dell, submission {} coverage {}".format(submission_id, coverage_id),
                               [traceback.format_exc()])
        task.retry()


def transmit_stp_xml(xml):
    "Make the actual SOAP call to the Dell web service for submitting XML data."
    from pysimplesoap.client import SoapClient
    # app['IS_STP_STORE_SOURCE']
    # app['IS_STP_STORE_RESULT']
    c = SoapClient(wsdl=taa_app.config['STP_URL'])
    # guid = 'SIMULATEDRUN'
    
    if not taa_app.config['IS_STP_SIMULATE']:
        return c.TXlifeProcessor(xml)
    #     r = result['TXlifeProcessorResult']
    #     guid = r[r.find('TransRefGUID')+13:r.find('TransRefGUID')+13+36]
    # print('[SENT] {}'.format(guid))
    
    return None


@app.task(bind=True, default_retry_delay=ONE_HOUR)
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
        submission_service.set_submissions_status(EnrollmentSubmission.STATUS_FAILURE, [submission])

        send_admin_error_email(
            "Error generating CSV for Paylogix, submission {}".format(submission_id),
            [traceback.format_exc()])
        
        # No retry on this task since it will likely fail without intervention.


@app.task(bind=True, default_retry_delay=ONE_HOUR)
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
        submission_service.set_submissions_status(EnrollmentSubmission.STATUS_FAILURE, [submission], error_message=ex.message)

        send_admin_error_email(
            "Error transmitting CSV to Paylogix",
            [traceback.format_exc()])
        
        task.retry()


# Exports that run in the background
@app.task
def export_user_case_enrollments(export_id):
    enrollment_export_service = LookupService('EnrollmentExportService')

    enrollment_export_service.process_export(export_id)


