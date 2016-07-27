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
ONE_HOUR = 1 * 60 * 60


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
    # if not envelope:
    #     send_admin_error_email(u"Error processing wizard enrollment {}".format(enrollment_id), [])
    #     # Go ahead and attempt to reprocess
    #     task.retry(exc=Exception("No envelope created"))


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


def generate_pdf(submission_id, coverage_id):
    """
    Task to generate an enrollment PDF
    """
    submission_service = LookupService('EnrollmentSubmissionService')
    submission = submission_service.get_submission_by_id(submission_id)
    enrollment_record = submission.enrollment_applications[0]
    coverage = LookupService('EnrollmentApplicationCoverageService').get(coverage_id)
    return submission_service.render_enrollment_pdf(enrollment_record, is_stp=True, product_id=coverage.product_id)


# def generate_xml(submission_id):
#     """
#     Task to generate XML STP item
#     """
#     submission_service = LookupService('EnrollmentSubmissionService')
#     submission = submission_service.get_submission_by_id(submission_id)
#     pdf_bytes = generate_pdf(submission.id)
#     return submission_service.render_enrollment_xml(submission_id, 'employee', pdf_bytes)


# @app.task(bind=True, default_retry_delay=ONE_HOUR)
# def process_uploaded_stp_enrollments(task):
#     submission_service = LookupService('EnrollmentSubmissionService')
#
#     # Set all the submissions to be processing so they do not get pulled in by another worker thread
#     submissions = submission_service.get_pending_stp_xml_submission()
#     if submissions is None or len(submissions) == 0:
#         return
#     submission_service.set_submissions_status(EnrollmentSubmission.STATUS_PROCESSING, submissions)
#
#     # Create submission logs for each submission and accumulate all the enrollment applications
#     submission_logs = submission_service.create_logs_for_submissions(submissions, SubmissionLog.STATUS_PROCESSING)
#     applications = submission_service.get_applications_for_submissions(submissions)
#     if applications is None or len(applications) == 0:
#         submission_service.set_submissions_status(EnrollmentSubmission.STATUS_SUCCESS, submissions, submission_logs)
#         return
#
#     try:
#         xml_data = export_hi_acc_enrollments(applications)
#         submit_submission = submission_service.create_submission_for_csv(xml_data, applications)
#         submission_service.set_submissions_status(EnrollmentSubmission.STATUS_SUCCESS, submissions, submission_logs)
#         submit_csv_to_dell.delay(submission_id=submit_submission.id)
#     except Exception as ex:
#         submission_service.set_submissions_status(EnrollmentSubmission.STATUS_FAILURE, submissions, submission_logs)
#         email_exception(taa_app, ex)


def send_stp_xml(xml):
    # import time
    from pysimplesoap.client import SoapClient
    # app['IS_STP_STORE_SOURCE']
    # app['IS_STP_STORE_RESULT']
    c = SoapClient(wsdl=app['STP_URL'])
    # start = time.time()
    xml = ''
    # guid = 'SIMULATEDRUN'
    result = None
    try:
        if not taa_app.config['IS_STP_SIMULATE']:
            result = c.TXlifeProcessor(xml)
        #     r = result['TXlifeProcessorResult']
        #     guid = r[r.find('TransRefGUID')+13:r.find('TransRefGUID')+13+36]
        # print('[SENT] {}'.format(guid))
    except:
        pass
        # print('[FAIL] {}'.format(guid))
    return result
    # stop = time.time()
    # print('took {:.2f} sec'.format(stop - start))


@app.task(bind=True, default_retry_delay=ONE_HOUR)
def submit_stp_xml_to_dell(task, submission_id, product_id):
    """
    Task to submit an STP XML item to Dell for processing
    """

    submission_service = LookupService('EnrollmentSubmissionService')
    submission = submission_service.get_submission_by_id(submission_id)
    
    # Create log for this attempt at processing
    log = SubmissionLog()
    log.enrollment_submission_id = submission_id
    log.status = SubmissionLog.STATUS_PROCESSING
    
    # Find the coverage record
    
    # Look up the applicant_coverage record
    coverage_service = LookupService('EnrollmentApplicationCoverageService')
    coverage = coverage_service.get(coverage_id)
    
    pdf_bytes = generate_pdf(submission.id, coverage_id)
    if coverage.applicant_type != "children":
        xml = submission_service.render_enrollment_xml(coverage, coverage.applicant_type, pdf_bytes)
    else:
        data = EnrollmentApplicationService().get_wrapped_data_for_coverage(coverage)
        for i, child in enumerate(data['children']):
            print("Generating child {}".format(i + 1))
            applicant_type = "child{}".format(i + 1)
            xml = submission_service.render_enrollment_xml(coverage, applicant_type, pdf_bytes)
            
    submission.data = xml
    db.session.add(log)

    try:
        # submission_service.submit_hi_acc_export_to_dell(submission.data)
        submission_service.submit_to_dell(submission.data)
        submission.status = EnrollmentSubmission.STATUS_SUCCESS
        log.status = SubmissionLog.STATUS_SUCCESS
        log.message = time.strftime(
                'STP XML enrollment applications were successfully submitted to Dell on %x at %X %Z.')
        db.session.commit()
    except Exception as ex:
        submission.status = EnrollmentSubmission.STATUS_FAILURE
        log.status = SubmissionLog.STATUS_FAILURE
        log.message = 'Error sending STP XML to Dell with message "%s"\n%s' % (ex.message, traceback.format_exc())
        db.session.commit()
        # task.retry()


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


