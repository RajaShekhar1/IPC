# Celery tasks

import json
import time
import traceback
from datetime import datetime
from xml.etree import ElementTree
import os
import ssl
from urlparse import urlparse

from celery import Celery
import flask
from pysimplesoap.client import SoapClient
from pysimplesoap.transport import set_http_wrapper

from taa import db, app as taa_app
from taa.services import LookupService
from taa.services.enrollments import SelfEnrollmentEmailLog
from taa.services.enrollments.models import EnrollmentSubmission, SubmissionLog
from taa.services.enrollments.csv_export import *

#app = celery.Celery('tasks')
#app.config_from_object('taa.config_defaults')


# http://stackoverflow.com/questions/12044776/how-to-use-flask-sqlalchemy-in-a-celery-task
class FlaskCelery(Celery):
    def __init__(self, *args, **kwargs):

        super(FlaskCelery, self).__init__(*args, **kwargs)
        self.patch_task()

        if 'app' in kwargs:
            self.init_app(kwargs['app'])

    def patch_task(self):
        TaskBase = self.Task
        _celery = self

        # This base class for tasks ensures that tasks run inside a proper flask context, which allows sqlalchemy sessions
        #   to get set up properly when using flask_sqlalchemy.
        class ContextTask(TaskBase):
            abstract = True

            def __call__(self, *args, **kwargs):
                if flask.has_app_context():
                    return TaskBase.__call__(self, *args, **kwargs)
                else:
                    with _celery.app.app_context():
                        return TaskBase.__call__(self, *args, **kwargs)

            def after_return(self, status, retval, task_id, args, kwargs, einfo):
                db.session.remove()
                
        self.Task = ContextTask

    def init_app(self, app):
        self.app = app
        self.config_from_object(app.config)


celery = FlaskCelery()


@celery.task()
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


@celery.task()
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


@celery.task(bind=True, default_retry_delay=FIVE_MINUTES)
def process_enrollment_upload(task, batch_id):
    submission_service = LookupService("EnrollmentSubmissionService")
    errors = submission_service.process_import_submission_batch(batch_id)
    if errors:
        send_admin_error_email(u"Error processing submission batch {}".format(batch_id), errors)
        # Go ahead and attempt to reprocess
        task.retry(exc=Exception(errors[0]))


@celery.task(bind=True, default_retry_delay=ONE_HOUR)
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

    details = '\n<br>'.join([err.replace('\n', '<br>') for err in error_details])
    body = u"{} <br><br>Errors: <br><br>{}".format(
        error_message, details.replace('<br>', '\n')
    )

    from taa import errors
    mailer = LookupService('MailerService')
    mailer.send_email(
        to=[e for e in errors.error_recipients],
        from_email=u"TAA Error <{}>".format(taa_app.config.get('EMAIL_FROM_ADDRESS', 'errors@5StarEnroll.com')),
        subject=u"5Star Processing Error ({})".format(taa_app.config['HOSTNAME']),
        html=body
    )


@celery.task(bind=True)
def sync_okta(task, notify_email):

    from taa.manage.sync_agents import sync_agents
    sync_agents()

    mailer = LookupService('MailerService')
    mailer.send_email(
        to=[notify_email],
        from_email=u"eApp Notifications <{}>".format(taa_app.config['EMAIL_FROM_ADDRESS']),
        subject=u"Sync with Okta Complete ({})".format(taa_app.config['HOSTNAME']),
        html="The sync with the Okta user data has finished successfully.",
    )


@celery.task(bind=True, default_retry_delay=FIVE_MINUTES)
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


@celery.task(bind=True, default_retry_delay=FIVE_MINUTES)
def submit_csv_to_dell(task, submission_id):
    """
    Task to submit a csv item to dell for processing
    """

    submission_service = LookupService('EnrollmentSubmissionService')
    submission = submission_service.get_submission_by_id(submission_id)
    log = SubmissionLog()
    log.processing_time = datetime.now()
    log.enrollment_submission_id = submission_id
    log.status = SubmissionLog.STATUS_PROCESSING
    db.session.add(log)
    db.session.commit()
    
    try:
        submitted_filename = submission_service.submit_hi_acc_export_to_dell(submission.data)
        submission.status = EnrollmentSubmission.STATUS_SUCCESS
        log.status = SubmissionLog.STATUS_SUCCESS
        log.message = time.strftime(
            '`{}` successfully transmitted to Dell on %x at %X %Z.'.format(submitted_filename))
        db.session.commit()
    except Exception as ex:
        submission.status = EnrollmentSubmission.STATUS_FAILURE
        log.status = SubmissionLog.STATUS_FAILURE
        log.message = 'Error sending CSV to Dell with message "%s"\n%s' % (ex.message, traceback.format_exc())
        db.session.commit()

        send_admin_error_email("Error submitting HI/ACC CSV file to Dell", [traceback.format_exc()])
        task.retry()


@celery.task(bind=True, default_retry_delay=ONE_HOUR)
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
    log.processing_time = datetime.now()
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
        log.message = json.dumps(result)
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
    
    # This ssl monkey-patch is to avoid certificate verification.
    if hasattr(ssl, '_create_unverified_context'):
        ssl._create_default_https_context = ssl._create_unverified_context
        
    # Use a custom transport for the SOAP library code to force it to use the Proximo proxy, if present.
    proxy = None
    if os.environ.get('PROXIMO_URL', '') != '':
        # Use the pycurl transport because its proxy support works with proximo
        proxy_url = os.environ.get('PROXIMO_URL', '')
        parsed_url = urlparse(proxy_url)
        proxy = dict(proxy_host=parsed_url.hostname, proxy_user=parsed_url.username,
                                  proxy_pass=parsed_url.password, proxy_port=80)
        
        # Tell pysimplesoap to use the pycurl transport
        set_http_wrapper('pycurl')
    
    if not taa_app.config['IS_STP_SIMULATE']:
        client = SoapClient(wsdl=taa_app.config['STP_URL'], proxy=proxy)
        response_dict = client.TXlifeProcessor(xml)
    else:
        response_dict = None
    
    return parse_stp_response(response_dict)
        
        
def parse_stp_response(response_dict):
    '''
    Example response:
    {'TXlifeProcessorResult':
    u'<?xml version="1.0"?><TXLife xmlns="http://ACORD.org/Standards/Life/2"><UserAuthResponse xmlns=""><TransResult><ResultCode tc="1">Success</ResultCode></TransResult></UserAuthResponse><TxLifeResponse xmlns=""><TransRefGUID>f63725d5-3530-4ec7-bb46-81c9b48e1389</TransRefGUID><TransType tc="103">New Business Reqeust</TransType><TransExeDate>2016-08-25</TransExeDate><TransExeTime>15:44:46</TransExeTime><TransMode tc="2">Original</TransMode><TransResult><ResultCode tc="5">Failure - lbweb02-01-mo</ResultCode><ResultInfo><ResultInfoCode tc="99001">Error Loading Company[67].</ResultInfoCode><ResultInfoDesc>Required CompanyNumber or ExtensionCode not found.</ResultInfoDesc></ResultInfo></TransResult></TxLifeResponse></TXLife>'
    }
    '''
    
    if not response_dict:
        return dict(
            status='Not Attempted',
            message='IS_STP_SIMULATE is TRUE, no STP transmission attempted',
            guid=None,
            resp=None)
    
    resp = response_dict['TXlifeProcessorResult']
    
    doc = ElementTree.fromstring(resp)
    
    # Get the GUID
    guid = doc.findall('./TxLifeResponse/TransRefGUID')[0].text
    
    # Get the status
    e = doc.findall('./TxLifeResponse/TransResult/ResultCode')[0]
    message = e.text
    if message.startswith('Failure'):
        return dict(status='Failure', message=message, guid=guid, resp=resp)
    else:
        return dict(status='Success', message=message, guid=guid, resp=resp)


@celery.task(bind=True, default_retry_delay=ONE_HOUR)
def submit_pdf_to_dell_sftp(task, submission_id):
    """
    Task to submit a PDF to Dell for processing using their SFTP dropbox.
    """
    
    submission = db.session.query(EnrollmentSubmission).get(submission_id)
    data = json.loads(submission.data)
    enrollment_id = data['enrollment_id']
    enrollment = LookupService('EnrollmentApplicationService').get(enrollment_id)
    product_id = data['product_id']
    product = LookupService('ProductService').get(product_id)
    pdf_bytes = submission.binary_data
    
    # Create log for this attempt at processing
    log = SubmissionLog()
    log.processing_time = datetime.now()
    log.enrollment_submission_id = submission.id
    log.status = SubmissionLog.STATUS_PROCESSING
    db.session.add(log)
    db.session.commit()
    
    if not pdf_bytes:
        submission.status = EnrollmentSubmission.STATUS_FAILURE
        log.status = SubmissionLog.STATUS_FAILURE
        log.message = "No PDF in data to transmit."
        db.session.commit()
        raise ValueError("Submission ID {}: Attempt to transmit empty PDF to Dell, aborting.".format(submission_id))
    
    try:
        filename = 'case-{}_enrollment-{}-{}.pdf'.format(enrollment.case_id, enrollment_id, product.get_base_product_code())
        transmit_dell_pdf(filename, pdf_bytes)
        
        # Success
        submission.status = EnrollmentSubmission.STATUS_SUCCESS
        log.status = SubmissionLog.STATUS_SUCCESS
        log.message = "Successfully sent {} to Dell at {}".format(filename, datetime.now().strftime("%Y-%m-%d %H:%M:%S"))
        db.session.commit()
    
    except Exception as ex:
        submission.status = EnrollmentSubmission.STATUS_FAILURE
        log.status = SubmissionLog.STATUS_FAILURE
        log.message = 'Error sending PDF to Dell SFTP Dropbox with traceback "%s"\n%s' % (ex.message, traceback.format_exc())
        db.session.commit()
        
        send_admin_error_email(
            "Error transmitting PDF to Dell SFTP Dropbox, submission {} enrollment {} product {}".format(submission_id, enrollment_id, product_id),
            [traceback.format_exc()])
        task.retry()


def transmit_dell_pdf(filename, pdf_bytes):
    sftp_service = LookupService('SFTPService')
    sftp_service.send_file(sftp_service.get_dell_server(), filename, pdf_bytes)

def get_xml_filename(xml_bytes):
    # Extract AFBA form name
    form_app_start = xml_bytes.find('<FormInstance id="Form_App')
    form_name_start = xml_bytes.find('<FormName>', form_app_start)
    form_name_end = xml_bytes.find('</FormName>', form_name_start)
    form_name = xml_bytes[form_name_start+len('<FormName>'):form_name_end]

    # Extract unique ID
    app_id_start = xml_bytes.find('<TransRefGUID>')
    app_id_end = xml_bytes.find('</TransRefGUID>', app_id_start)
    app_id = xml_bytes[app_id_start+len('<TransRefGUID>'):app_id_end]

    return '{}_{}.XML'.format(form_name, app_id)
    

@celery.task(bind=True, default_retry_delay=ONE_HOUR)
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


@celery.task(bind=True, default_retry_delay=ONE_HOUR)
def process_paylogix_csv_generation(task, run_now=False):
    submission_service = LookupService('EnrollmentSubmissionService')
    """:type: taa.services.submissions.EnrollmentSubmissionService"""
    submission = None
    # noinspection PyBroadException
    try:
        submission = submission_service.get_pending_batch_submission(EnrollmentSubmission.TYPE_PAYLOGIX_CSV_GENERATION)
        if not submission:
            return
        export_submission = submission_service.process_paylogix_csv_generation_submission(submission)

        if run_now:
            process_paylogix_export.run(submission_id=export_submission.id)
        else:
            process_paylogix_export.delay(submission_id=export_submission.id)

    except Exception as ex:
        submission_service.set_submissions_status(EnrollmentSubmission.STATUS_FAILURE, [submission], error_message=ex.message)

        send_admin_error_email(
            "Error transmitting CSV to Paylogix",
            [traceback.format_exc()])
        
        task.retry()


# Exports that run in the background
@celery.task()
def export_user_case_enrollments(export_id):
    enrollment_export_service = LookupService('EnrollmentExportService')

    enrollment_export_service.process_export(export_id)


@celery.task(bind=True)
def schedule_case_report(task, email_address):
    from taa.manage.generate_case_report import generate_case_report
    generate_case_report(email_address)
    