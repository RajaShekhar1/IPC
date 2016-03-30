from datetime import datetime
from io import BytesIO
import json
import traceback

from PyPDF2 import PdfFileReader, PdfFileWriter
from StringIO import StringIO

import gnupg

import taa.tasks as tasks
from taa import db
from taa.config_defaults import DOCUSIGN_CC_RECIPIENTS
from taa.services import RequiredFeature
from taa.services.docusign.docusign_envelope import EnrollmentDataWrap
from taa.services.docusign.service import AgentDocuSignRecipient, EmployeeDocuSignRecipient, CarbonCopyRecipient
from taa.services.enrollments.models import EnrollmentImportBatchItem, EnrollmentSubmission, SubmissionLog, \
    EnrollmentApplication
from ftplib import FTP
from taa.config_defaults import DELL_FTP_HOSTNAME, DELL_FTP_USERNAME, DELL_FTP_PASSWORD, GNUPG_DIR, DELL_FTP_PGP_KEY, \
    DELL_FTP_WORKING_DIRECTORY




# noinspection PyMethodMayBeStatic
class EnrollmentSubmissionService(object):
    enrollment_application_service = RequiredFeature('EnrollmentApplicationService')
    enrollment_batch_service = RequiredFeature('EnrollmentImportBatchService')
    docusign_service = RequiredFeature('DocuSignService')

    __gpg = None
    """:type: gnupg.GPG"""

    def submit_wizard_enrollment(self, enrollment_application):
        tasks.process_wizard_enrollment.delay(enrollment_application.id)

    def submit_hi_acc_enrollments(self, start_time=None, end_time=None):
        tasks.process_hi_acc_enrollments.delay(start_time, end_time)

    def process_wizard_submission(self, enrollment_application_id):

        enrollment_application = self.enrollment_application_service.get(enrollment_application_id)
        if not enrollment_application:
            raise ValueError("No enrollment application exists with id {}".format(enrollment_application_id))

        envelope = self.docusign_service.get_existing_envelope(enrollment_application)
        if not envelope:
            # Create the envelope
            standardized_data = json.loads(enrollment_application.standardized_data)
            in_person_signer, envelope = self.docusign_service.create_multiproduct_envelope(standardized_data,
                                                                                            enrollment_application.case,
                                                                                            enrollment_application)

            # Save envelope ID on enrollment
            self.enrollment_application_service.save_docusign_envelope(enrollment_application, envelope)

        db.session.commit()
        return envelope

    def submit_import_enrollments(self, enrollment_batch):

        # Schedule a task to process this enrollment record
        tasks.process_enrollment_upload.delay(enrollment_batch.id)

    def process_import_submission_batch(self, enrollment_batch_id):
        """
        Process a whole enrollment upload file. Each record should be checked
         to see if it was successfully imported already.
        """

        enrollment_batch = self.enrollment_batch_service.get(enrollment_batch_id)
        if not enrollment_batch:
            raise ValueError("No enrollment import batch exists with id {}".format(enrollment_batch_id))

        errors = []
        for batch_item in self.enrollment_batch_service.get_records_needing_submission(enrollment_batch):
            is_successful, err = self.process_and_update_status(batch_item)
            if not is_successful:
                errors.append(err)

        return errors

    def process_and_update_status(self, batch_item):
        try:
            self.process_import_submission(batch_item)
            return True, ""
        except Exception as exc:
            self._mark_item_error(batch_item, exc)
            return False, batch_item.error_message

    def process_import_submission(self, batch_item):
        """
        Submit the enrollment and mark as complete
        """
        self._mark_item_processing(batch_item)

        EnrollmentSubmissionProcessor().submit_to_docusign(batch_item.enrollment_record)

        self._mark_item_success(batch_item)

    def render_enrollment_pdf(self, enrollment_record):
        """
        Used for previewing and testing.
        """
        submission_processor = EnrollmentSubmissionProcessor()
        components, data_wrap = submission_processor.generate_envelope_components(enrollment_record)
        pdfs = [c.generate_pdf_bytes() for c in components]

        writer = PdfFileWriter()
        for pdf in pdfs:
            reader = PdfFileReader(BytesIO(pdf))
            writer.appendPagesFromReader(reader)

        output = BytesIO()
        writer.write(output)
        return output.getvalue()

    def _mark_item_processing(self, batch_item):
        batch_item.processed_time = datetime.now()
        batch_item.status = EnrollmentImportBatchItem.STATUS_PROCESSING
        db.session.commit()

    def _mark_item_success(self, batch_item):
        batch_item.processed_time = datetime.now()
        batch_item.status = EnrollmentImportBatchItem.STATUS_SUCCESS
        db.session.commit()

    def _mark_item_error(self, batch_item, exc):
        batch_item.status = EnrollmentImportBatchItem.STATUS_ERROR
        batch_item.error_message = traceback.format_exc()
        batch_item.processed_time = datetime.now()
        db.session.commit()

    def get_pending_submissions(self, product_id=None):
        """
        Get all submissions that are either pending or failed
        :param product_id: Optional ID of the product to get submissions for
        :type product_id: int
        :return: All pending or failed submissions
        :rtype: list[EnrollmentSubmission]
        """
        query = db.session.query(EnrollmentSubmission).filter(EnrollmentSubmission.status.in_(
            [EnrollmentSubmission.STATUS_FAILURE, EnrollmentSubmission.STATUS_PENDING]))
        if product_id is not None:
            query.filter(EnrollmentSubmission.product_id == product_id)
        return query.all()

    def get_enrollment_applications_between(self, start_date=None, end_date=None):
        """
        Get the enrollments between the specified dates.
        If None the date will not filter by the start or end
        :param start_date: Start date
        :type start_date: datetime.datetime
        :param end_date: End date
        :type end_date: datetime.datetime
        """
        query = db.session.query(EnrollmentApplication).join(EnrollmentSubmission)

        if start_date is not None:
            query.filter(EnrollmentSubmission.created_at > start_date)
        if end_date is not None:
            query.filter(EnrollmentSubmission.created_at < end_date)

        return query.all()

    def create_submissions_for_enrollment_application(self, enrollment_application):
        """
        Create submissions for necessary products for the given EnrollmentApplication
        :param enrollment_application: EnrollmentApplication to create submissions for
        :type enrollment_application: EnrollmentApplication
        :rtype: list[EnrollmentSubmission]
        """
        submissions = list()
        """:type: list[EnrollmentSubmission]"""
        for product in [p for p in enrollment_application.case.products if p.get_base_product() in ['HI', 'ACC']]:
            submission = db.session.query(EnrollmentSubmission) \
                .filter(EnrollmentSubmission.product_id == product.id) \
                .filter(EnrollmentSubmission.enrollment_application_id == enrollment_application.id) \
                .first()
            if submission is not None:
                continue
            # noinspection PyArgumentList
            submission = EnrollmentSubmission(product_id=product.id)
            submission.enrollment_applications.append(enrollment_application)
            submission.submission_type = EnrollmentSubmission.TYPE_GENERATE_CSV
            submissions.append(submission)
            db.session.add(submission)
        db.session.commit()
        return submissions

    def __initialize_pgp_key(self, gpg):
        """
        Initialize the GPG instance with out public key
        :param gpg: GPG to load the key into
        :type gpg: gnupg.GPG
        """
        result = gpg.import_keys(DELL_FTP_PGP_KEY)
        if not result.ok:
            raise Exception

    def __initialize_gpg(self):
        gpg = gnupg.GPG(binary=GNUPG_DIR)
        self.__initialize_pgp_key(gpg)

    def pgp_encrypt_string(self, data, recipient_email):
        """
        Encrypt the given data with PGP and return the encrypted result
        :param data: Data to encrypt
        :type data: str
        :type recipient_email: Email to sign the key with
        :type recipient_email: str
        :rtype: str
        """
        # noinspection PyBroadException
        try:
            if self.__gpg is None:
                self.__initialize_gpg()
            key = None
            for k in self.__gpg.list_keys():
                for sig in k['sigs'].keys():
                    if recipient_email in sig:
                        key = k
            if key is None:
                return None
            return self.__gpg.encrypt(data, key['keyid'])
        except Exception:
            return None

    def submit_hi_acc_export_to_dell(self, csv_data):
        """
        Submit csv data to dell for processing
        :param csv_data: String containing the CSV file's data
        :type csv_data: str
        """
        ftp = FTP(DELL_FTP_HOSTNAME)
        ftp.login(DELL_FTP_USERNAME, DELL_FTP_PASSWORD)
        string_buffer = StringIO(csv_data)
        # TODO: Change the name of the file to something more appropriate
        ftp.storlines('STOR name.csv', string_buffer)
        ftp.close()

    def get_submission_by_id(self, submission_id):
        """
        Get an EnrollmentSubmission by ID
        :param submission_id: ID of the EnrollmentSubmission
        :type submission_id: int
        :rtype: EnrollmentSubmission
        """
        return db.session.query(EnrollmentSubmission).filter(EnrollmentSubmission.id == submission_id).first()

    def create_submission_for_csv(self, csv_data, applications):
        """
        Create a new EnrollmentSubmission for submitting csv data to dell
        :param csv_data: String containing the CSV's data
        :type csv_data: str
        :param applications: Enrollment applications that the new submission should be related to
        :type applications: list[EnrollmentApplication]
        :rtype: EnrollmentSubmission
        """
        # noinspection PyArgumentList
        submission = EnrollmentSubmission(data=csv_data)
        for application in applications:
            submission.enrollment_applications.append(application)
        db.session.add(submission)
        db.session.commit()
        return submission

    def set_submissions_status(self, status, submissions=None, submission_logs=None):
        """
        Set the status
        :param status: Status to set for the submissions and logs
        :type status: str
        :param submissions: Submissions to set the status for
        :type submissions: list[EnrollmentSubmission]
        :param submission_logs: Logs to set the status for
        :type submission_logs: list[SubmissionLog]
        """
        submissions = submissions if submissions is not None else list()
        submission_logs = submission_logs if submission_logs is not None else list()

        for submission in submissions:
            submission.status = status
        for log in submission_logs:
            log.status = status
        db.session.commit()

    def get_applications_for_submissions(self, submissions):
        """
        Get a list of unique applications in the given submissions
        :param submissions: EnrollmentSubmissions to grab applications from
        :type submissions: list[EnrollmentSubmission]
        :rtype: list[EnrollmentApplication]
        """
        applications = list()
        for submission in submissions:
            for application in submission.enrollment_applications:
                if application not in applications:
                    applications.append(application)
        return applications

    def create_logs_for_submissions(self, submissions, status=None):
        """
        Create a log entry for each
        :param submissions: Submissions to create logs for
        :type submissions: list[EnrollmentSubmission]
        :param status: Optional status to set the new SubmissionLogs to
        :type status: str
        :rtype: list[SubmissionLog]
        """
        submission_logs = list()
        for submission in submissions:
            # noinspection PyArgumentList
            submission_log = SubmissionLog(enrollment_submission_id=submission.id)
            if status is not None:
                submission_log.status = status
            db.session.add(submission_log)
            submission_logs.append(submission_log)
        db.session.commit()
        return submission_logs


class EnrollmentSubmissionProcessor(object):
    pdf_generator_service = RequiredFeature('ImagedFormGeneratorService')
    docusign_service = RequiredFeature('DocuSignService')

    def submit_to_docusign(self, enrollment_record):

        components, data_wrap = self.generate_envelope_components(enrollment_record)

        # Generate envelope
        envelope = self.docusign_service.create_envelope(
            email_subject=u"Enrollment imported: {} for {} ({})".format(
                data_wrap.get_product_code(),
                data_wrap.get_employee_name(),
                data_wrap.get_employer_name()),
            components=components
        )

        # Save the envelope ID on the enrollment record
        enrollment_record.docusign_envelope_id = envelope.uri

    def generate_envelope_components(self, enrollment_record):
        data_wrap = EnrollmentDataWrap(json.loads(enrollment_record.standardized_data), case=enrollment_record.case,
                                       enrollment_record=enrollment_record)
        recipients = self._create_import_recipients(enrollment_record.case, data_wrap)

        product = data_wrap.get_product()

        # Add back in for HI/ACC
        # if not product.does_generate_form():
        #    return [], data_wrap

        if product.is_fpp():
            components = self.docusign_service.create_fpp_envelope_components(
                data_wrap,
                recipients,
                should_use_docusign_renderer=False,
            )
        else:
            components = self.docusign_service.create_group_ci_envelope_components(
                data_wrap,
                recipients,
                should_use_docusign_renderer=False,
            )
        return components, data_wrap

    def _create_import_recipients(self, case, enrollment_data):

        # Exclude both from the envelope, use them only for tab generation purposes
        signing_agent = enrollment_data.get_signing_agent()
        recipients = [
            AgentDocuSignRecipient(signing_agent, name=signing_agent.name(),
                                   email=signing_agent.email,
                                   exclude_from_envelope=True),
            EmployeeDocuSignRecipient(name=enrollment_data.get_employee_name(),
                                      email=enrollment_data.get_employee_email(),
                                      exclude_from_envelope=True),
        ]
        recipients += self._get_carbon_copy_recipients()
        return recipients

    def _get_carbon_copy_recipients(self):
        return [
            CarbonCopyRecipient(name, email)
            for name, email in DOCUSIGN_CC_RECIPIENTS
            ]
