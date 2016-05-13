from datetime import datetime
from io import BytesIO
import json
import traceback

from PyPDF2 import PdfFileReader, PdfFileWriter
from StringIO import StringIO

import gnupg
import taa.services.enrollments as enrollments

from taa import db
from taa.config_defaults import DOCUSIGN_CC_RECIPIENTS
from taa.services import RequiredFeature
from taa.services.docusign.docusign_envelope import EnrollmentDataWrap
from taa.services.docusign.service import AgentDocuSignRecipient, EmployeeDocuSignRecipient, CarbonCopyRecipient
from taa.services.enrollments.models import EnrollmentImportBatchItem, EnrollmentSubmission, SubmissionLog, \
    EnrollmentApplication
from taa.services import LookupService
from ftplib import FTP
from taa.config_defaults import DELL_FTP_HOSTNAME, DELL_FTP_USERNAME, DELL_FTP_PASSWORD, GNUPG_DIR, DELL_PGP_KEY, \
    DELL_FTP_WORKING_DIRECTORY, DELL_FTP_PGP_KEY_ID


# noinspection PyMethodMayBeStatic
class EnrollmentSubmissionService(object):
    enrollment_application_service = RequiredFeature('EnrollmentApplicationService')
    enrollment_batch_service = RequiredFeature('EnrollmentImportBatchService')
    docusign_service = RequiredFeature('DocuSignService')

    __gpg = None
    """:type: gnupg.GPG"""

    def submit_wizard_enrollment(self, enrollment_application):
        import taa.tasks as tasks
        #if True:
        #    self.process_wizard_submission(enrollment_application.id)
        #else:
        tasks.process_wizard_enrollment.delay(enrollment_application.id)

    def submit_hi_acc_enrollments(self, start_time=None, end_time=None):
        import taa.tasks as tasks
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
        import taa.tasks as tasks
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

    def get_pending_csv_submission(self):
        """
        Get the pending csv generation submission record if it exists
        """
        return db.session.query(EnrollmentSubmission) \
            .filter(EnrollmentSubmission.submission_type == EnrollmentSubmission.TYPE_DELL_CSV_GENERATION) \
            .filter(EnrollmentSubmission.status == EnrollmentSubmission.STATUS_PENDING) \
            .first()

    def get_pending_or_failed_csv_submissions(self):
        """
        Get all submissions that are either pending or failed
        """
        query = db.session.query(EnrollmentSubmission).filter(EnrollmentSubmission.status.in_(
            [EnrollmentSubmission.STATUS_FAILURE, EnrollmentSubmission.STATUS_PENDING]))
        return query.all()

    def get_submissions(self, start_date=None, end_date=None):
        """
        Get all submissions which can optionally be filtered by start and end dates
        """
        query = db.session.query(EnrollmentSubmission).order_by(EnrollmentSubmission.created_at.desc())

        if start_date is not None:
            query = query.filter(EnrollmentSubmission.created_at > start_date)
        if end_date is not None:
            query = query.filter(EnrollmentSubmission.created_at < end_date)

        return query.all()

    def create_docusign_submission_for_application(self, application):
        """
        Create and submit a submission for Docusign
        """
        if 'Group CI' not in [p for p in application.case.products]:
            return None

        submission = EnrollmentSubmission(submission_type=EnrollmentSubmission.TYPE_DOCUSIGN)
        submission.enrollment_applications.append(application)
        db.session.add(submission)
        db.session.commit()
        return submission

    def create_dell_csv_generation_submission_for_application(self, application):
        """
        Create or add an application to the next pending csv generation submission if the product should be in the next
        csv generation batch as determined by the case having either an HI or ACC product on it.
        """
        if not any(p for p in application.case.products if p.requires_dell_csv_submission()):
            return None
        submission = self.get_pending_csv_submission()
        if submission is None:
            # noinspection PyArgumentList
            submission = EnrollmentSubmission(
                submission_type=EnrollmentSubmission.TYPE_DELL_CSV_GENERATION)
            db.session.add(submission)
        submission.enrollment_applications.append(application)
        db.session.commit()
        return submission

    def create_submission(self, submission_type, status=EnrollmentSubmission.STATUS_PENDING, applications=None,
                          commit=True):
        if applications is None:
            applications = list()
        # noinspection PyArgumentList
        submission = EnrollmentSubmission(submission_type=submission_type, status=status)
        submission.enrollment_applications.extend(applications)
        if commit:
            db.session.add(submission)
            db.session.commit()
        return submission

    def start_submission(self, submission):
        submission.status = EnrollmentSubmission.STATUS_PROCESSING
        # noinspection PyArgumentList
        log = SubmissionLog(enrollment_submission=submission, status=EnrollmentSubmission.STATUS_PROCESSING)
        db.session.add(log)
        db.session.commit()
        return log

    def complete_submission(self, submission, log=None, message=None):
        submission.status = EnrollmentSubmission.STATUS_SUCCESS
        if log:
            log.status = EnrollmentSubmission.STATUS_SUCCESS
            if message:
                log.message = message
        db.session.commit()

    def fail_submission(self, submission, log=None, message=None):
        submission.status = EnrollmentSubmission.STATUS_FAILURE
        if log:
            log.status = EnrollmentSubmission.STATUS_FAILURE
            if message:
                log.message = message
        db.session.commit()

    def add_application_to_batch_submission(self, application, submission_type):
        submission = self.get_pending_batch_submission(submission_type)
        if not submission:
            submission = self.create_batch_submission(submission_type)
        submission.enrollment_applications.append(application)
        db.session.commit()

    def create_paylogix_csv_generation_submission(self, applications):
        if isinstance(applications, EnrollmentApplication):
            applications = [applications]
        submission = self.get_pending_batch_submission(EnrollmentSubmission.TYPE_PAYLOGIX_CSV_GENERATION)
        if not submission:
            # noinspection PyArgumentList
            submission = self.create_submission(EnrollmentSubmission.TYPE_PAYLOGIX_CSV_GENERATION)
        for application in applications:
            standardized_data = enrollments.load_standardized_data_from_application(application)
            if any(EnrollmentDataWrap(d, application.case, application).requires_paylogix_export() for d in
                   standardized_data):
                submission.enrollment_applications.append(application)
        db.session.commit()
        return submission

    def create_paylogix_export_submission(self, csv_submission, data):
        # noinspection PyArgumentList
        submission = self.create_submission(EnrollmentSubmission.TYPE_PAYLOGIX_EXPORT,
                                            applications=csv_submission.enrollment_applications, commit=False)
        submission.data = data
        db.session.add(submission)
        db.session.commit()
        return submission

    def create_submissions_for_application(self, application):
        """
        Create submissions for necessary products for the given EnrollmentApplication
        """
        submissions = list()
        """:type: list[EnrollmentSubmission]"""

        # Create or add to pending CSV generation submission. None if its case doesn't contain and HI or ACC products
        submission = self.create_dell_csv_generation_submission_for_application(application)
        if submission is not None:
            submissions.append(submission)
        submission = self.create_static_benefit_submission_for_application(application)
        if submission is not None:
            submissions.append(submission)
        submission = self.create_paylogix_csv_generation_submission(application)
        if submission is not None:
            submissions.append(submission)
        # TODO: Uncomment this when the docusign submission is fully switched over to this
        # Create a docusign submission. None if its case doesn't have Group CI as one of its products
        # submission = self.create_docusign_submission_for_application(application)
        # if submission is not None:
        #     submission.append(submission)

        return submissions

    def __initialize_pgp_key(self, gpg):
        """
        Initialize the GPG instance with out public key
        """
        import_result = gpg.import_keys(DELL_PGP_KEY)
        if len(import_result.results) == 0 or not all(
                (r.get('status').strip() in import_result._ok_reason.values() for r in import_result.results)):
            raise Exception

    def __initialize_gpg(self):
        self.__gpg = gnupg.GPG(binary=GNUPG_DIR)
        self.__initialize_pgp_key(self.__gpg)

    def pgp_encrypt_string(self, data, recipient_id=DELL_FTP_PGP_KEY_ID):
        """
        Encrypt the given data with PGP and return the encrypted result
        """

        if self.__gpg is None:
            self.__initialize_gpg()
        try:
            return self.__gpg.encrypt(data, recipient_id)
        except Exception as exception:
            return None

    def submit_hi_acc_export_to_dell(self, csv_data):
        """
        Submit csv data to dell for processing
        """
        ftp_service = LookupService('FtpService')
        filename = 'enrollment_submissions_%s.csv.pgp' % datetime.now().strftime('%Y-%m-%dT%H:%M:%S')
        ftp_service.send_file(DELL_FTP_HOSTNAME, DELL_FTP_USERNAME, DELL_FTP_PASSWORD, filename, csv_data,
                              key_id=DELL_FTP_PGP_KEY_ID)

    def get_submission_by_id(self, submission_id):
        """
        Get an EnrollmentSubmission by ID
        """
        return db.session.query(EnrollmentSubmission).filter(EnrollmentSubmission.id == submission_id).first()

    def create_submission_for_csv(self, csv_data, applications):
        """
        Create a new EnrollmentSubmission for submitting csv data to dell
        """
        # noinspection PyArgumentList
        submission = EnrollmentSubmission(data=csv_data,
                                          submission_type=EnrollmentSubmission.TYPE_DELL_EXPORT)
        for application in applications:
            submission.enrollment_applications.append(application)
        db.session.add(submission)
        db.session.commit()
        return submission

    def set_submissions_status(self, status, submissions=None, submission_logs=None, error_message=None):
        """
        Set the status
        """
        submissions = submissions if submissions is not None else list()
        submission_logs = submission_logs if submission_logs is not None else list()

        for submission in submissions:
            submission.status = status
        for log in submission_logs:
            log.status = status
            if error_message:
                log.message = error_message
        db.session.commit()

    def get_applications_for_submissions(self, submissions):
        """
        Get a list of unique applications in the given submissions
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

    def create_static_benefit_submission_for_application(self, application):
        if not any(p for p in application.case.products if p.is_static_benefit()):
            return None
        # noinspection PyArgumentList
        submission = EnrollmentSubmission(submission_type=EnrollmentSubmission.TYPE_STATIC_BENEFIT,
                                          status=EnrollmentSubmission.STATUS_SUCCESS)
        submission.enrollment_applications.append(application)
        db.session.add(submission)
        db.session.commit()
        return submission

    def get_pending_batch_submission(self, submission_type):
        return db.session.query(EnrollmentSubmission) \
            .filter(EnrollmentSubmission.status == EnrollmentSubmission.STATUS_PENDING) \
            .filter(EnrollmentSubmission.submission_type == submission_type) \
            .first()

    def has_pending_batch_submission(self, submission_type):
        return db.session.query(EnrollmentSubmission) \
                   .filter(EnrollmentSubmission.status == EnrollmentSubmission.STATUS_PENDING and
                           EnrollmentSubmission.submission_type == submission_type) \
                   .count() > 0

    def process_paylogix_csv_generation_submission(self, submission):
        log = None
        try:
            import taa.services.enrollments.paylogix as paylogix
            # noinspection PyArgumentList
            log = self.start_submission(submission)
            csv = paylogix.create_paylogix_csv(submission.enrollment_applications)
            self.complete_submission(submission, log)
            # noinspection PyArgumentList
            return self.create_paylogix_export_submission(submission, csv)
        except Exception as ex:
            if log:
                self.fail_submission(submission, log, ex.message)
            raise ex

    def process_paylogix_export(self, submission):
        log = None
        try:
            import taa.services.enrollments.paylogix as paylogix
            import taa.services.submissions as submissions
            # noinspection PyArgumentList
            log = self.start_submission(submission)
            submissions.upload_paylogix_file(submission.data)
            self.complete_submission(submission, log)
        except Exception as ex:
            if log:
                self.fail_submission(submission, log, ex.message)
            raise ex

    def create_batch_submission(self, submission_type):
        submission = EnrollmentSubmission(submission_type=submission_type, status=EnrollmentSubmission.STATUS_PENDING)
        db.session.add(submission)
        db.session.commit()
        return submission


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
