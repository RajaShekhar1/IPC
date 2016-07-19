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
        tasks.process_wizard_enrollment(enrollment_application.id)
        # tasks.process_wizard_enrollment.delay(enrollment_application.id)

    def submit_hi_acc_enrollments(self, start_time=None, end_time=None):
        import taa.tasks as tasks
        tasks.process_hi_acc_enrollments.delay(start_time, end_time)

    def process_wizard_submission(self, enrollment_application_id):

        enrollment_application = self.enrollment_application_service.get(enrollment_application_id)
        if not enrollment_application:
            raise ValueError("No enrollment application exists with id {}".format(enrollment_application_id))

        # We still run the code through the docusign submission process, but if it is call center
        #  it will generate all forms internally and sign them automatically.
        return self.submit_to_docusign_or_dell(enrollment_application)

    def _should_submit_to_dell(self, case):
        return case.is_stp

    def submit_to_docusign_or_dell(self, enrollment_record):
        if self._should_submit_to_dell(enrollment_record.case):
            self.submit_to_dell(enrollment_record)
        else:
            self.submit_to_docusign(enrollment_record)

    def submit_to_docusign(self, enrollment_application):
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
        return True

    def submit_to_dell(self, enrollment_application):
        import taa.tasks as tasks
        submission = EnrollmentSubmission()
        submission.submission_type = EnrollmentSubmission.TYPE_DELL_STP_XML
        # determine where to store metadata about submission (`employee_application_coverage` ID == stores applicant type, product, etc.)
        db.session.add(submission)
        db.session.commit()
        submission.enrollment_applications = [enrollment_application]
        # add to enrollment_submission_blah
        # for each enrollment
        tasks.submit_stp_xml_to_dell(submission.id)

    def submit_signed_application(self, enrollment_application):
        return EnrollmentSubmissionProcessor().submit_signed_enrollment(enrollment_application)

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

        # TODO: submit must determine to send to docusign or dell
        # get PDF:
        # api/enrollments.py > binary_pdf = enrollment_submission_service.render_enrollment_pdf(item.enrollment_record)
        EnrollmentSubmissionProcessor().submit_to_docusign(batch_item.enrollment_record)

        self._mark_item_success(batch_item)

    def render_enrollment_pdf(self, enrollment_record, is_stp=False):
        """
        Used for viewing enrollments that are generated and signed without using docusign.
        """

        submission_processor = EnrollmentSubmissionProcessor()
        #components, data_wrap = submission_processor.generate_envelope_components(enrollment_record)
        components = submission_processor.generate_document_components(enrollment_record, is_stp)
        pdfs = [c.generate_pdf_bytes() for c in components]

        writer = PdfFileWriter()
        for pdf in pdfs:
            reader = PdfFileReader(BytesIO(pdf))
            writer.appendPagesFromReader(reader)

        output = BytesIO()
        writer.write(output)
        return output.getvalue()

    def get_enrollees(self, enrollment_record):
        enrollees = ['employee']
        data = json.loads(enrollment_record.standardized_data)
        if ('spouse_coverage' in data and
                data['spouse_coverage'].get('face_value') is not None):
            enrollees.append('spouse')
        for idx in range(len(data.get('children', []))):
            enrollees.append('child{}'.format(idx))
        return enrollees

    def render_enrollment_xml(self, enrollment_record, form_for, pdf_bytes):
        """
        Used for previewing and testing XML files.
        """
        from taa.services.enrollments.xml_export import generate_xml, generate_from_enrollment
        agents = []
        agents.append(enrollment_record.case.owner_agent)
        for agent in enrollment_record.case.partner_agents:
            agents.append(agent)
        # generate_from_enrollment(enrollment_id, census_record_id,
        #                          template='xml/base.xml'):
        # data = json.loads(enrollment_record.standardized_data)
        data = self.enrollment_application_service.get_standardized_json_for_enrollment(enrollment_record)
        # TODO: what are the below lines doing? needed?
        # for idx in range(len(data)):
        #     # if product is to be submitted via STP, generate XML
        #     # move check to a higher level
        #     # pass in product_id
        data[0]['case'] = {
            'company_name': enrollment_record.case.company_name,
            'group_number': enrollment_record.case.group_number,
        }
        xml = generate_xml(data[0], enrollment_record, 'xml/base.xml',
                           form_for, pdf_bytes)
        return xml

    def get_applicant_types(self, enrollment_record):
        data = json.loads(enrollment_record.standardized_data)

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

    # def get_pending_or_failed_csv_submissions(self):
    #     """
    #     Get all submissions that are either pending or failed
    #     """
    #     query = db.session.query(EnrollmentSubmission).filter(EnrollmentSubmission.status.in_(
    #         [EnrollmentSubmission.STATUS_FAILURE, EnrollmentSubmission.STATUS_PENDING]))
    #     return query.all()

    def get_submissions(self, start_date=None, end_date=None):
        """
        Get all submissions which can optionally be filtered by start and end dates
        """
        query = db.session.query(EnrollmentSubmission
             ).options(db.subqueryload(EnrollmentSubmission.enrollment_applications
                                       ).joinedload(EnrollmentApplication.census_record, EnrollmentApplication.case
                                       )
             ).options(db.subqueryload(EnrollmentSubmission.submission_logs)
             ).order_by(EnrollmentSubmission.created_at.desc())

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

        submission = EnrollmentSubmission(submission_type=submission_type, status=status)
        submission.enrollment_applications.extend(applications)
        if commit:
            db.session.add(submission)
            db.session.commit()
        return submission

    def start_submission(self, submission):
        submission.status = EnrollmentSubmission.STATUS_PROCESSING

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
            submission = self.create_submission(EnrollmentSubmission.TYPE_PAYLOGIX_CSV_GENERATION)

        for application in applications:
            standardized_data = enrollments.load_standardized_data_from_application(application)
            if any(EnrollmentDataWrap(d, application.case, application).requires_paylogix_export() for d in
                   standardized_data):
                submission.enrollment_applications.append(application)
        db.session.commit()

        return submission

    def create_paylogix_export_submission(self, csv_submission, data):
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
        filename = '5Star-%s.csv.pgp' % datetime.now().strftime('%Y-%m-%d')
        ftp_service.send_file(DELL_FTP_HOSTNAME, DELL_FTP_USERNAME, DELL_FTP_PASSWORD, filename, csv_data,
                              directory=DELL_FTP_WORKING_DIRECTORY, key_id=DELL_FTP_PGP_KEY_ID)

    def get_submission_by_id(self, submission_id):
        """
        Get an EnrollmentSubmission by ID
        """
        return db.session.query(EnrollmentSubmission).filter(EnrollmentSubmission.id == submission_id).first()

    def create_submission_for_csv(self, csv_data, applications):
        """
        Create a new EnrollmentSubmission for submitting csv data to dell
        """
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

            log = self.start_submission(submission)
            csv = paylogix.create_paylogix_csv(submission.enrollment_applications)
            self.complete_submission(submission, log)

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

            log = self.start_submission(submission)
            submissions.upload_paylogix_file(submission.data)
            self.complete_submission(submission, log)
        except Exception as ex:
            if log:
                self.fail_submission(submission, log, ex.message)
            raise ex

    def create_batch_submission(self, submission_type):
        submission = EnrollmentSubmission(submission_type=submission_type,
                                          status=EnrollmentSubmission.STATUS_PENDING)
        db.session.add(submission)
        db.session.commit()
        return submission


class EnrollmentSubmissionProcessor(object):
    pdf_generator_service = RequiredFeature('ImagedFormGeneratorService')
    docusign_service = RequiredFeature('DocuSignService')
    product_service = RequiredFeature('ProductService')
    enrollment_service = RequiredFeature('EnrollmentApplicationService')

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

    def submit_signed_enrollment(self, enrollment_application):

        #first_product_data = EnrollmentDataWrap(product_submissions[0], case, enrollment_record=enrollment_application)
        #in_person_signer, recipients = self.create_envelope_recipients(case, first_product_data)

        components = self.generate_document_components(enrollment_application)

        # We need to submit certain documents to Dell via DocuSign.
        # TODO
        return True

    def generate_document_components(self, enrollment_application, is_stp=False):
        """Used for generating PDFs from enrollments signed in the wizard, outside of docusign"""

        case = enrollment_application.case

        all_product_data = self.enrollment_service.get_standardized_json_for_enrollment(enrollment_application)

        first_product_data = EnrollmentDataWrap(all_product_data[0], case=enrollment_application.case,
                                       enrollment_record=enrollment_application)
        signing_agent = first_product_data.get_signing_agent()
        recipients = [
            AgentDocuSignRecipient(signing_agent, name=signing_agent.name(),
                                   email=signing_agent.email,
                                   exclude_from_envelope=True),
            EmployeeDocuSignRecipient(name=first_product_data.get_employee_name(),
                                      email=first_product_data.get_employee_email(),
                                      exclude_from_envelope=True),
        ]
        emp_recip = recipients[1]

        components = []

        # TODO: dont include for Dell STP XML
        if not is_stp:
            if case.include_cover_sheet:
                from taa.services.docusign.documents.cover_sheet import CoverSheetAttachment
                components.append(CoverSheetAttachment([emp_recip], EnrollmentDataWrap(all_product_data[0], case,
                                                                                              enrollment_record=enrollment_application),
                                                       all_product_data))

        for raw_enrollment_data in all_product_data:
            # Wrap the submission with an object that knows how to pull out key info.
            enrollment_data = EnrollmentDataWrap(raw_enrollment_data, case, enrollment_record=enrollment_application)

            # Don't use docusign rendering of form if we need to adjust the recipient routing/roles.
            should_use_docusign_renderer = False # if enrollment_data.should_use_call_center_workflow() else True

            product_id = enrollment_data.get_product_id()
            product = self.product_service.get(product_id)
            if not product.does_generate_form():
                continue


            if product.is_fpp():
                components += self.docusign_service.create_fpp_envelope_components(enrollment_data, recipients,
                                                                  should_use_docusign_renderer, show_all_documents=True)
            # TODO: dont include for Dell STP XML
            elif product.is_static_benefit():
                if not is_stp:
                    components += self.docusign_service.create_static_benefit_components(enrollment_data, recipients,
                                                                        should_use_docusign_renderer,
                                                                        enrollment_application, show_all_documents=True)
            # TODO: dont include for Dell STP XML
            elif product.is_group_ci():
                if not is_stp:
                    components += self.docusign_service.create_group_ci_envelope_components(enrollment_data, recipients,
                                                                           should_use_docusign_renderer, show_all_documents=True)

        return components

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
