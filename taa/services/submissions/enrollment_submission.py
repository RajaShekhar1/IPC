from datetime import datetime, time
from io import BytesIO
import json
import traceback
from zipfile import ZipFile

from PyPDF2 import PdfFileReader, PdfFileWriter

from taa import db, tasks
from taa.config_defaults import DOCUSIGN_CC_RECIPIENTS
from taa.services import RequiredFeature
from taa.services.docusign.docusign_envelope import EnrollmentDataWrap
from taa.services.docusign.service import AgentDocuSignRecipient, EmployeeDocuSignRecipient, CarbonCopyRecipient
from taa.services.enrollments.models import EnrollmentImportBatchItem, EnrollmentSubmission, SubmissionLog, \
    EnrollmentApplication
from taa.services import LookupService


class EnrollmentSubmissionService(object):
    enrollment_application_service = RequiredFeature('EnrollmentApplicationService')
    enrollment_batch_service = RequiredFeature('EnrollmentImportBatchService')
    docusign_service = RequiredFeature('DocuSignService')

    def submit_wizard_enrollment(self, enrollment_application):
        # if True:
        #  self.process_wizard_submission(enrollment_application.id)
        # else:
        tasks.process_wizard_enrollment.delay(enrollment_application.id)

    def submit_hi_acc_enrollments(self, start_time=None, end_time=None):
        tasks.process_hi_acc_enrollments.delay(start_time, end_time)

    def process_wizard_submission(self, enrollment_application_id):

        enrollment_application = self.enrollment_application_service.get(enrollment_application_id)
        if not enrollment_application:
            raise ValueError("No enrollment application exists with id {}".format(enrollment_application_id))
        if enrollment_application.is_preview:
            # Preview mode only; don't submit the application
            return
        
        # If this enrollment has not finished the signing ceremony, we cannot submit it yet.
        if enrollment_application.is_pending():
            return
        
        # Generate all the submissions that are not queued up in batches.
        self.create_all_submissions(enrollment_application)

    def _should_submit_to_dell(self, case):
        return case.is_stp

    def search_submissions(self, start_date=None, end_date=None, submission_type=None, submission_status=None):
        q = db.session.query(EnrollmentSubmission)
        
        if start_date:
            q = q.filter(EnrollmentSubmission.created_at >= start_date)
        if end_date:
            q = q.filter(EnrollmentSubmission.created_at <= end_date)
        
        if submission_type:
            q = q.filter(EnrollmentSubmission.submission_type == submission_type)
        
        if submission_status:
            q = q.filter(EnrollmentSubmission.status == submission_status)
        
        q = q.order_by(db.desc(EnrollmentSubmission.created_at))
        
        return q
    
    def get_submission(self, submission_id):
        submission = db.session.query(EnrollmentSubmission).get(submission_id)
        if not submission:
            from flask import abort
            abort(404)
        
        return submission
            
    
    def get_submission_applications(self, submission_id):
        submission = self.get_submission(submission_id)
        return submission.enrollment_applications

    def get_submission_logs(self, submission_id):
        submission = self.get_submission(submission_id)
        return submission.submission_logs
    
    def get_submission_data(self, submission_id):
        submission = self.get_submission(submission_id)
        if submission.data:
            return submission.data
        elif submission.binary_data:
            return submission.binary_data
        else:
            return None
            
    
    def create_all_submissions(self, enrollment_record):
        "Create a submission records and immediately queue up for submission."
        
        # Submit any STP-Enabled products
        self.submit_STP_to_dell(enrollment_record)
        
        # Submit any SFTP transmissions to Dell
        self.submit_to_dell_SFTP_inbox(enrollment_record)
        


    # def submit_to_docusign(self, enrollment_application):
    #     envelope = self.docusign_service.get_existing_envelope(enrollment_application)
    #     if not envelope:
    #         # Create the envelope
    #         standardized_data = json.loads(enrollment_application.standardized_data)
    #         in_person_signer, envelope = self.docusign_service.create_multiproduct_envelope(standardized_data,
    #                                                                                         enrollment_application.case,
    #                                                                                         enrollment_application)
    #
    #         # Save envelope ID on enrollment
    #         self.enrollment_application_service.save_docusign_envelope(enrollment_application, envelope)
    #
    #     db.session.commit()
    #     return True

    def submit_STP_to_dell(self, enrollment_application):
        """
        Submit XML to Dell via their STP endpoint for all products that support it.
        """

        # Bail out early if this case does not have STP enabled.
        if not enrollment_application.case.is_stp:
            return
        
        # Generate all the XML documents, one for each covered applicant.
        xmls = self.generate_enrollment_xml_docs(enrollment_application)
        
        # Create the submissions and schedule them for delivery.
        for xml, applicant_type, coverage, pdf_bytes in xmls:
            # Create a submission to track the status of this XML submission, include the XML in the data for the submission.
            submission = EnrollmentSubmission(
                created_at=datetime.now(),
                submission_type=EnrollmentSubmission.TYPE_DELL_STP_XML,
                data=json.dumps(dict(xml=xml, applicant_type=applicant_type, coverage_id=coverage.id)),
            )
            submission.enrollment_applications = [enrollment_application]
            db.session.add(submission)
            db.session.commit()
            
            # Schedule a task to transmit this submission to Dell
            tasks.submit_stp_xml_to_dell.delay(submission.id)

    def generate_enrollment_xml_docs(self, enrollment_application):
        # returns a list of (xml, applicant_type, coverage) tuples
        xmls = []
        
        # Track if we have generated children for each product. (product_id: bool)
        #  this is necessary because we want to directly iterate through the children here, but we have
        #  a separate coverage record for each child now, so we don't want N*N submissions either.
        has_generated_children_for_product = {}
        
        for coverage in enrollment_application.coverages:
        
            # Filter out products that don't have STP submission by skipping them.
            if not coverage.product.can_submit_stp():
                continue
        
            # Render the PDF for this product's coverage to include in the XML output.
            pdf_bytes = self.render_enrollment_pdf(coverage.enrollment, is_stp=True, product_id=coverage.product_id)
        
            # If this is coverage for children, add an XML doc for each child.
            if coverage.applicant_type == 'children' and not has_generated_children_for_product.get(coverage.product_id):
                has_generated_children_for_product[coverage.product_id] = True
                
                # Generate one for each child
                data = self.enrollment_application_service.get_wrapped_data_for_coverage(coverage)
                for i, child in enumerate(data['children']):
                    # print("Generating child {}".format(i + 1))
                    applicant_type = "child{}".format(i)
                    xml = self.render_enrollment_xml(coverage, applicant_type, pdf_bytes=pdf_bytes)
                    if xml:
                        xmls.append((xml, applicant_type, coverage, pdf_bytes))
            elif coverage.applicant_type != 'children':
                # Otherwise, we add the Employee or Spouse XML doc if they chose coverage.
                applicant_type = coverage.applicant_type
                xml = EnrollmentSubmissionService().render_enrollment_xml(coverage, applicant_type, pdf_bytes=pdf_bytes)
                if xml:
                    xmls.append((xml, applicant_type, coverage, pdf_bytes))
    
        return xmls

    def create_xml_zip(self, xmls, include_pdfs=True):
        zipstream = BytesIO()
        
        included_pdfs = set()
        
        with ZipFile(zipstream, 'w') as zip:
            for xml, applicant_type, coverage, pdf_bytes in xmls:
                
                if pdf_bytes not in included_pdfs:
                    included_pdfs.add(pdf_bytes)
                    
                fn = 'case_{}_enrollment_{}_{}_{}.xml'.format(coverage.enrollment.case.id,
                                                              coverage.enrollment.id,
                                                              coverage.product.get_base_product_code(),
                                                              applicant_type)
                zip.writestr(fn, xml.encode('latin-1'))

        zipstream.seek(0)
        return zipstream

    def submit_to_dell_SFTP_inbox(self, enrollment_application):
        
        # Find each product that used to go to docusign and submit it to the Dell SFTP dropbox.
        
        product_data_list = self.enrollment_application_service.get_wrapped_enrollment_data(enrollment_application)
        for product_data in product_data_list:
    
            product = product_data.get_product()
            
            # Skip if this product is handled by STP and STP is on for the case
            if enrollment_application.case.is_stp and product.can_submit_stp():
                continue
                
            # Skip if declined
            if product_data.did_decline():
                continue
            
            # Skip if this product doesn't generate a PDF for submission.
            if not product.does_generate_form():
                continue
                
            # TODO: combine these conditions into new method, does_submit_pdf_to_dell() ?
            if product.is_static_benefit():
                continue
            
            # Otherwise, generate the PDF and create a submission and queue it up.
            pdf_bytes = self.render_enrollment_pdf(enrollment_application, is_stp=False, product_id=product.id)
            submission = self.create_dell_sftp_submission(enrollment_application, product, pdf_bytes)
            tasks.submit_pdf_to_dell_sftp.delay(submission.id)
            
    def create_dell_sftp_submission(self, enrollment_application, product, pdf_bytes):
        
        submission = EnrollmentSubmission(
            submission_type=EnrollmentSubmission.TYPE_DELL_PDF_SFTP,
            product_id=product.id,
            status=EnrollmentSubmission.STATUS_PENDING,
            created_at=datetime.now(),
            data=json.dumps(dict(
                enrollment_id=enrollment_application.id,
                product_id=product.id,
            )),
            binary_data=pdf_bytes
        )
        submission.enrollment_applications.append(enrollment_application)
        db.session.add(submission)
        db.session.commit()
        return submission

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

        # Generate all the submissions that are not queued up in batches.
        self.create_all_submissions(batch_item.enrollment_record)
        
        # Generate any submissions that are batched.
        self.create_submissions_for_application(batch_item.enrollment_record)
        
        # Success
        self._mark_item_success(batch_item)

    def render_enrollment_pdf(self, enrollment_record, is_stp=False, product_id=None, force_show_all_docs=False):
        """
        Used for viewing enrollments that are generated and signed without using docusign.
        """
        
        submission_processor = EnrollmentSubmissionProcessor()
        # components, data_wrap = submission_processor.generate_envelope_components(enrollment_record)
        components = submission_processor.generate_document_components(enrollment_record, is_stp, only_product_id=product_id, force_show_all_docs=force_show_all_docs)
        pdfs = [c.generate_pdf_bytes() for c in components]

        writer = PdfFileWriter()
        for pdf in pdfs:
            reader = PdfFileReader(BytesIO(pdf))
            writer.appendPagesFromReader(reader)

        output = BytesIO()
        writer.write(output)
        return output.getvalue()

    def get_summary_pdf(self, enrollment_application):
        """
        gets pdf from enrollment submission
        """
        import base64
        submission_processor = EnrollmentSubmissionProcessor()
        pdf_data = submission_processor.generate_cover_sheet(enrollment_application).generate_pdf_bytes()
        data = base64.standard_b64encode(pdf_data)

        return data

    def get_enrollees(self, enrollment_record):
        enrollees = ['employee']
        data = json.loads(enrollment_record.standardized_data)
        if ('spouse_coverage' in data and
                data['spouse_coverage'].get('face_value') is not None):
            enrollees.append('spouse')
        for idx in range(len(data.get('children', []))):
            enrollees.append('child{}'.format(idx))
        return enrollees

    def render_enrollment_xml(self, enrollment_coverage, applicant_type, pdf_bytes):
        """
        Used for previewing and testing XML files.
        """
        enrollment_record = enrollment_coverage.enrollment

        from taa.services.enrollments.xml_export import generate_xml

        data = self.enrollment_application_service.get_wrapped_data_for_coverage(enrollment_coverage)
        # TODO: what are the below lines doing? needed?
        data['case'] = {
            'company_name': enrollment_record.case.company_name,
            'group_number': enrollment_record.case.group_number,
        }
        xml = generate_xml(data, enrollment_record, 'xml/base.xml',
                           applicant_type, pdf_bytes)
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
                                                           ).joinedload(EnrollmentApplication.census_record,
                                                                        EnrollmentApplication.case
                                                                        )
                                           ).options(db.subqueryload(EnrollmentSubmission.submission_logs)
                                                     ).order_by(EnrollmentSubmission.created_at.desc())

        if start_date is not None:
            query = query.filter(EnrollmentSubmission.created_at > start_date)
        if end_date is not None:
            query = query.filter(EnrollmentSubmission.created_at < end_date)

        return query.all()

    def create_dell_csv_generation_submission_for_application(self, application):
        """
        Create or add an application to the next pending csv generation submission if the product should be in the next
        csv generation batch as determined by the applicant having applied for either an HI or ACC products.
        """
        
        data = LookupService('EnrollmentApplicationService').get_wrapped_enrollment_data(application)
        enrolled_products = [d.get_product() for d in data if not d.did_decline()]
        
        if not any(p for p in enrolled_products if p.requires_dell_csv_submission()):
            return None
        
        submission = self.get_pending_csv_submission()
        if submission is None:
            submission = EnrollmentSubmission(
                created_at=datetime.now(),
                submission_type=EnrollmentSubmission.TYPE_DELL_CSV_GENERATION)
            db.session.add(submission)
        submission.enrollment_applications.append(application)
        db.session.commit()
        return submission

    def create_submission(self, submission_type, status=EnrollmentSubmission.STATUS_PENDING, applications=None,
                          commit=True):
        if applications is None:
            applications = list()

        submission = EnrollmentSubmission(submission_type=submission_type, status=status, created_at=datetime.now())
        submission.enrollment_applications.extend(applications)
        if commit:
            db.session.add(submission)
            db.session.commit()
        return submission

    def start_submission(self, submission):
        submission.status = EnrollmentSubmission.STATUS_PROCESSING

        log = SubmissionLog(enrollment_submission=submission, status=EnrollmentSubmission.STATUS_PROCESSING,
                            processing_time=datetime.now(),
                            )
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
            wrapped_data = self.enrollment_application_service.get_wrapped_enrollment_data(application)
            #standardized_data = enrollments.load_standardized_data_from_application(application)
            if any(d.requires_paylogix_export() and not d.did_decline() for d in wrapped_data):
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
        Create batched submissions for necessary products for the given EnrollmentApplication
        """
        submissions = list()
        """:type: list[EnrollmentSubmission]"""

        # Create or add to pending CSV generation submission. None if there are no HI or ACC products enrolled.
        submission = self.create_dell_csv_generation_submission_for_application(application)
        if submission is not None:
            submissions.append(submission)
        
        submission = self.create_paylogix_csv_generation_submission(application)
        if submission is not None:
            submissions.append(submission)

        return submissions

    def submit_hi_acc_export_to_dell(self, csv_data):
        """
        Submit csv data to dell for processing
        """
        sftp_service = LookupService('SFTPService')
        filename = '5Star-HIACC-%s.csv.pgp' % datetime.now().strftime('%Y-%m-%d')
        sftp_service.send_file(sftp_service.get_dell_server(), filename, csv_data)
        
        return filename

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
                                          submission_type=EnrollmentSubmission.TYPE_DELL_EXPORT,
                                          created_at=datetime.now(),
                                          )
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
            submission_log = SubmissionLog(enrollment_submission_id=submission.id, processing_time=datetime.now())
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
                                          status=EnrollmentSubmission.STATUS_SUCCESS,
                                          created_at=datetime.now(),
                                          )
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
            csv = paylogix.create_paylogix_csv('2017-08-01', '2099-01-01', False)
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
                                          status=EnrollmentSubmission.STATUS_PENDING,
                                          created_at=datetime.now(),
                                          )
        db.session.add(submission)
        db.session.commit()
        return submission


class EnrollmentSubmissionProcessor(object):
    pdf_generator_service = RequiredFeature('ImagedFormGeneratorService')
    docusign_service = RequiredFeature('DocuSignService')
    product_service = RequiredFeature('ProductService')
    enrollment_service = RequiredFeature('EnrollmentApplicationService')
    enrollment_coverage_service = RequiredFeature('EnrollmentApplicationCoverageService')

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

    def generate_cover_sheet(self, enrollment_application):
        """Used for generating cover sheet PDF"""
        from taa.services.docusign.documents.cover_sheet import CoverSheetAttachment

        case = enrollment_application.case

        all_product_data = self.enrollment_service.get_standardized_json_for_enrollment(enrollment_application)

        first_product_data = EnrollmentDataWrap(all_product_data[0], case=enrollment_application.case,
                                                enrollment_record=enrollment_application)
        signing_agent = first_product_data.get_signing_agent()
        emp_recip = EmployeeDocuSignRecipient(name=first_product_data.get_employee_name(),
                                              email=first_product_data.get_employee_email(),
                                              exclude_from_envelope=True)

        return CoverSheetAttachment([emp_recip], EnrollmentDataWrap(all_product_data[0], case,
                                                                    enrollment_record=enrollment_application),
                                    all_product_data, enrollment_application=enrollment_application)

    def generate_document_components(self, enrollment_application, is_stp=False, only_product_id=None, force_show_all_docs=False):
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

        # don't include for Dell STP XML
        if not is_stp and case.include_cover_sheet and not only_product_id:
            from taa.services.docusign.documents.cover_sheet import CoverSheetAttachment
            components.append(CoverSheetAttachment([emp_recip], EnrollmentDataWrap(all_product_data[0], case,
                                                                               enrollment_record=enrollment_application,
                                                                               ),
                                               all_product_data, enrollment_application=enrollment_application))

        for raw_enrollment_data in all_product_data:
            # Wrap the submission with an object that knows how to pull out key info.
            enrollment_data = EnrollmentDataWrap(raw_enrollment_data, case, enrollment_record=enrollment_application)

            # Don't use docusign rendering of form if we need to adjust the recipient routing/roles.
            should_use_docusign_renderer = False

            if enrollment_data['did_decline']:
                continue
            
            product = self.product_service.get(enrollment_data.get_product_id())
            if not product.does_generate_form():
                continue

            # If we only want to generate the document for a particular product, skip all other products.
            if only_product_id and product.id != only_product_id:
                continue
                
            if product.is_fpp():
                components += self.docusign_service.create_fpp_envelope_components(enrollment_data, recipients,
                                                                                   should_use_docusign_renderer,
                                                                                   show_all_documents=force_show_all_docs)
            elif product.is_static_benefit():
                if not is_stp:
                    components += self.docusign_service.create_static_benefit_components(enrollment_data, recipients,
                                                                        should_use_docusign_renderer,
                                                                        enrollment_application, show_all_documents=force_show_all_docs)
            elif product.is_group_ci():
                if not is_stp:
                    components += self.docusign_service.create_group_ci_envelope_components(enrollment_data, recipients,
                                                                                            should_use_docusign_renderer,
                                                                                            show_all_documents=force_show_all_docs)

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
