from datetime import datetime
import json
import traceback

import taa.tasks as tasks
from taa import db
from taa.config_defaults import DOCUSIGN_CC_RECIPIENTS
from taa.services import RequiredFeature
from taa.services.docusign.docusign_envelope import EnrollmentDataWrap
from taa.services.docusign.service import AgentDocuSignRecipient, EmployeeDocuSignRecipient, CarbonCopyRecipient
from taa.services.enrollments.models import EnrollmentImportBatchItem


class EnrollmentSubmissionService(object):
    enrollment_application_service = RequiredFeature('EnrollmentApplicationService')
    enrollment_batch_service = RequiredFeature('EnrollmentImportBatchService')

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
        
        for batch_item in self.enrollment_batch_service.get_records_needing_submission(enrollment_batch):
            self.process_and_update_status(batch_item)


    def process_and_update_status(self, batch_item):
        try:
            self.process_import_submission(batch_item)
        except Exception as exc:
            self._mark_item_error(batch_item, exc)

    def process_import_submission(self, batch_item):
        """
        Submit the enrollment and mark as complete
        """
        self._mark_item_processing(batch_item)

        EnrollmentSubmissionProcessor().submit_to_docusign(batch_item.enrollment_record)

        self._mark_item_success(batch_item)

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


class EnrollmentSubmissionProcessor(object):

    pdf_generator_service = RequiredFeature('ImagedFormGeneratorService')
    docusign_service = RequiredFeature('DocuSignService')

    def submit_to_docusign(self, enrollment_record):

        data_wrap = EnrollmentDataWrap(json.loads(enrollment_record.standardized_data),
                                       census_record=enrollment_record.census_record,
                                       case=enrollment_record.case)
        recipients = self._create_import_recipients(enrollment_record.case, data_wrap)
        components = self.docusign_service.create_fpp_envelope_components(
            data_wrap,
            recipients,
            should_use_docusign_renderer=False
        )

        # Generate envelope
        envelope = self.docusign_service.create_envelope(
            email_subject="Enrollment imported: {} for {} ({})".format(
                data_wrap.get_product_code(),
                data_wrap.get_employee_name(),
                data_wrap.get_employer_name()),
            components=components
        )

    def _create_import_recipients(self, case, enrollment_data):

        # Exclude both from the envelope, use them only for tab generation purposes
        signing_agent = self.docusign_service.get_signing_agent(case)
        recipients = [
            AgentDocuSignRecipient(name=signing_agent.name(),
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
