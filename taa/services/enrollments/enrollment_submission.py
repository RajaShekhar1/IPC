import json

from taa.services import RequiredFeature
from taa.services.docusign.docusign_envelope import EnrollmentDataWrap
from taa.services.docusign.service import AgentDocuSignRecipient, EmployeeDocuSignRecipient, CarbonCopyRecipient

class EnrollmentSubmissionService(object):
    def submit_imported_enrollment(self, enrollment_record):

        processor = EnrollmentSubmissionProcessor()
        processor.submit_to_docusign(enrollment_record)

        return processor


class EnrollmentSubmissionProcessor(object):

    pdf_generator_service = RequiredFeature('ImagedFormGeneratorService')
    docusign_service = RequiredFeature('DocuSignService')

    def submit_to_docusign(self, enrollment_record):

        data_wrap = EnrollmentDataWrap(json.loads(enrollment_record.standardized_data),
                                       census_record=enrollment_record.census_record,
                                       case=enrollment_record.case)
        #employee_recip, recipients = self.docusign_service.create_envelope_recipients(enrollment_record.case, data_wrap)
        recipients = self._create_import_recipients(enrollment_record.case, data_wrap)
        components = self.docusign_service.create_fpp_envelope_components(data_wrap, recipients, should_use_docusign_renderer=False)

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
        recipients += self._get_carbon_copy_recipients(enrollment_data)
        return recipients

    def _get_carbon_copy_recipients(self, enrollment_data):

        return [CarbonCopyRecipient("Zach Mason", "zach@zachmason.com")]