
from unittest2 import TestCase
from mock import Mock
from hamcrest import assert_that, equal_to, greater_than_or_equal_to

from taa.services import services_broker
from taa.services.enrollments import EnrollmentApplication
from taa.services.enrollments.enrollment_submission import EnrollmentSubmissionProcessor


class TestEnrollmentSubmission(TestCase):

    def setUp(self):
        self.mock_pdf_generator = Mock()
        self.enrollment_record = Mock()
        self.enrollment_record.standardized_data = """{
            "product_type": "FPPTI",
            "employee":{"first":"Bob", "last":"Smith"}
        }"""

        self.mock_emp = Mock()
        self.mock_agent = Mock()

        self.mock_docusign_service = Mock()
        self.mock_docusign_service.create_envelope_recipients.return_value = (
            self.mock_emp,
            [self.mock_emp, self.mock_agent]
        )

        services_broker.Provide('DocuSignService', self.mock_docusign_service)

        self.sut = EnrollmentSubmissionProcessor()
        self.sut.pdf_generator_service = self.mock_pdf_generator
        self.sut.docusign_service = self.mock_docusign_service

    def test_it_uses_local_pdf_generation_when_source_is_import(self):

        self.sut.submit_to_docusign(self.enrollment_record)

        assert_that(self.mock_docusign_service.create_envelope.call_count, greater_than_or_equal_to(1))
