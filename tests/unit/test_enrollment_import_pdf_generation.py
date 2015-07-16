import unittest2

from mock import Mock, sentinel, call, patch
from hamcrest import assert_that, has_items, equal_to

from taa.services import initialize_services, services_broker
from taa.services.enrollments import ImagedFormGeneratorService
from taa.services.docusign.service import DocuSignRadioTab, DocuSignTextTab


class TestEnrollmentImportFormGeneration(unittest2.TestCase):
    def setUp(self):
        # Reset to default services before each test.
        initialize_services()

        # Setup some sample data.
        self.template_id = sentinel.template_id
        self.good_enrollment_data = [
            DocuSignTextTab('empFirst', 'Joe'),
            DocuSignTextTab('empLast', 'Smith'),
            DocuSignRadioTab('paymentMode', 'monthly'),
        ]

        self.tab_repository_test_data = [
            {'label': 'empFirst', 'x': sentinel.empFirstX, 'y': sentinel.empFirstY, 'width': sentinel.empFirstWidth},
            {'label': 'empLast', 'x': sentinel.empLastX, 'y': sentinel.empLastY, 'width': sentinel.empLastWidth},
            {'label': 'paymentMode', 'x': sentinel.empPaymentModeX, 'y': sentinel.empPaymentModeY, 'value': 'monthly'},
        ]

        # Override with mock services
        self.tab_repository = self.mock_service("FormTemplateTabRepository")
        self.pdf_renderer = self.mock_service("FormPDFRenderer")

        # Return the test data when requested.
        self.tab_repository.return_value.get_tabs_for_template.return_value = self.tab_repository_test_data

        # System under test
        self.sut = ImagedFormGeneratorService()

    def mock_service(self, name):
        mock = Mock()
        services_broker.Provide(name, mock)
        return mock

    def test_it_queries_the_tab_repository(self):

        self.sut.generate_form_pdf(self.template_id, self.good_enrollment_data)

        self.tab_repository.return_value.get_tabs_for_template.assert_called_with(self.template_id)

    def test_it_renders_tabs_to_pdf(self):

        self.sut.generate_form_pdf(self.template_id, self.good_enrollment_data)

        expected_calls = [
            call().draw_text(x=sentinel.empFirstX, y=sentinel.empFirstY, width=sentinel.empFirstWidth, text='Joe'),
            call().draw_text(x=sentinel.empLastX, y=sentinel.empLastY, width=sentinel.empLastWidth, text='Smith'),
            call().draw_radio_checkmark(x=sentinel.empPaymentModeX, y=sentinel.empPaymentModeY),
        ]

        assert_that(self.pdf_renderer.mock_calls, has_items(*expected_calls))

