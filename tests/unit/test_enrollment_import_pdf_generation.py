import unittest2

from mock import Mock, sentinel, call, patch
from hamcrest import assert_that, has_items, equal_to

from taa.services.enrollments import ImagedFormGeneratorService
from taa.services import services_broker
from taa.services.docusign.service import DocuSignRadioTab, DocuSignTextTab


class TestEnrollmentImportFormGeneration(unittest2.TestCase):
    def setUp(self):
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

        # Return the test data when requested.
        self.tab_repository = Mock()
        services_broker.Provide("FormTemplateTabRepository", self.tab_repository)
        self.tab_repository.return_value.get_tabs_for_template.return_value = self.tab_repository_test_data

        self.pdf_renderer = Mock()
        services_broker.Provide("FormPDFRenderer", self.pdf_renderer)

        # System under test
        self.sut = ImagedFormGeneratorService()


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

