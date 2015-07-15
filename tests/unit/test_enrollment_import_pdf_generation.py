import unittest

from mock import Mock, sentinel

from taa.services.enrollments import ImagedFormGeneratorService
from taa.services import services
from taa.services.docusign.service import DocuSignRadioTab, DocuSignTextTab


class TestEnrollmentImportFormGeneration(unittest.TestCase):
    def setUp(self):
        self.template_id = sentinel.template_id
        self.good_enrollment_data = [
            DocuSignTextTab('empFirst', 'Joe'),
            DocuSignTextTab('empLast', 'Smith'),
            DocuSignRadioTab('paymentMode', 'monthly'),
        ]

        self.tab_repository_test_data = {
            'empFirst': {'x': sentinel.empFirstX, 'y': sentinel.empFirstY, 'width': sentinel.empFirstWidth},
            'empLast': {'x': sentinel.empLastX, 'y': sentinel.empLastY, 'width': sentinel.empLastWidth},
            'paymentMode': {'x': sentinel.empPaymentModeX, 'y': sentinel.empPaymentModeY, 'value': 'monthly'},
        }

        # Stub out the PDF generation service
        self.pdf_renderer = Mock()
        self.tab_repository = Mock()
        self.tab_repository.return_value.get_tabs_for_template.return_value = self.tab_repository_test_data

        # System under test
        self.sut = ImagedFormGeneratorService()

        # Override service lookup with our service stubs
        services.Provide("FormPDFRenderer", self.pdf_renderer)
        services.Provide("FormTemplateTabRepository", self.tab_repository)


    def test_it_queries_the_tab_repository(self):


        self.sut.generate_form_pdf(self.template_id, self.good_enrollment_data)

        self.tab_repository.return_value.get_tabs_for_template.assert_called_with(self.template_id)

    # def test_it_renders_tabs_to_pdf(self):
    #     self.sut.generate_form_pdf(self.template_id, self.good_enrollment_data)
    #
    #     self.pdf_renderer.return_value.draw_text.assert_called_with(
    #         x=sentinel.empFirstX,
    #         y=sentinel.empFirstY,
    #         width=sentinel.empFirstWidth,
    #         text='Joe',
    #     )
    #     self.pdf_renderer.return_value.draw_text.assert_called_with(
    #         x=sentinel.empLast,
    #         y=sentinel.empLastY,
    #         width=sentinel.empLastWidth,
    #         text='Smith',
    #     )
