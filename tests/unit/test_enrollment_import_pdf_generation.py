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

        self.tab_repository_test_data = map(lambda x: FakeTabDef(**x), [
            {'label': 'empFirst', 'x': sentinel.empFirstX, 'y': sentinel.empFirstY, 'width': sentinel.empFirstWidth},
            {'label': 'empLast', 'x': sentinel.empLastX, 'y': sentinel.empLastY, 'width': sentinel.empLastWidth},
            {'label': 'paymentMode.monthly','name':'monthly', 'x': sentinel.empPaymentModeX, 'y': sentinel.empPaymentModeY},
        ])

        # Override with mock services
        self.tab_repository = self.mock_service("FormTemplateTabRepository")
        self.pdf_renderer = self.mock_service("FormPDFRenderer")
        self.merge_pdfs = self.mock_service("merge_pdfs")

        # Return the test data when requested.
        self.tab_repository.return_value.get_tabs_for_template.return_value = self.tab_repository_test_data
        self.tab_repository.return_value.has_template.return_value = True
        mock_template = Mock()
        mock_template.data = ""
        self.tab_repository.return_value.get_template.return_value = mock_template

        # System under test
        self.imaged_form_generator = ImagedFormGeneratorService()

    def mock_service(self, name):
        mock = Mock()
        services_broker.Provide(name, mock)
        return mock

    def test_it_queries_the_tab_repository(self):

        self.imaged_form_generator.generate_form_pdf(self.template_id, self.good_enrollment_data)

        self.tab_repository.return_value.get_tabs_for_template.assert_called_with(self.template_id)

    def test_it_renders_tabs_to_pdf(self):

        self.imaged_form_generator.generate_form_pdf(self.template_id, self.good_enrollment_data)

        expected_calls = [
            call()().draw_text(
                x=sentinel.empFirstX,
                y=sentinel.empFirstY,
                width=sentinel.empFirstWidth,
                text='Joe',
                is_italic=None,
                fontsize=None,
                is_bold=None,
                fontcolor=None,
                font=None,
            ),
            call()().draw_text(x=sentinel.empLastX, y=sentinel.empLastY, width=sentinel.empLastWidth, text='Smith',
                            is_italic=None,
                            fontsize=None,
                            is_bold=None,
                            fontcolor=None,
                            font=None),
            call()().draw_radio_checkmark(x=sentinel.empPaymentModeX, y=sentinel.empPaymentModeY),
        ]

        assert_that(self.pdf_renderer.mock_calls, has_items(*expected_calls))


class FakeTabDef(object):
    def __init__(self, label, x, y, name=None, width=None, font=None, font_size=None, is_bold=None, is_italic=None, font_color=None, page=1, type_=None):
        self.label = label
        self.name = name
        self.x = x
        self.y = y
        self.width = width
        self.font = font
        self.font_size = font_size
        self.is_bold = is_bold
        self.is_italic = is_italic
        self.font_color = font_color
        self.page = page
        self.type_ = type_