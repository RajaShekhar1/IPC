
from taa.services import RequiredFeature
from taa.services.docusign.service import DocuSignTextTab, DocuSignRadioTab

class ImagedFormGeneratorService(object):

    tab_repository = RequiredFeature("FormTemplateTabRepository")
    pdf_renderer = RequiredFeature('FormPDFRenderer')

    def generate_form_pdf(self, template_id, enrollment_data):
        tab_definitions = self.tab_repository.get_tabs_for_template(template_id)

        for tab_value in enrollment_data:
            if isinstance(tab_value, DocuSignTextTab):
                value = tab_value.name

                for tab_name, tab_def in tab_definitions.items():
                    if tab_value.name == tab_name:
                        self.pdf_renderer.draw_text(x=tab_def['x'], y=tab_def['y'], text=value)


class FormTemplateTabRepository(object):
    def get_tabs_for_template(self, template_id):
        pass


class FormPDFRenderer(object):
    def draw_text(self, x, y, width):
        pass

    def draw_radio_checkmark(self, x, y):
        pass