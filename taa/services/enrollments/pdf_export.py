
from taa.services import RequiredFeature
from taa.services.docusign.service import DocuSignTextTab, DocuSignRadioTab

class ImagedFormGeneratorService(object):

    tab_repository = RequiredFeature("FormTemplateTabRepository")
    pdf_renderer = RequiredFeature('FormPDFRenderer')

    def generate_form_pdf(self, template_id, enrollment_data):
        tab_definitions = self.tab_repository.get_tabs_for_template(template_id)

        for tab_value in enrollment_data:
            if isinstance(tab_value, DocuSignTextTab):
                label = tab_value.name
                text = tab_value.value

                tab_defs = filter(lambda x: x['label'] == label, tab_definitions)
                if not tab_defs:
                    continue
                tab_def = tab_defs[0]
                self.pdf_renderer.draw_text(text=text, x=tab_def['x'], y=tab_def['y'], width=tab_def['width'])
            elif isinstance(tab_value, DocuSignRadioTab):
                label = tab_value.group_name
                value = tab_value.value
                tab_defs = filter(lambda x: x['label'] == label and x['value'] == value, tab_definitions)
                if not tab_defs:
                    continue
                tab_def = tab_defs[0]
                self.pdf_renderer.draw_radio_checkmark(x=tab_def['x'], y=tab_def['y'])

class FormTemplateTabRepository(object):
    def get_tabs_for_template(self, template_id):
        pass


class FormPDFRenderer(object):
    def draw_text(self, text, x, y, width):
        pass

    def draw_radio_checkmark(self, x, y):
        pass