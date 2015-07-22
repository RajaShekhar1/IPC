from cStringIO import StringIO

from reportlab.lib.units import inch
from reportlab.pdfgen import canvas
from reportlab.pdfbase import pdfmetrics
from reportlab.pdfbase.pdfmetrics import stringWidth

from .models import db
from pdf_generation import merge_pdfs
from taa.services import RequiredFeature
from taa.services.docusign.service import DocuSignTextTab, DocuSignRadioTab
from taa.services.enrollments.models import FormTemplate, FormTemplateTabs


class ImagedFormGeneratorService(object):

    tab_repository = RequiredFeature('FormTemplateTabRepository')
    pdf_renderer = RequiredFeature('FormPDFRenderer')

    def generate_form_pdf(self, template_id, enrollment_data, path=None):
        template = FormTemplate.query.filter_by(
            template_id=template_id).first()
        if template is None:
            # Failed to retrieve template
            raise Exception("Template ID '{}' not found".format(template_id))

        tab_definitions = self.tab_repository.get_tabs_for_template(template_id)

        tab_pages = {}
        for tab_value in enrollment_data:
            if isinstance(tab_value, DocuSignTextTab):
                label = tab_value.name
                text = tab_value.value

                tab_defs = filter(lambda x: x.label == label, tab_definitions)
                if not tab_defs:
                    continue
                tab_def = tab_defs[0]

                page = tab_def.page
                if page not in tab_pages:
                    tab_pages[page] = []
                tab_pages[page].append((tab_def, tab_value))
            elif isinstance(tab_value, DocuSignRadioTab):
                label = tab_value.group_name
                value = tab_value.value
                tab_defs = filter(lambda x: x.label == '{}.{}'.format(label, value), tab_definitions)
                if not tab_defs:
                    continue
                tab_def = tab_defs[0]

                page = tab_def.page
                if page not in tab_pages:
                    tab_pages[page] = []
                tab_pages[page].append((tab_def, tab_value))

        for page in sorted(tab_pages):
            for tab_def, tab_value in tab_pages[page]:
                if isinstance(tab_value, DocuSignTextTab):
                    text = tab_value.value
                    font = tab_def.font
                    fontsize = tab_def.font_size
                    self.pdf_renderer.draw_text(text=text, x=tab_def.x, y=tab_def.y, width=tab_def.width, font=font, fontsize=fontsize)
                elif isinstance(tab_value, DocuSignRadioTab):
                    self.pdf_renderer.draw_radio_checkmark(x=tab_def.x, y=tab_def.y)
            self.pdf_renderer.next_page()

        x = self.pdf_renderer.pdf

        # Write PDF
        form = template.data
        with open('/tmp/baseform.pdf', 'wb') as f:
            f.write(form)

        if path is not None:
            merge_pdfs('/tmp/values.pdf', '/tmp/baseform.pdf', path)

        return True

class FormTemplateTabRepository(object):
    def get_tabs_for_template(self, template_id):
        # db.session.query(FormTemplateTabs).filter(FormTemplateTabs.form_template_id)
        form_template_id = FormTemplate.query.filter_by(
            template_id=template_id).first().id
        return list(FormTemplateTabs.query.filter_by(
            form_template_id=form_template_id))


FONTMAP = {
    'CourierNew': 'Courier',
    'Arial': 'Helvetica',
    'TimesNewRoman': 'Times',
    # 'ArialNarrow': 'Helvetica','Calibri','Garamond','Georgia','LucidaConsole','Tahoma','Trebuchet','Verdana','MSGothic','MSMincho
}

class FormPDFRenderer(object):
    def __init__(self):
        self.buffer = StringIO()
        self.c = canvas.Canvas('/tmp/values.pdf', pagesize=(8.5 * inch,
                                                                  11 * inch))
        self.c.setStrokeColorRGB(0, 0, 0)
        self.c.setFillColorRGB(0, 0, 0)
        self.c.setFont('Helvetica', 12)

    def draw_text(self, text, x, y, width, font=None, fontsize=None, is_bold=False, is_italic=False):
        font = FONTMAP.get(font, 'Helvetica')
        if is_bold and is_italic:
            font += '-BoldOblique'
        elif is_bold:
            font += '-Bold'
        elif is_italic:
            font += '-Oblique'
        fontsize = fontsize or 10
        self.c.setFont(font, fontsize)
        self.c.drawString(self._translate_x(x), self._translate_y(y), text)

    def draw_radio_checkmark(self, x, y):
        self.c.setFont('Courier', 10)
        self.c.drawString(self._translate_x(x), self._translate_y(y), "X")

    def next_page(self):
        self.c.showPage()

    @property
    def pdf(self):
        self.c.save()
        pdf = self.c
        return pdf

    def _translate_x(self, x):
        return x + 10

    def _translate_y(self, y):
        face = pdfmetrics.getFont(self.c._fontname).face
        y_offset = face.descent * self.c._fontsize / 1000
        return 11 * inch - y - y_offset - 16
