import os
from io import BytesIO

from reportlab.lib.units import inch
from reportlab.pdfgen import canvas
from reportlab.pdfbase import pdfmetrics
from reportlab.pdfbase.pdfmetrics import stringWidth
from reportlab.pdfbase.ttfonts import TTFont

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
                tab_defs = filter(
                    lambda x: x.label == '{}.{}'.format(label,
                                                        value), tab_definitions)
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
                    self.pdf_renderer.draw_text(text=text, x=tab_def.x,
                                                y=tab_def.y,
                                                width=tab_def.width,
                                                font=font, fontsize=fontsize)
                elif isinstance(tab_value, DocuSignRadioTab):
                    self.pdf_renderer.draw_radio_checkmark(x=tab_def.x,
                                                           y=tab_def.y)
            self.pdf_renderer.next_page()

        if path is not None:
            overlay_pdf = self.pdf_renderer.get_pdf_bytes()
            base_pdf = BytesIO(template.data)
            merge_pdfs(base_pdf, overlay_pdf, path)


class FormTemplateTabRepository(object):
    def get_tabs_for_template(self, template_id):
        form_template_id = FormTemplate.query.filter_by(
            template_id=template_id).first().id
        return list(FormTemplateTabs.query.filter_by(
            form_template_id=form_template_id))


FONTPATH_LUCIDIA = os.path.join('Artifacts', 'fonts', 'LucidaConsole.ttf')
FONTPATH_COURIERNEW = os.path.join('Artifacts', 'fonts', 'CourierNew-Bold.ttf')
FONTMAP = {
    'CourierNew': 'CourierNew',
    'LucidaConsole': 'LucidaConsole',
    'Arial': 'Helvetica',
    'TimesNewRoman': 'Times',
}

class FormPDFRenderer(object):
    def __init__(self):
        self.buf = BytesIO()
        self.c = canvas.Canvas(self.buf, pagesize=(8.5 * inch,
                                                   11 * inch))
        self.c.setStrokeColorRGB(0, 0, 0)
        self.c.setFillColorRGB(0, 0, 0)
        self.c.setFont('Helvetica', 12)
        pdfmetrics.registerFont(TTFont('LucidaConsole', FONTPATH_LUCIDIA))
        pdfmetrics.registerFont(TTFont('CourierNew', FONTPATH_COURIERNEW))

    def draw_text(self, text, x, y, width, font=None, fontsize=None,
                  is_bold=False, is_italic=False):
        font = FONTMAP.get(font, 'Helvetica')
        if is_bold and is_italic:
            font += '-BoldOblique'
        elif is_bold:
            font += '-Bold'
        elif is_italic:
            font += '-Oblique'
        fontsize = fontsize or 10
        self.c.setFont(font, fontsize)
        self.c.drawString(*self._translate(x, y), text=text)

    def draw_radio_checkmark(self, x, y):
        self.c.setFont('Courier', 10)
        self.c.drawString(*self._translate(x, y), text="X")

    def next_page(self):
        self.c.showPage()

    def get_pdf_bytes(self, as_bytesio=True):
        self.c.save()
        if as_bytesio:
            return self.buf
        else:
            pdf = self.buf.getvalue()
            self.buf.close()
            return pdf

    def _translate(self, x, y):
        face = pdfmetrics.getFont(self.c._fontname).face
        y_offset = face.descent * self.c._fontsize / 1000
        return x + 8, self.c._pagesize[1] - y - y_offset - 14

    def _get_width(self, s):
        return stringWidth(s, self.c._fontname, self.c._fontsize)
