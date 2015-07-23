import os
from io import BytesIO

from reportlab.lib.colors import HexColor
from reportlab.lib.units import inch
from reportlab.lib.utils import simpleSplit
from reportlab.pdfgen import canvas
from reportlab.pdfbase import pdfmetrics
from reportlab.pdfbase.pdfmetrics import stringWidth
from reportlab.pdfbase.ttfonts import TTFont

from .models import db
from taa.services import RequiredFeature
from taa.services.docusign.service import DocuSignTextTab, DocuSignRadioTab
from taa.services.enrollments.models import FormTemplate, FormTemplateTabs


DEFAULT_FONT = 'LucidaConsole'
DEFAULT_COLOR = HexColor('#000000')
FONT_DIR = 'taa/services/enrollments/pdf_generator_fonts'

class ImagedFormGeneratorService(object):

    tab_repository = RequiredFeature('FormTemplateTabRepository')
    pdf_renderer = RequiredFeature('FormPDFRenderer')
    merge_pdfs = RequiredFeature('merge_pdfs')

    def generate_form_pdf(self, template_id, enrollment_data, path=None):
        template = self.tab_repository.get_template(template_id)
        if not template:
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
                    is_bold = tab_def.is_bold
                    is_italic = tab_def.is_italic
                    fontcolor = tab_def.font_color
                    self.pdf_renderer.draw_text(text=text, x=tab_def.x,
                                                y=tab_def.y,
                                                width=tab_def.width,
                                                font=font, fontsize=fontsize,
                                                is_bold=is_bold,
                                                is_italic=is_italic,
                                                fontcolor=fontcolor)
                elif isinstance(tab_value, DocuSignRadioTab):
                    self.pdf_renderer.draw_radio_checkmark(x=tab_def.x,
                                                           y=tab_def.y)
            self.pdf_renderer.next_page()

        overlay_pdf = self.pdf_renderer.get_pdf_bytes()
        base_pdf = BytesIO(template.data)
        return self.merge_pdfs(base_pdf, overlay_pdf, path)


class FormTemplateTabRepository(object):
    def get_tabs_for_template(self, template_id):
        form_template_id = FormTemplate.query.filter_by(
            template_id=template_id).first().id
        return list(FormTemplateTabs.query.filter_by(
            form_template_id=form_template_id))

    def has_template(self, template_id):
        return bool(self.has_template(template_id))

    def get_template(self, template_id):
        return FormTemplate.query.filter_by(
            template_id=template_id).first()


pdfmetrics.registerFont(
    TTFont('LucidaConsole', os.path.join(FONT_DIR, 'LucidaConsole.ttf')))
pdfmetrics.registerFont(
    TTFont('CourierNew', os.path.join(FONT_DIR, 'CourierNew-Bold.ttf')))


FONTMAP = {
    'CourierNew': 'CourierNew',
    'LucidaConsole': 'LucidaConsole',
    'LucidaConsole-Oblique': 'Courier-Oblique',
    'LucidaConsole-Bold': 'Courier-Bold',
    'LucidaConsole-BoldOblique': 'Courier-BoldOblique',
    'Arial': 'Helvetica',
    'TimesNewRoman': 'Times',
}


# Unverified; approximated from HTML colors
COLORMAP = {
    'Black': HexColor('#000000'),
    'BrightBlue': HexColor('#add8e6'),
    'BrightRed': HexColor('#ff0000'),
    'DarkGreen': HexColor('#006400'),
    'DarkRed': HexColor('#8b0000'),
    'Gold': HexColor('#ffd700'),
    'Green': HexColor('#008000'),
    'NavyBlue': HexColor('#000080'),
    'Purple': HexColor('#800080'),
    'White': HexColor('#ffffff'),
}


class FormPDFRenderer(object):
    def __init__(self):
        self.buf = None
        self.c = None
        self._initialize()

    def draw_text(self, text, x, y, width, font=None, fontsize=None,
                  is_bold=False, is_italic=False, fontcolor=None):
        # Determine font to use
        font = font or DEFAULT_FONT
        if is_bold and is_italic:
            font += '-BoldOblique'
        elif is_bold:
            font += '-Bold'
        elif is_italic:
            font += '-Oblique'
        font = FONTMAP.get(font, DEFAULT_FONT)
        fontsize = fontsize or 10
        # Set font color if specified
        if fontcolor is not None:
            self.c.saveState()
            self.c.setFillColor(COLORMAP.get(fontcolor, DEFAULT_COLOR))
        self.c.setFont(font, fontsize)
        # Ensure text fits in width if specified
        if width is not None and len(text) > 0:
            lines = simpleSplit(text, self.c._fontname, self.c._fontsize, width)
            text = lines[0]
        self.c.drawString(*self._translate(x, y), text=text)
        if fontcolor is not None:
            self.c.restoreState()

    def draw_radio_checkmark(self, x, y):
        self.c.setFont('LucidaConsole', 10)
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

    def _initialize(self):
        self.buf = BytesIO()
        self.c = canvas.Canvas(self.buf, pagesize=(8.5 * inch,
                                                   11 * inch))
        self.c.setStrokeColorRGB(0, 0, 0)
        self.c.setFillColorRGB(0, 0, 0)
        self.c.setFont('Helvetica', 12)

    def _translate(self, x, y):
        face = pdfmetrics.getFont(self.c._fontname).face
        y_offset = face.descent * self.c._fontsize / 1000
        return x + 8, self.c._pagesize[1] - y - y_offset - 14
