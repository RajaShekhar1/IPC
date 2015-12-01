import os
from io import BytesIO

from reportlab.lib.colors import HexColor
from reportlab.lib.units import inch
from reportlab.lib.utils import simpleSplit
from reportlab.pdfgen import canvas
from reportlab.pdfbase import pdfmetrics
from reportlab.pdfbase.pdfmetrics import stringWidth
from reportlab.pdfbase.ttfonts import TTFont
from PyPDF2 import PdfFileReader

from .models import db
from taa.services import RequiredFeature
from taa.services.docusign.service import DocuSignTextTab, DocuSignRadioTab, DocuSignSigTab
from taa.services.enrollments.models import FormTemplate, FormTemplateTabs


DEFAULT_FONT = 'LucidaConsole'
DEFAULT_COLOR = HexColor('#000000')
FONT_DIR = 'taa/services/enrollments/pdf_generator_fonts'


class ImagedFormGeneratorService(object):

    tab_repository = RequiredFeature('FormTemplateTabRepository')
    pdf_renderer_service = RequiredFeature('FormPDFRenderer')
    merge_pdfs = RequiredFeature('merge_pdfs')

    def generate_form_pdf(self, template_id, enrollment_tabs, path=None):

        template = self.validate_template(template_id)
        tab_definitions = self.tab_repository.get_tabs_for_template(template_id)

        return self.generate_overlay_pdf_from_tabs(
            enrollment_tabs,
            tab_definitions,
            template.data,
        )

    def generate_overlay_pdf_from_tabs(self, enrollment_tabs, tab_definitions, base_pdf_bytes):
        # Get a new FormPDFRenderer
        self.pdf_renderer = self.pdf_renderer_service()

        self.tab_pages = {}
        self._initialize_tab_pages(base_pdf_bytes)
        self._match_tab_values_to_defs(enrollment_tabs, tab_definitions)
        self._add_signature_tabs(enrollment_tabs, tab_definitions)
        self._render_tabs()
        return self.combine_pdfs(base_pdf_bytes)

    def _initialize_tab_pages(self, base_pdf_bytes):
        if not base_pdf_bytes:
            return

        # Ensure there is a page for each page of the base pdf
        pdf_reader = PdfFileReader(BytesIO(base_pdf_bytes))
        num_pages = pdf_reader.getNumPages()
        for p in range(num_pages):
            # Use 1-indexed page numbers to match the tabs
            self.tab_pages[p+1] = []

    def validate_template(self, template_id):
        template = self.tab_repository.get_template(template_id)
        if not template:
            raise Exception("Template ID '{}' not found".format(template_id))
        return template

    def _match_tab_values_to_defs(self, enrollment_tabs, tab_definitions):
        for tab_value in enrollment_tabs:
            self._match_tab_value_to_def(tab_definitions, tab_value)

    def _match_tab_value_to_def(self, tab_definitions, tab_value):
        if isinstance(tab_value, DocuSignTextTab):
            self._match_text_tab(tab_definitions, tab_value)
        elif isinstance(tab_value, DocuSignRadioTab):
            self._match_radio_tab(tab_definitions, tab_value)

    def _match_radio_tab(self, tab_definitions, tab_value):
        label = tab_value.group_name
        value = tab_value.value
        for tab_def in filter(
                lambda x: x.label == '{}.{}'.format(label, value), tab_definitions):
            self._add_to_tab_pages(tab_def, tab_value)

    def _match_text_tab(self, tab_definitions, tab_value):
        label = tab_value.name
        for tab_def in filter(lambda x: x.label == label, tab_definitions):
            self._add_to_tab_pages(tab_def, tab_value)

    def _add_to_tab_pages(self, tab_def, tab_value):
        page = int(tab_def.page)
        if page not in self.tab_pages:
            self.tab_pages[page] = []
        self.tab_pages[page].append((tab_def, tab_value))

    def _render_tabs(self):
        for page in sorted(self.tab_pages):
            for tab_def, tab_value in self.tab_pages[page]:
                self._render_tab(tab_def, tab_value)
            self.pdf_renderer.next_page()

    def _render_tab(self, tab_def, tab_value):
        if isinstance(tab_value, DocuSignTextTab):
            self._render_text_tab(tab_def, text=tab_value.value)
        elif isinstance(tab_value, DocuSignRadioTab):
            self._render_radio_tab(tab_def)

    def _render_text_tab(self, tab_def, text):

        font = tab_def.font
        fontsize = tab_def.font_size
        is_bold = tab_def.is_bold
        is_italic = tab_def.is_italic
        fontcolor = tab_def.font_color
        y = tab_def.y
        x = tab_def.x

        # Special case for SignHere and DateSigned tabs
        #if (tab_def.type_ == 'SignHere' or
        #            tab_def.type_ == 'DateSigned'):
        #    fontcolor = 'Green'
        if tab_def.type_ == 'SignHere':
            y += 35
            x -= 5
            fontsize = 8

        self.pdf_renderer.draw_text(text=text, x=x,
                                    y=y,
                                    width=tab_def.width,
                                    font=font, fontsize=fontsize,
                                    is_bold=is_bold,
                                    is_italic=is_italic,
                                    fontcolor=fontcolor)

    def _render_radio_tab(self, tab_def):
        self.pdf_renderer.draw_radio_checkmark(x=tab_def.x,
                                               y=tab_def.y)

    def combine_pdfs(self, base_pdf_bytes):
        overlay_pdf = self.pdf_renderer.get_pdf_bytes()
        base_pdf = BytesIO(base_pdf_bytes)
        merged_pdf = self.merge_pdfs(base_pdf, overlay_pdf)
        return merged_pdf

    def _add_signature_tabs(self, enrollment_tabs, tab_definitions):
        self.add_custom_signature_tabs(enrollment_tabs, tab_definitions)
        self.match_signatures_to_defs(enrollment_tabs, tab_definitions)

    def match_signatures_to_defs(self, enrollment_tabs, tab_definitions):
        """
        Match up any eSignatures to proper signature definitions
        """

        def is_tab_for_role_and_type(role, type_):
            """returns a filter function for tab defs matching both recipient role and tab type"""
            def f(t):
                return t.recipient_role == role and t.type_ == type_
            return f

        for recip_role in ['Employee', 'Agent']:
            for tab_type in ['SignHere', 'DateSigned', 'InitialHere']:
                tab_value_label = "{}{}".format(tab_type, recip_role)
                tab_values = filter(lambda t: t.name == tab_value_label, enrollment_tabs)
                tab_defs = filter(is_tab_for_role_and_type(recip_role, tab_type), tab_definitions)

                if tab_values and tab_defs:
                    for tab_def in tab_defs:
                        self._add_to_tab_pages(tab_def, tab_values[0])

    def add_custom_signature_tabs(self, enrollment_tabs, tab_definitions):
        custom_sig_tabs = filter(lambda t: isinstance(t, DocuSignSigTab), enrollment_tabs)
        for tab in custom_sig_tabs:
            # Create an ad-hoc tab definition
            tab_def = FormTemplateTabs(
                page=tab.page_number,
                x=tab.x,
                y=tab.y,
                type_="SignHere",
                recipient_role="Employee",
            )
            tab_definitions.append(tab_def)


class FormTemplateTabRepository(object):
    def get_tabs_for_template(self, template_id):
        if not self.has_template(template_id):
            return []

        form_template_id = self.get_template(template_id).id

        return list(FormTemplateTabs.query.filter_by(
            form_template_id=form_template_id))

    def has_template(self, template_id):
        return bool(self.get_template(template_id))

    def get_template(self, template_id):
        return FormTemplate.query.filter(
            FormTemplate.template_id.ilike(template_id)).first()


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
        text = str(text)

        self.set_color(fontcolor)
        self.set_font(font, fontsize, is_bold, is_italic)
        text = self.ensure_text_fits(text, width)

        self._render_string(text, x, y)

        self.reset_font_color(fontcolor)

    def draw_radio_checkmark(self, x, y):
        self.c.setFont('LucidaConsole', 10)
        self._render_string("X", x, y)

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

    def ensure_text_fits(self, text, width):
        # Ensure text fits in width if specified
        if width is not None and len(text) > 0:
            lines = simpleSplit(text, self.c._fontname, self.c._fontsize, width)
            # Possible to have an empty list returned, so check that lines has an entry here.
            #  If it doesn't, just leave the text alone as a fallback.
            if lines:
                text = lines[0]
            
        return text

    def set_font(self, font, fontsize, is_bold, is_italic):
        fontmap_font = self.lookup_font(font, is_bold, is_italic)
        fontsize = fontsize or 10
        self.c.setFont(fontmap_font, fontsize)

    def set_color(self, fontcolor):
        self.c.saveState()
        # Set font color if specified
        if fontcolor is not None:
            self.c.setFillColor(COLORMAP.get(fontcolor, DEFAULT_COLOR))

    def reset_font_color(self, fontcolor):
        self.c.restoreState()

    def lookup_font(self, font, is_bold, is_italic):
        # Determine font to use
        font = font or DEFAULT_FONT
        if is_bold and is_italic:
            font += '-BoldOblique'
        elif is_bold:
            font += '-Bold'
        elif is_italic:
            font += '-Oblique'
        font = FONTMAP.get(font, DEFAULT_FONT)
        return font

    def _render_string(self, text, x, y):
        self.c.drawString(*self._translate(x, y), text=text)

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
