import re

from reportlab.platypus import Paragraph, Spacer
from reportlab.lib.styles import getSampleStyleSheet, ParagraphStyle, _baseFontNameB, _baseFontName
from reportlab.pdfgen.canvas import Canvas
from reportlab.lib.units import mm, inch
from reportlab.lib.colors import black
from reportlab.graphics.renderPDF import Drawing
from reportlab.graphics.shapes import Line, String

# Some common styles used in the attachment documents

styles = getSampleStyleSheet()
style = styles["Normal"]
bold_style = ParagraphStyle(name='HeadingCustom',
                          parent=style,
                          fontName = _baseFontNameB,
                          fontSize=10,
                          leading=14,
                          spaceBefore=6,
                          spaceAfter=6)
bold_style2 = ParagraphStyle(name='HeadingCustom2',
                          parent=bold_style,
                          fontName = _baseFontNameB,
                          fontSize=10,
                          leading=14,
                          spaceBefore=6,
                          spaceAfter=4)


def create_attachment_header(title, enrollment_data):
    "Draws a fancy header using the enrollment data and a title, and returns a list of flowables."

    group_name = enrollment_data.get_employer_name()
    employee_first = enrollment_data['employee']['first']
    employee_last = enrollment_data['employee']['last']
    employee_ssn = enrollment_data['employee']['ssn']
    masked_ssn = mask_ssn(employee_ssn)

    product = enrollment_data.get_product()
    if product.is_fpp():
        product_title = '5Star Family Protection Plan Application'
    else:
        product_title = "{} Application".format(product.get_short_name())
    
    return [
        Paragraph(product_title, style),
        Paragraph(title, bold_style),
        Spacer(0, .2*inch),
        Paragraph(u"Employer/Group: {}".format(group_name), style),
        Paragraph(u"Employee: {} {} {}".format(employee_first, employee_last, masked_ssn), style),
    ]


def create_signature_line(page_width, signature_coordinate_map, recipient_names):
    flowables = []

    # Get a wrapper around the drawing class so we can extract the coords out for the signature tab.

    for recip_name in recipient_names:
        flowables.append(Spacer(0, .1*inch))

        # Wrap the drawings in our Drawing object so it flows with the document and we can extract the signature coords
        sig_height = .75 * inch
        CoordSavingDrawing = _wrap_drawing_class(signature_coordinate_map, recip_name)
        sig_drawing = CoordSavingDrawing(page_width, sig_height)

        # Pull out the drawing object we are wrapping, and add the line and the name for this recipient
        sig_drawing.add(Line(0, 16, 4*inch, 16, strokeColor=black))
        sig_drawing.add(String(0, 0, "Employee", fontSize=11, fillColor=black))

        flowables.append(sig_drawing)

    return flowables


def _wrap_drawing_class(sig_coordinate_map, recipient_name):
    # The following represents a bit of a hack on reportlab to extract the coordinates of a
    #   Flowable object just before it is rendered. This lets us feed the coordinates of the
    #   rendered line into a DocuSign tab specification.
    #
    #  Normally the code would be a little more straightforward, but the  Drawing object has problems
    #   with any extra attributes -> reportlab raises an exception if you try to track other data within the class,
    #   so I overcome that here using a closure over the arguments passed to this function.

    class DrawingWithCoordTracking(Drawing):
        def drawOn(self, canvas, x, y, _sW=0):
            # Flowable alignment adjustment
            x = self._hAlignAdjust(x,_sW)

            # Magic number from trial and error. Will need adjustments if the height of the
            #  signature is changed (or placement of the line within the signature drawing).
            manual_adjustment = 10

            # Store the coordinates of the signature line.
            sig_coordinate_map[recipient_name] = (x, y + self.height + manual_adjustment)

            # Let the overridden method do its thing
            Drawing.drawOn(self, canvas, x, y, _sW)

    # Return the class closure
    return DrawingWithCoordTracking


def mask_ssn(ssn):
    if not ssn:
        return ""

    p = re.compile('\d\d\d-?\d\d-?(\d\d\d\d)')
    match = p.match(ssn)
    if not match:
        return ssn

    last_four = match.groups()[0]
    return "xxx-xx-%s"%last_four


class NumberedCanvas(Canvas):
    def __init__(self, *args, **kwargs):
        Canvas.__init__(self, *args, **kwargs)
        self._saved_page_states = []

    def showPage(self):
        self._saved_page_states.append(dict(self.__dict__))
        self._startPage()

    def save(self):
        """add page info to each page (page x of y)"""
        num_pages = len(self._saved_page_states)
        for state in self._saved_page_states:
            self.__dict__.update(state)
            self.draw_page_number(num_pages)
            Canvas.showPage(self)
        Canvas.save(self)

    def draw_page_number(self, page_count):
        self.saveState()
        self.setFont("Helvetica", 7)
        self.drawRightString(200*mm, 20*mm,
            "Page %d of %d" % (self._pageNumber, page_count))
        self.restoreState()