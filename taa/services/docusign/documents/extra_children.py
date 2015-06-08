import re

from reportlab.platypus import SimpleDocTemplate, Paragraph, Spacer, Table, TableStyle
from reportlab.lib.styles import getSampleStyleSheet, ParagraphStyle
from reportlab.lib.units import inch, mm
from reportlab.pdfgen.canvas import Canvas
from reportlab.graphics.shapes import Line, String
from reportlab.lib import colors
from reportlab.lib.colors import black
from reportlab.graphics.renderPDF import Drawing

from taa.services.docusign.service import (
    BasePDFDoc,
    DocuSignSigTab,
)

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


class ChildAttachmentForm(BasePDFDoc):
    def __init__(self, recipients, enrollment_data):
        BasePDFDoc.__init__(self, recipients)

        self.data = enrollment_data

        self.children = []

        # Map recipient signers to the coords of the signature line so we can attach tabs to those lines.
        self.sig_coords = {}

    def add_child(self, child_data):
        """
        Expects a dict of data with the following keys:
            first, last, ssn, gender (male or female), dob, soh_answers (dict with question, answer keys),
            coverage (int), premium (decimal)
        """
        self.children.append(child_data)

    def generate(self):

        styles = getSampleStyleSheet()

        def draw_first_page(canvas, doc):
            #canvas.saveState()
            # Title on top of first page
            #canvas.setFont('Times-Roman', 10)
            #canvas.drawCentredString(self.page_width/2.0, self.page_height - 2*inch, "Extra Children Attachment Form")
            #canvas.restoreState()
            pass

        def draw_extra_pages(canvas, doc):

            canvas.saveState()
            #canvas.setFont('Times-Roman', 9)
            #canvas.drawString(inch, 0.75 * inch,"Page %d" % (doc.page))
            canvas.restoreState()

        style = styles["Normal"]
        bold_style = styles["Heading3"]
        from reportlab.lib.styles import _baseFontNameB
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

        group_name = self.data.get_employer_name()
        employee_first = self.data['employee']['first']
        employee_last = self.data['employee']['last']
        employee_ssn = self.data['employee']['ssn']
        masked_ssn = mask_ssn(employee_ssn)

        child_table_style = TableStyle([
            # Put a box around each cell
             ('GRID', (0,0), (-1,-1), 0.25, colors.black),
            # Align first column
             ('ALIGN', (0,1), (0,-1), 'CENTER'),
            # Align Gender col
             ('ALIGN', (3,1), (3,-1), 'CENTER'),
            # Right-Align money columns
             ('ALIGN', (5,1), (5,-1), 'RIGHT'),
             ('ALIGN', (6,1), (6,-1), 'RIGHT'),
        ])
        child_table_data = [
            [
                "Child #",
                "Name",
                "SSN",
                "Gender",
                "Birth Date",
                "Coverage",
                "Premium",
            ]
        ]
        for num, child in enumerate(self.children):
            row = [
                str(num + 3),
                "%s %s"%(child['first'], child['last']),
                child["ssn"],
                "M" if child["gender"].lower()[0] == "m" else "F",
                child['birthdate'],
                "$%s"%child['coverage'],
                "$%s"%child['premium'],
            ]
            child_table_data.append(row)

        spacer = Spacer(0, .2 * inch)

        story = [

            Paragraph("5Star Family Protection Plan Application", style),
            Paragraph(u"<u>Supplemental Form:  Children\u2019s Information</u>", bold_style),
            spacer,
            Paragraph("Employer/Group: %s"%group_name, style),
            Paragraph("Employee: %s %s %s"%(employee_first, employee_last, masked_ssn), style),
            spacer,
            Paragraph("Information and Coverage", bold_style2),
            Table(child_table_data, style=child_table_style, hAlign='LEFT'),
            spacer,
            Paragraph("Statement of Health", bold_style2),

        ]

        # Build up questions mapped to child answers
        question_data = {}
        for child in self.children:
            child_name = child['first']
            for soh_data in child['soh_questions']:
                if soh_data['question'] not in question_data:
                    question_data[soh_data['question']] = {}

                question_data[soh_data['question']][child_name] = soh_data['answer']

        for question in question_data:
            story.append(Paragraph(question, style))

            answer_table_data = []
            for child_name, answer in question_data[question].iteritems():
                answer_table_data.append(
                    ["", Paragraph(child_name, style), Paragraph(answer, style)],
                )
            story.append(Table(answer_table_data, colWidths=[.2*inch, 1.5*inch, None]))



        # The following represents a bit of a hack on reportlab to extract the coordinates of a
        #   Flowable object just before it is rendered. This lets us feed the coordinates of the
        #   rendered line into a DocuSign tab specification.
        #
        #  Normally the code would be a little more straightforward, but the  Drawing object has problems
        #   with any extra attributes -> reportlab raises an exception if you try to track other data within the class,
        #   so I overcome that here using closures over a wrapper object.
        child_attachment_object = self
        class CoordSavingDrawing(object):
            def __init__(self, width, height, recipient, **kwargs):
                self.recipient = recipient

                drawing_wrap = self
                class DrawingWithCoordTracking(Drawing):
                    def drawOn(self, canvas, x, y, _sW=0):
                        # Flowable alignment adjustment
                        x = self._hAlignAdjust(x,_sW)

                        # Store the coordinates of the signature line
                        child_attachment_object.sig_coords[drawing_wrap.recipient.name] = (x, y + self.height)
                        Drawing.drawOn(self, canvas, x, y, _sW)

                self.drawing = DrawingWithCoordTracking(width, height, **kwargs)


        # Signature line
        signers = [r for r in self.recipients if r.is_employee()]
        sig_height = 1 * inch
        for recip in signers:
            story.append(Spacer(0, .75*inch))
            sig_line = Line(0, 16, 4*inch, 16, strokeColor=black)
            #sig_line.strokeColor = black
            sig_drawing_wrap = CoordSavingDrawing(self.page_width, sig_height, recip)
            sig_drawing = sig_drawing_wrap.drawing
            sig_drawing.add(sig_line)
            sig_drawing.add(String(0, 0, recip.name, fontSize=11, fillColor=black))
            story.append(sig_drawing)

        self._doc.build(story, onFirstPage=draw_first_page, onLaterPages=draw_extra_pages, canvasmaker=NumberedCanvas)

    def generate_tabs(self, recipient):
        tabs = {}
        #if recipient.is_employee():
        if self.is_recipient_signer(recipient):
            # Add a signature tab to the last page

            pdf_x, pdf_y = self.sig_coords[recipient.name]
            pix_x = pdf_x

            pix_y = (self.page_height - pdf_y)
            #print("Converted %s, %s to %s %s"%(pdf_x, pdf_y, pix_x, pix_y))
            tab = DocuSignSigTab(x=pix_x, y=pix_y, document_id="1", page_number=str(self.get_num_pages()))
            tab.add_to_tabs(tabs)

        return tabs

    def is_recipient_signer(self, recipient):
        return recipient.is_employee() # or recipient.is_agent()


def mask_ssn(ssn):
    if not ssn:
        return ""

    p = re.compile('\d\d\d-?\d\d-?(\d\d\d\d)')
    match = p.match(ssn)
    if not match:
        return ssn

    last_four = match.groups()[0]
    return "xxx-xx-%s"%last_four

if __name__ == "__main__":
    # Test drive the code

    from taa.services.docusign.service import AgentDocuSignRecipient, EmployeeDocuSignRecipient
    from taa.services.docusign.docusign_envelope import EnrollmentDataWrap

    agent = AgentDocuSignRecipient(name="Zachary Mason", email="zmason@delmarsd.com")
    employee = EmployeeDocuSignRecipient(name="Joe Tester", email="zach@zachmason.com")
    test_recipients = [
        agent,
        employee,
    ]

    child_attachment_form = ChildAttachmentForm(test_recipients, enrollment_data=EnrollmentDataWrap(dict(
        agent_data=dict(company_name="DelMar SD"),
        employee=dict(first="Test", last="Employee", ssn="123-12-1234")
    ), None))

    child_attachment_form.add_child(dict(
        first="Joe",
        last="Johnson",
        birthdate="12/01/2010",
        gender="male",
        ssn='123-12-1234',
        soh_questions=[dict(question="Have you ever eaten a lollipop?", answer="no")],
        coverage=10000,
        premium='10.50')
    )
    child_attachment_form.add_child(dict(
        first="Susie",
        last="Johnson",
        birthdate="12/01/2012",
        gender="female",
        ssn='111-12-2222',
        soh_questions=[dict(question="Have you ever eaten a lollipop?", answer="no")],
        coverage=10000,
        premium='10.50')
    )
    child_attachment_form.add_child(dict(
        first="Christy",
        last="Johnson",
        birthdate="12/01/2014",
        gender="female",
        ssn='444-12-4321',
        soh_questions=[dict(question="Have you ever eaten a lollipop?", answer="GI")],
        coverage=10000,
        premium='5.25')
    )

    child_attachment_form.generate()
    f = open('test.pdf', 'w+')
    f.write(child_attachment_form._pdf_data.getvalue())
    f.close()
    print("Wrote PDF to test.pdf")

    print("%s pages in document."%child_attachment_form.get_num_pages())