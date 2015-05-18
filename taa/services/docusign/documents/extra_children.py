
from reportlab.platypus import SimpleDocTemplate, Paragraph, Spacer, Table
from reportlab.lib.styles import getSampleStyleSheet
from reportlab.lib.units import inch, mm
from reportlab.pdfgen.canvas import Canvas
from reportlab.graphics.shapes import Line, String
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
            #self.draw_page_number(num_pages)
            Canvas.showPage(self)
        Canvas.save(self)

    def draw_page_number(self, page_count):
        self.setFont("Helvetica", 7)
        self.drawRightString(200*mm, 20*mm,
            "Page %d of %d" % (self._pageNumber, page_count))

class ChildAttachmentForm(BasePDFDoc):
    def __init__(self, recipients):
        BasePDFDoc.__init__(self, recipients)

        self.children = []

        # Map recipient signers to the coords of the signature line so we can attach tabs to those lines.
        self.sig_coords = {}

    def add_child(self, child_first, child_last, child_ssn, child_dob, child_soh_answers):
        self.children.append((child_first, child_last, child_ssn, child_dob, child_soh_answers))

    def generate(self):

        styles = getSampleStyleSheet()

        def draw_first_page(canvas, doc):
            canvas.saveState()
            # Title on top of first page
            canvas.setFont('Times-Roman', 10)
            canvas.drawCentredString(self.page_width/2.0, self.page_height - 2*inch, "Extra Children Attachment Form")
            canvas.restoreState()

        def draw_extra_pages(canvas, doc):

            canvas.saveState()
            canvas.setFont('Times-Roman', 9)
            canvas.drawString(inch, 0.75 * inch,"Page %d" % (doc.page))
            canvas.restoreState()

        story = [Spacer(1, 2*inch)]
        style = styles["Normal"]

        for child_first, child_last, child_ssn, child_dob, child_soh_answers in self.children:
            p = Paragraph( "%s %s %s %s"%(child_first, child_last, child_ssn, child_dob), style)
            story.append(p)
            story.append(Spacer(1, 0.2*inch))
            #self._canvas.drawString(inch, self.page_height - y, "%s %s %s %s"%(child_first, child_last, child_ssn, child_dob))
            #y += inch

            # Statement of Health questions
            question_data = [("Have you done this in the last 90 days?", "No"),
             ("Have you done this in the last 90 days?", "No"),
             ("Have you done this in the last 90 days?", "No"),
             ("Have you done this in the last 90 days?", "No"),
             ("Have you done this in the last 90 days?", "No"),
             ]
            story.append(Table(question_data))

        child_attachment_object = self

        class CoordSavingDrawing(object):
            def __init__(self, width, height, recipient, **kwargs):
                self.recipient = recipient

                drawing_wrap = self
                class DrawingWithCoordTracking(Drawing):
                    def drawOn(self, canvas, x, y, _sW=0):
                        # Flowable alignment adjustment
                        x = self._hAlignAdjust(x,_sW)
                        child_attachment_object.sig_coords[drawing_wrap.recipient] = (x, y + self.height)
                        print("Got coords %s %s"%(x, y))
                        Drawing.drawOn(self, canvas, x, y, _sW)

                self.drawing = DrawingWithCoordTracking(width, height, **kwargs)


        # Signature line
        signers = [r for r in self.recipients if self.is_recipient_signer(r)]
        sig_height = 1 * inch
        for recip in signers:
            story.append(Spacer(0, 1*inch))
            sig_line = Line(0, 0, 4*inch, 0, strokeColor=black)
            #sig_line.strokeColor = black
            sig_drawing_wrap = CoordSavingDrawing(self.page_width, sig_height, recip)
            sig_drawing = sig_drawing_wrap.drawing
            sig_drawing.add(String(0, sig_height, recip.name, fontSize=14, fillColor=black))
            sig_drawing.add(sig_line)
            story.append(sig_drawing)

        self._doc.build(story, onFirstPage=draw_first_page, onLaterPages=draw_extra_pages, canvasmaker=NumberedCanvas)

    def generate_tabs(self, recipient):
        tabs = {}
        #if recipient.is_employee():
        if self.is_recipient_signer(recipient):
            # Add a signature tab to the last page
            # Convert PDF coords to 72 DPI pixel coords
            #DPI = 72.0
            pdf_x, pdf_y = self.sig_coords[recipient]
            pix_x = pdf_x # * DPI
            pix_y = (self.page_height - pdf_y) #* DPI
            print("Converted %s, %s to %s %s"%(pdf_x, pdf_y, pix_x, pix_y))
            tab = DocuSignSigTab(x=pix_x, y=pix_y, document_id="1", page_number=str(self.get_num_pages()))
            tab.add_to_tabs(tabs)

        return tabs

    def is_recipient_signer(self, recipient):
        return recipient.is_employee() or recipient.is_agent()


if __name__ == "__main__":
    # Test drive the code

    from taa.services.docusign.service import AgentDocuSignRecipient, EmployeeDocuSignRecipient

    agent = AgentDocuSignRecipient(name="Zachary Mason", email="zmason@delmarsd.com")
    employee = EmployeeDocuSignRecipient(name="Joe Tester", email="zach@zachmason.com")
    test_recipients = [
        agent,
        employee,
    ]

    child_attachment_form = ChildAttachmentForm(test_recipients)
    for x in range(2):
        child_attachment_form.add_child("Joe", "Johnson", child_dob="12/01/2010", child_ssn='123-12-1234', child_soh_answers=[])
        child_attachment_form.add_child("Susie", "Johnson", child_dob="12/01/2012", child_ssn='123-12-3234', child_soh_answers=[])
        child_attachment_form.add_child("Christy", "Johnson", child_dob="12/01/2014", child_ssn='223-12-3234', child_soh_answers=[])

    child_attachment_form.generate()
    f = open('test.pdf', 'w+')
    f.write(child_attachment_form._pdf_data.getvalue())
    f.close()
    print("Wrote PDF to test.pdf")

    print("%s pages in document."%child_attachment_form.get_num_pages())