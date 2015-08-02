import re

from reportlab.platypus import SimpleDocTemplate, Paragraph, Spacer, Table, TableStyle
from reportlab.lib.styles import getSampleStyleSheet, ParagraphStyle, _baseFontNameB
from reportlab.lib.units import inch, mm
from reportlab.pdfgen.canvas import Canvas
from reportlab.graphics.shapes import Line, String
from reportlab.lib import colors
from reportlab.graphics.renderPDF import Drawing

from taa.services.docusign.service import (
    BasePDFDoc,
    DocuSignSigTab,
)

from utils import style, bold_style2, NumberedCanvas, create_attachment_header, create_signature_line


class AdditionalReplacementPoliciesForm(BasePDFDoc):
    def __init__(self, recipients, enrollment_data):
        BasePDFDoc.__init__(self, recipients)

        self.data = enrollment_data


        # Put all the policies on this form if we have 2 or more
        self.policies = [policy for policy in self.data['replacement_policies']]

        # Map recipient signers to the coords of the signature line so we can attach tabs to those lines.
        self.sig_coords = {}

    def generate(self):
        flowables = []

        flowables += self.draw_header()

        flowables += self.draw_policies_table()

        # Signature line and name
        flowables += self.draw_signature_line()

        # Generate the document using reportlab's PLATYPUS layout api.
        self._doc.build(flowables, canvasmaker=NumberedCanvas)

    def get_spacer(self, size=.2 * inch):
        return Spacer(0, size)

    def draw_header(self):
        return create_attachment_header(u"<u>Supplemental Form:  Additional Replacement Policies</u>", self.data)

    def draw_policies_table(self):
        flowables = [
            self.get_spacer(),
            Paragraph("Additional Policy Information", bold_style2),
        ]

        table_style = TableStyle([
            # Put a box around each cell
             ('GRID', (0,0), (-1,-1), 0.25, colors.black),
            # Align first column
            # ('ALIGN', (0,1), (0,-1), 'CENTER'),
            # Align Gender col
            # ('ALIGN', (3,1), (3,-1), 'CENTER'),
            # Right-Align money columns
            # ('ALIGN', (5,1), (5,-1), 'RIGHT'),
            # ('ALIGN', (6,1), (6,-1), 'RIGHT'),
        ])

        table_data = []

        headers = [
            "Insurer Name",
            Paragraph("Contract or Policy Number", style),
            "Insured",
            Paragraph("Replaced (R) or Financing (F)", style),
            "Reason for Replacement",
        ]
        table_data.append(headers)

        for num, policy in enumerate(self.policies):
            row = [
                Paragraph(policy['name'], style),
                Paragraph(policy['policy_number'], style),
                Paragraph(policy['insured'], style),
                Paragraph(policy['replaced_or_financing'], style),
                Paragraph(policy['replacement_reason'], style),
            ]
            table_data.append(row)

        flowables += [
            Table(table_data, style=table_style, hAlign='LEFT', colWidths=[
                1.25*inch,
                1.25*inch,
                1.25*inch,
                1*inch,
                2.5*inch
            ])
        ]

        return flowables

    def draw_signature_line(self):
        return create_signature_line(self.page_width, self.sig_coords, self.get_signer_recipients())

    def generate_tabs(self, recipient):
        tabs = super(AdditionalReplacementPoliciesForm, self).generate_tabs(recipient)

        if self.is_recipient_signer(recipient):
            # Add a signature tab to the last page

            pdf_x, pdf_y = self.sig_coords[recipient.name]
            pix_x = pdf_x

            pix_y = (self.page_height - pdf_y)
            #print("Converted %s, %s to %s %s"%(pdf_x, pdf_y, pix_x, pix_y))
            tab = DocuSignSigTab(x=pix_x, y=pix_y, document_id="1", page_number=str(self.get_num_pages()))
            tabs.append(tab)

        return tabs

    def is_recipient_signer(self, recipient):
        return recipient.is_employee() # or recipient.is_agent()

    def get_signer_recipients(self):
        return [r for r in self.recipients if r.is_employee()]




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

    form = AdditionalReplacementPoliciesForm(test_recipients, enrollment_data=EnrollmentDataWrap(dict(
        agent_data=dict(company_name="DelMar SD"),
        employee=dict(first="Test", last="Employee", ssn="123-12-1234")
    ), None, None), additional_policies=[dict(
        name="Test Policy",
        policy_number="123415",
        insured="Joe Johnson",
        replaced_or_financing="R",
        replacement_reason="Testing Reason ntahoensuthansoe hunsao a oeunaoenu hane unaoe a asoe unaeu",
        )])

    form.generate()
    f = open('test.pdf', 'w+')
    f.write(form._pdf_data.getvalue())
    f.close()
    print("Wrote PDF to test.pdf")

    print("%s pages in document."%form.get_num_pages())