
from reportlab.platypus import Paragraph, Table, TableStyle
from reportlab.lib.units import inch
from reportlab.lib import colors

from PDFAttachment import PDFAttachment
from taa.services.docusign.service import BasePDFDoc
from utils import style, bold_style2, NumberedCanvas, create_attachment_header


class AdditionalReplacementPoliciesForm(PDFAttachment):
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



if __name__ == "__main__":
    # Test drive the code
    pass
    # from taa.services.docusign.service import AgentDocuSignRecipient, EmployeeDocuSignRecipient
    # from taa.services.docusign.docusign_envelope import EnrollmentDataWrap
    # from mock import Mock
    #
    # case = Mock(company_name='DelMar SD')
    # agent = AgentDocuSignRecipient(name="Zachary Mason", email="zmason@delmarsd.com")
    # employee = EmployeeDocuSignRecipient(name="Joe Tester", email="zach@zachmason.com")
    # test_recipients = [
    #     agent,
    #     employee,
    # ]
    #
    # form = AdditionalReplacementPoliciesForm(test_recipients, enrollment_data=EnrollmentDataWrap(dict(
    #     agent_data=dict(company_name="DelMar SD"),
    #     employee=dict(first="Test", last="Employee", ssn="123-12-1234"),
    #     replacement_policies=[
    #         dict(
    #             name="Test Policy",
    #             policy_number="123415",
    #             insured="Joe Johnson",
    #             replaced_or_financing="R",
    #             replacement_reason="Testing Reason ntahoensuthansoe hunsao a oeunaoenu hane unaoe a asoe unaeu",
    #         )
    #     ]
    # ), None, case))
    #
    # form.generate()
    # f = open('test.pdf', 'w+')
    # f.write(form._pdf_data.getvalue())
    # f.close()
    # print("Wrote PDF to test.pdf")
    #
    # print("%s pages in document."%form.get_num_pages())