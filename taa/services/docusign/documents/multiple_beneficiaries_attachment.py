
from reportlab.platypus import Paragraph, Spacer, Table, TableStyle
from reportlab.lib.units import inch
from reportlab.lib import colors

from utils import style, bold_style2, NumberedCanvas, create_attachment_header, create_signature_line
from taa.services.docusign.service import (
    BasePDFDoc,
    DocuSignSigTab,
    DocuSignTextTab)
from PDFAttachment import PDFAttachment


class MultipleBeneficiariesAttachment(PDFAttachment):
    def __init__(self, recipients, enrollment_data):
        PDFAttachment.__init__(self, recipients, enrollment_data)

        self.beneficiaries = enrollment_data.get_beneficiary_data()

    def generate(self):
        flowables = []

        flowables += self.draw_header()

        flowables += self.draw_beneficiaries_tables()

        # Signature line and name
        flowables += self.draw_signature_line()

        # Generate the document using reportlab's PLATYPUS layout api.
        self._doc.build(flowables, canvasmaker=NumberedCanvas)

    def draw_header(self):
        return create_attachment_header(u"Supplemental Form:  Beneficiary Information", self.data)

    def draw_beneficiaries_tables(self):
        flowables = []

        beneficiary_tables = [
            dict(
                key='employee_primary',
                heading='Employee Beneficiary',
                subheading='Primary Employee Beneficiary Class'
            ),
            dict(
                key='employee_contingent',
                heading=None,
                subheading='Contingent Employee Beneficiary Class'
            ),
            dict(
                key='spouse_primary',
                heading='Spouse Beneficiary',
                subheading='Primary Spouse Beneficiary Class'
            ),
            dict(
                key='spouse_contingent',
                heading=None,
                subheading='Contingent Spouse Beneficiary Class'
            ),
        ]

        for table in beneficiary_tables:
            flowables += self.draw_beneficiaries_table(table)

        return flowables

    def draw_beneficiaries_table(self, table):
        flowables = []

        if self.beneficiaries.get(table['key']):
            beneficiary_data = self.beneficiaries[table['key']]
        else:
            return flowables

        beneficiary_heading = table['heading']
        beneficiary_subheading = table['subheading']

        table_style = TableStyle([
            # Put a box around each cell
             ('GRID', (0,0), (-1,-1), 0.25, colors.black),
            # Align Relationship column
             ('ALIGN', (1,1), (1,-1), 'CENTER'),
            # Right-Align Percentage column
             ('ALIGN', (4,1), (4,-1), 'RIGHT'),
        ])

        table_widths = (
            1.5*inch,
            1.2*inch,
            1.0*inch,
            1.2*inch,
            0.9*inch,
        )
        table_header = [
            [
                "Name",
                "Relationship",
                "Birth Date",
                "SSN",
                "Percentage",
            ]
        ]

        if len(beneficiary_data) > 0:
            flowables += [
                self.get_spacer(),
            ]

            if beneficiary_heading is not None:
                flowables += [
                    Paragraph(beneficiary_heading, bold_style2),
                ]

            flowables += [
                self.get_spacer(),
                Paragraph(beneficiary_subheading, style),
            ]

            table_data = table_header
            for num, beneficiary in enumerate(beneficiary_data):
                row = [
                    Paragraph(beneficiary['name'] or "", style),
                    Paragraph(beneficiary['relationship'] or "", style),
                    beneficiary['birthdate'],
                    beneficiary['ssn'],
                    beneficiary['percentage'],
                ]
                table_data.append(row)

            flowables += [
                Table(table_data, style=table_style, hAlign='LEFT', colWidths=table_widths)
            ]

        return flowables


if __name__ == '__main__':

    from taa.services.docusign.service import AgentDocuSignRecipient, EmployeeDocuSignRecipient
    from taa.services.docusign.docusign_envelope import EnrollmentDataWrap
    from mock import Mock

    case = Mock(company_name='DelMar SD')
    agent = AgentDocuSignRecipient(name="Zachary Mason", email="zmason@delmarsd.com")
    employee = EmployeeDocuSignRecipient(name="Joe Tester", email="zach@zachmason.com")
    test_recipients = [
        agent,
        employee,
    ]
    data_test = dict(agent_data=dict(company_name="DelMar SD"),
        employee=dict(first="Test", last="Employee", ssn="123-12-1234"),
        emp_bene1_name="John Doe",
        emp_bene1_ssn='123-123-1234',
        emp_bene1_relationship='Friend',
        emp_bene1_birthdate='1990-01-01',
        emp_bene1_percentage=100,

        emp_bene2_name="Frank Doe",
        emp_bene2_ssn='123-123-1234',
        emp_bene2_relationship=None,
        emp_bene2_birthdate='1940-01-01',
        emp_bene2_percentage=10,

        emp_cont_bene1_name="Jimmy Doe",
        emp_cont_bene1_ssn='123-123-1234',
        emp_cont_bene1_relationship='Child',
        emp_cont_bene1_birthdate='1940-01-01',
        emp_cont_bene1_percentage=34,


        emp_cont_bene2_name="Suzie Doe",
        emp_cont_bene2_ssn='123-123-1234',
        emp_cont_bene2_relationship='Child',
        emp_cont_bene2_birthdate='1940-01-01',
        emp_cont_bene2_percentage=33,

        emp_cont_bene3_name="Frank Doe Jr",
        emp_cont_bene3_ssn='123-123-1234',
        emp_cont_bene3_relationship='Child',
        emp_cont_bene3_birthdate='1940-01-01',
        emp_cont_bene3_percentage=33,

        sp_bene1_name="Jane Doe",
        sp_bene1_ssn='123-123-1234',
        sp_bene1_relationship='Spouse',
        sp_bene1_birthdate='1940-01-01',
        sp_bene1_percentage=10,

        sp_bene2_name="Janet Doe",
        sp_bene2_ssn='123-123-1234',
        sp_bene2_relationship='Mother',
        sp_bene2_birthdate='1940-01-01',
        sp_bene2_percentage=10,

        sp_cont_bene1_name="Jimmy Doe",
        sp_cont_bene1_ssn='123-123-1234',
        sp_cont_bene1_relationship='Child',
        sp_cont_bene1_birthdate='1940-01-01',
        sp_cont_bene1_percentage=34,

        sp_cont_bene2_name="Suzie Doe",
        sp_cont_bene2_ssn='123-123-1234',
        sp_cont_bene2_relationship='Child',
        sp_cont_bene2_birthdate='1940-01-01',
        sp_cont_bene2_percentage=33,

        sp_cont_bene3_name="Frank Doe Jr",
        sp_cont_bene3_ssn='123-123-1234',
        sp_cont_bene3_relationship='Child',
        sp_cont_bene3_birthdate='1940-01-01',
        sp_cont_bene3_percentage=33,
    )

    attachment = MultipleBeneficiariesAttachment(test_recipients,
                                                 EnrollmentDataWrap(data_test, None, case),
    )

    attachment.generate()
    bytes = attachment._pdf_data.getvalue()

    f = open('test.pdf', 'w+')
    f.write(bytes)
    f.close()
