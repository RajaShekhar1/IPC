
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
        return create_attachment_header(u"PUT HEADER HERE", self.data)

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

    beneficiaries = {
        'employee_primary':[
            dict(
                name="John Doe",
                ssn='123-123-1234',
                relationship='Friend',
                birthdate='1990-01-01',
                percentage=100,
            ),
            dict(
                name="Frank Doe",
                ssn='123-123-1234',
                relationship=None,
                birthdate='1940-01-01',
                percentage=10,
            ),
        ],
        'employee_contingent':[
            dict(
                name="Jimmy Doe",
                ssn='123-123-1234',
                relationship='Child',
                birthdate='1940-01-01',
                percentage=34,
            ),
            dict(
                name="Suzie Doe",
                ssn='123-123-1234',
                relationship='Child',
                birthdate='1940-01-01',
                percentage=33,
            ),
            dict(
                name="Frank Doe Jr",
                ssn='123-123-1234',
                relationship='Child',
                birthdate='1940-01-01',
                percentage=33,
            ),
        ],
        'spouse_primary':[
            dict(
                name="Jane Doe",
                ssn='123-123-1234',
                relationship='Spouse',
                birthdate='1940-01-01',
                percentage=10,
            ),
            dict(
                name="Janet Doe",
                ssn='123-123-1234',
                relationship='Mother',
                birthdate='1940-01-01',
                percentage=10,
            ),
        ],
        'spouse_contingent':[
            dict(
                name="Jimmy Doe",
                ssn='123-123-1234',
                relationship='Child',
                birthdate='1940-01-01',
                percentage=34,
            ),
            dict(
                name="Suzie Doe",
                ssn='123-123-1234',
                relationship='Child',
                birthdate='1940-01-01',
                percentage=33,
            ),
            dict(
                name="Frank Doe Jr",
                ssn='123-123-1234',
                relationship='Child',
                birthdate='1940-01-01',
                percentage=33,
            ),
        ],
    }

    attachment = MultipleBeneficiariesAttachment(test_recipients,
                                                 EnrollmentDataWrap(dict(agent_data=dict(company_name="DelMar SD"),
        employee=dict(first="Test", last="Employee", ssn="123-12-1234")), None, case),
                                                 beneficiaries)
    attachment.generate()
    bytes = attachment._pdf_data.getvalue()

    f = open('test.pdf', 'w+')
    f.write(bytes)
    f.close()
