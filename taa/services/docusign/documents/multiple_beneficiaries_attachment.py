
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
    def __init__(self, recipients, enrollment_data, beneficiaries):
        PDFAttachment.__init__(self, recipients, enrollment_data)

        self.beneficiaries = beneficiaries

    def generate(self):
        flowables = []

        flowables += self.draw_header()

        flowables += self.draw_beneficiaries_table()

        # Signature line and name
        flowables += self.draw_signature_line()

        # Generate the document using reportlab's PLATYPUS layout api.
        self._doc.build(flowables, canvasmaker=NumberedCanvas)

    def draw_header(self):
        return create_attachment_header(u"PUT HEADER HERE", self.data)

    def draw_beneficiaries_table(self):
        flowables = [
            self.get_spacer(),
        ]

        # TODO: generate beneficiary tables

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
        ],
        'employee_contingent':[],
        'spouse_primary':[],
        'spouse_contingent':[],
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
