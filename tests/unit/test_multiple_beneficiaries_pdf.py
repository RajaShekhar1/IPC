from mock import Mock
from unittest2 import TestCase

from taa.services.docusign.documents.multiple_beneficiaries_attachment import MultipleBeneficiariesAttachment
from taa.services.docusign.service import AgentDocuSignRecipient, EmployeeDocuSignRecipient
from taa.services.docusign.docusign_envelope import EnrollmentDataWrap

class TestBeneficiariesPDFAttachment(TestCase):

    def test_it_generates_pdf_with_single_employee_beneficiary_data(self):
        beneficiaries = {'employee_primary':[
            dict(
                name="John Doe",
                ssn='123-123-1234',
                relationship='Friend',
                birthdate='1990-01-01',
                percentage=100,
            ),
        ]}

        attachment = MultipleBeneficiariesAttachment(self.test_recipients,
                                                     EnrollmentDataWrap(self.test_enrollment_data, None, self.case),
                                                     beneficiaries)

        attachment.generate()
        bytes = attachment._pdf_data.getvalue()

        self.assertGreater(bytes, 0, "The attachment form did not generate any bytes")

        #f = open('test.pdf', 'w+')
        #f.write(bytes)
        #f.close()

    def test_it_generates_pdf_with_multiple_employee_beneficiary_data(self):
        beneficiaries = {'employee_primary':[
            dict(
                name="John Doe",
                ssn='123-123-1234',
                relationship='Friend',
                birthdate='1990-01-01',
                percentage=100,
            ),
            dict(
                name="Bob Bobson",
                ssn='453-34-3453',
                relationship='Another Friend',
                birthdate='1995-01-01',
                percentage=50,
            ),
        ]}

        attachment = MultipleBeneficiariesAttachment(self.test_recipients,
                                                     EnrollmentDataWrap(self.test_enrollment_data, None, self.case),
                                                     beneficiaries,
                                                     )

        attachment.generate()
        bytes = attachment._pdf_data.getvalue()

        self.assertGreater(bytes, 0, "The attachment form did not generate any bytes")



    def test_it_has_multiple_pages_with_lots_of_beneficiaries(self):
        pass
        # attachment.get_num_pages()

    def setUp(self):


        agent = AgentDocuSignRecipient(name="Zachary Mason", email="zmason@delmarsd.com")
        employee = EmployeeDocuSignRecipient(name="Joe Tester", email="zach@zachmason.com")
        self.test_recipients = [
            agent,
            employee,
        ]
        self.test_enrollment_data = dict(
            agent_data=dict(company_name="DelMar SD"),
            employee=dict(first="Test", last="Employee", ssn="123-12-1234")
        )
        self.case = Mock(company_name="DelMar SD")