from mock import Mock
from taa.services.docusign.documents.additional_replacement_policies import AdditionalReplacementPoliciesForm
from taa.services.docusign.documents.additional_children import ChildAttachmentForm
from unittest2 import TestCase

from taa.services.docusign.documents.multiple_beneficiaries_attachment import MultipleBeneficiariesAttachment
from taa.services.docusign.service import AgentDocuSignRecipient, EmployeeDocuSignRecipient
from taa.services.docusign.docusign_envelope import EnrollmentDataWrap

class TestPDFAttachments(TestCase):
    """
    These just exercise the various attachment PDF generators with some test data to make sure
    they don't crash. There is no validation of the data in the PDF.
    """
    def test_n_beneficiaries_with_single_employee_beneficiary_data(self):
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

    def test_n_beneficiaries_with_multiple_employee_beneficiary_data(self):
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

    def test_n_beneficiaries_with_other_beneficiary_data(self):
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
        ],
        "employee_contingent":[dict(name="Bob", ssn='123-22-1234', relationship="son", percentage=100, birthdate='')],
        "spouse_primary":[dict(name="Bob", ssn="", relationship="Mom", percentage=100, birthdate='')],
        "spouse_contingent":[dict(name="Joe", ssn="", relationship="random", percentage=100, birthdate='')]}

        attachment = MultipleBeneficiariesAttachment(self.test_recipients,
                                                     EnrollmentDataWrap(self.test_enrollment_data, None, self.case),
                                                     beneficiaries,
                                                     )

        attachment.generate()
        bytes = attachment._pdf_data.getvalue()

        self.assertGreater(bytes, 0, "The attachment form did not generate any bytes")

    def test_multiple_children_attachment(self):
        child_attachment_form = ChildAttachmentForm(self.test_recipients,
                                                    enrollment_data=EnrollmentDataWrap(dict(
                                                        agent_data=dict(company_name="DelMar SD"),
                                                        employee=dict(first="Test", last="Employee", ssn="123-12-1234")
                                                        ), None, self.case
                                                    )
        )

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
        bytes = child_attachment_form._pdf_data.getvalue()

        self.assertGreater(bytes, 0, "The attachment form did not generate any bytes")

    def test_multiple_replacement_policies_attachment(self):
        attachment = AdditionalReplacementPoliciesForm(self.test_recipients, enrollment_data=EnrollmentDataWrap(dict(
            agent_data=dict(company_name="DelMar SD"),
            employee=dict(first="Test", last="Employee", ssn="123-12-1234"),
            replacement_policies=[
                dict(
                    name="Test Policy",
                    policy_number="123415",
                    insured="Joe Johnson",
                    replaced_or_financing="R",
                    replacement_reason="Testing Reason ntahoensuthansoe hunsao a oeunaoenu hane unaoe a asoe unaeu",
                )
            ]
        ), None, self.case))

        attachment.generate()
        bytes = attachment._pdf_data.getvalue()

        self.assertGreater(bytes, 0, "The attachment form did not generate any bytes")

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