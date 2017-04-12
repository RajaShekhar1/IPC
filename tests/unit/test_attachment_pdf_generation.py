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
        self.test_enrollment_data.update(dict(
            emp_bene1_name="John Doe",
            emp_bene1_ssn='123-123-1234',
            emp_bene1_relationship='Friend',
            emp_bene1_birthdate='1990-01-01',
            emp_bene1_percentage=100,
        ))

        attachment = MultipleBeneficiariesAttachment(self.test_recipients,
                                                     EnrollmentDataWrap(self.test_enrollment_data, self.case),
                                                     )

        attachment.generate()
        bytes = attachment._pdf_data.getvalue()

        self.assertGreater(bytes, 0, "The attachment form did not generate any bytes")

    def test_n_beneficiaries_with_multiple_employee_beneficiary_data(self):
        self.test_enrollment_data.update(dict(
            emp_bene1_name="John Doe",
            emp_bene1_ssn='123-123-1234',
            emp_bene1_relationship='Friend',
            emp_bene1_birthdate='1990-01-01',
            emp_bene1_percentage=100,

            emp_bene2_name="Bob Bobson",
            emp_bene2_ssn='453-34-3453',
            emp_bene2_relationship='Another Friend',
            emp_bene2_birthdate='1995-01-01',
            emp_bene2_percentage=50,
        ))

        attachment = MultipleBeneficiariesAttachment(self.test_recipients,
                                                     EnrollmentDataWrap(self.test_enrollment_data, self.case),
                                                     )

        attachment.generate()
        bytes = attachment._pdf_data.getvalue()

        self.assertGreater(bytes, 0, "The attachment form did not generate any bytes")

    def test_n_beneficiaries_with_other_beneficiary_data(self):
        self.test_enrollment_data.update(dict(
            emp_bene1_name="John Doe",
            emp_bene1_ssn='123-123-1234',
            emp_bene1_relationship='Friend',
            emp_bene1_birthdate='1990-01-01',
            emp_bene1_percentage=100,

            emp_cont_bene1_name="John Doe",
            emp_cont_bene1_ssn='123-123-1234',
            emp_cont_bene1_relationship='Friend',
            emp_cont_bene1_birthdate='1990-01-01',
            emp_cont_bene1_percentage=100,


            sp_bene1_name="John Doe",
            sp_bene1_ssn='123-123-1234',
            sp_bene1_relationship='Friend',
            sp_bene1_birthdate='1990-01-01',
            sp_bene1_percentage=100,

            sp_cont_bene1_name="John Doe",
            sp_cont_bene1_ssn='123-123-1234',
            sp_cont_bene1_relationship='Friend',
            sp_cont_bene1_birthdate='1990-01-01',
            sp_cont_bene1_percentage=100,
        ))

        attachment = MultipleBeneficiariesAttachment(self.test_recipients,
                                                     EnrollmentDataWrap(self.test_enrollment_data, self.case),
                                                     )

        attachment.generate()
        bytes = attachment._pdf_data.getvalue()

        self.assertGreater(bytes, 0, "The attachment form did not generate any bytes")

    def test_multiple_children_attachment(self):
        child_attachment_form = ChildAttachmentForm(self.test_recipients,
                                                    enrollment_data=EnrollmentDataWrap(dict(
                                                        agent_data=dict(company_name="DelMar SD"),
                                                        employee=dict(first="Test", last="Employee", ssn="123-12-1234"),
                    product_id=1
                                                    ), self.case)
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
            ],
                product_id=1
        ), self.case))

        attachment.generate()
        bytes = attachment._pdf_data.getvalue()

        self.assertGreater(bytes, 0, "The attachment form did not generate any bytes")

    def setUp(self):

        agent = AgentDocuSignRecipient(agent=None, name="Zachary Mason", email="zmason@delmarsd.com")
        employee = EmployeeDocuSignRecipient(name="Joe Tester", email="zach@zachmason.com")
        self.test_recipients = [
            agent,
            employee,
        ]
        self.test_enrollment_data = dict(
            agent_data=dict(company_name="DelMar SD"),
            employee=dict(first="Test", last="Employee", ssn="123-12-1234"),
            product_id=1
        )
        self.case = Mock(company_name="DelMar SD")