from decimal import Decimal
from unittest2 import TestCase

from hamcrest import assert_that, has_entries
from mock import Mock

from taa.services.enrollments.enrollment_import import EnrollmentImportService


class TestDataStandardization(TestCase):
    def setUp(self):
        # Set up the service and stub out it's dependencies.
        self.import_service = EnrollmentImportService()
        self.import_service.product_service = self.get_mock_product_service()
        self.import_service.soh_service = self.get_mock_soh_service()

        self.init_data = dict(
            user_token='ABC',
            case_token='XYZ',
            product_code='FPPTI',
            payment_mode='52',
            emp_first='Joe',
            emp_last='Johnson',
            emp_gender='m',
            emp_ssn='123121234',
            emp_birthdate='1980-01-31',
            emp_coverage='50000',
            emp_premium='10.00',
            emp_street='123 Sesame',
            emp_street2='',
            emp_city='Indianapolis',
            emp_state='IN',
            emp_zipcode='47999',
            emp_phone='',
            emp_pin='12341234',
            emp_sig_txt='esign by Joe Johnson',
            emp_question_1_answer='N',
            emp_question_2_answer='N',
            emp_question_3_answer='N',
            emp_question_4_answer='N',
            emp_question_5_answer='N',
            emp_question_6_answer='N',
            emp_question_7_answer='N',
            sp_question_1_answer='N',
            sp_question_2_answer='N',
            sp_question_3_answer='N',
            sp_question_4_answer='N',
            sp_question_5_answer='N',
            sp_question_6_answer='N',
            sp_question_7_answer='N',
            application_date='2015-06-01',
            time_stamp='2015-06-01 23:00:00',
            signed_at_city='Lansing',
            signed_at_state='MI',
            agent_name='Andy Agent',
            agent_code='26CODE',
            agent_sig_txt='esign by Andy Agent',
            sp_first="Jane",
            sp_last="Doe",
            sp_gender="f",
            sp_birthdate="1990-01-01",
            sp_ssn="123-33-4444",
            sp_premium="3.00",
            sp_coverage="10000",
            ch1_first="Johnny",
            ch1_last="Doe",
            ch1_gender="m",
            ch1_birthdate="2009-01-01",
            ch1_ssn="126-66-7777",
            ch1_premium="2.50",
            ch1_coverage="10000",
            ch2_first="Mary",
            ch2_last="Doe",
            ch2_gender="f",
            ch2_birthdate="2009-12-01",
            ch2_ssn="124-44-8888",
            ch2_premium="2.50",
            ch2_coverage="10000",
            actively_at_work="Y",
            emp_email="joe@gmail.com",
            emp_date_of_hire="2010-01-31",
            emp_height_inches="70",
            emp_weight_pounds="150",
            emp_smoker="N",
            sp_height_inches="65",
            sp_weight_pounds="130",
            sp_smoker="N",
            sp_street="Other st",
            sp_street2="",
            sp_city="Chicago",
            sp_state="IL",
            sp_zipcode="11444",
            sp_phone="1242223535",
            sp_email='sp@email.com',
            existing_insurance="N",
            replacing_insurance="N",
            sp_treated_6_months="N",
            sp_disabled_6_months="N",
            replacement_read_aloud="N",
            replacement_is_terminating="N",
            replacement_using_funds="N",
            replacement_policy1_name="Prudential",
            replacement_policy1_number="111AAA33",
            replacement_policy1_insured="Joe",
            replacement_policy1_replaced_or_financing="R",
            replacement_policy1_reason="Needed better coverage",
            emp_bene_name="Emp. Prim. Bene",
            emp_bene_birthdate="1990-10-10",
            emp_bene_relationship="Brother",
            emp_bene_ssn="555-55-5555",
            sp_bene_name="Sp prim bene",
            sp_bene_birthdate="1980-11-11",
            sp_bene_relationship="daughter",
            sp_bene_ssn="111-11-1112",
            emp_cont_bene_name="Emp. Cont. Bene ",
            emp_cont_bene_birthdate="1989-01-10",
            emp_cont_bene_relationship="Relative",
            emp_cont_bene_ssn="666-55-5555",
            sp_cont_bene_name="Sp cont bene",
            sp_cont_bene_birthdate="1985-11-12",
            sp_cont_bene_relationship="friend",
            sp_cont_bene_ssn="121-12-1112"
        )

        # Create a different version of the data with new beneficiary format.
        self.nbene_data = self.init_data.copy()
        for key in self.nbene_data.keys():
            if 'bene' in key:
                del self.nbene_data[key]

        self.nbene_data.update(dict(
            emp_bene1_name="Emp. Prim. Bene",
            emp_bene1_relationship="Brother",
            emp_bene1_birthdate="1990-10-10",
            emp_bene1_ssn="555-55-5555",
            emp_bene1_percentage="60",
            emp_bene2_name="Emp. Prim. Bene2",
            emp_bene2_relationship="Brother2",
            emp_bene2_birthdate="1990-10-12",
            emp_bene2_ssn="555-55-5552",
            emp_bene2_percentage="40",
            sp_bene1_name="Sp prim bene",
            sp_bene1_relationship="daughter",
            sp_bene1_birthdate="1980-11-11",
            sp_bene1_ssn="111-11-1112",
            sp_bene1_percentage="60",
            sp_bene2_name="Sp prim bene2",
            sp_bene2_relationship="daughter2",
            sp_bene2_birthdate="1980-11-12",
            sp_bene2_ssn="111-11-1113",
            sp_bene2_percentage="40",
            emp_cont_bene1_name="Emp. Cont. Bene ",
            emp_cont_bene1_relationship="Relative",
            emp_cont_bene1_birthdate="1989-01-10",
            emp_cont_bene1_ssn="666-55-5555",
            emp_cont_bene1_percentage="60",
            emp_cont_bene2_name="Emp. Cont. Bene2",
            emp_cont_bene2_relationship="Relative2",
            emp_cont_bene2_birthdate="1989-01-12",
            emp_cont_bene2_ssn="666-55-5552",
            emp_cont_bene2_percentage="40",
            sp_cont_bene1_name="Sp cont bene",
            sp_cont_bene1_relationship="friend",
            sp_cont_bene1_birthdate="1985-11-12",
            sp_cont_bene1_ssn="121-12-1112",
            sp_cont_bene1_percentage="60",
            sp_cont_bene2_name="Sp cont bene2",
            sp_cont_bene2_relationship="friend2",
            sp_cont_bene2_birthdate="1985-11-12",
            sp_cont_bene2_ssn="121-12-1113",
            sp_cont_bene2_percentage="40",
        ))

    def test_it_should_standardize_required_main_data(self):
        output = self.import_service.standardize_imported_data(self.init_data)
        expected = {
             'enrollCity': 'Lansing',
             'enrollState': 'MI',
             'existing_insurance': False,
             'has_spouse_been_disabled_6_months': 'No',
             'has_spouse_been_treated_6_months': 'No',
             'is_employee_actively_at_work': True,
             'is_spouse_address_same_as_employee': False,
             'is_spouse_email_same_as_employee': False,
             'payment_mode': 52,
             'payment_mode_text': 'weekly',
             'product_type': 'FPPTI',
        }

        # Check each key
        assert_that(output, has_entries(expected))

    def test_it_should_standardize_employee_data(self):

        output = self.import_service.standardize_imported_data(self.init_data)
        expected = {
            'address1': '123 Sesame',
            'address2': '',
            'birthdate': '1980-01-31',
            'city': 'Indianapolis',
            'first': 'Joe',
            'gender': 'male',
            'email':'joe@gmail.com',
            'height': '70',
            'is_smoker': False,
            'last': 'Johnson',
            'phone': '',
            'ssn': '123121234',
            'state': 'IN',
            'weight': '150',
            'zip': '47999',
            'soh_questions': [{'answer': 'No', 'question':self.mock_question_text}],
        }
        assert_that(output.get('employee'), has_entries(expected))

    def test_it_should_standardize_spouse_info(self):

        output = self.import_service.standardize_imported_data(self.init_data)
        expected = {
            'address1': 'Other st',
            'address2': '',
            'birthdate': '1990-01-01',
            'city': 'Chicago',
            'first': 'Jane',
            'gender': 'female',
            'height': '65',
            'is_smoker': False,
            'last': 'Doe',
            'phone': '1242223535',
            'email':'sp@email.com',
            'ssn': '123-33-4444',
            'state': 'IL',
            'weight': '130',
            'zip': '11444'}

        assert_that(output['spouse'], has_entries(expected))

    def test_it_should_standardize_children_data(self):

        output = self.import_service.standardize_imported_data(self.init_data)

        expected = [{
            'address1': '',
            'address2': '',
            'birthdate': '2009-01-01',
            'city': '',
            'first': 'Johnny',
            'gender': 'male',
            'height': '',
            'is_smoker': False,
            'last': 'Doe',
            'phone': '',
            'ssn': '126-66-7777',
            'state': '',
            'weight': '',
            'zip': ''},
            {'address1': '',
            'address2': '',
            'birthdate': '2009-12-01',
            'city': '',
            'first': 'Mary',
            'gender': 'female',
            'height': '',
            'is_smoker': False,
            'last': 'Doe',
            'phone': '',
            'ssn': '124-44-8888',
            'state': '',
            'weight': '',
            'zip': ''}
        ]
        assert_that(output['children'][0], has_entries(expected[0]))
        assert_that(output['children'][1], has_entries(expected[1]))

    def test_it_should_standardize_coverages(self):

        output = self.import_service.standardize_imported_data(self.init_data)
        expected = {
            'child_coverages': [
                     {'face_value': 10000, 'premium': Decimal('2.50')},
                     {'face_value': 10000, 'premium': Decimal('2.50')},
            ],
            'employee_coverage': {'face_value': 50000, 'premium': Decimal('10.00')},
            'spouse_coverage': {'face_value': 10000, 'premium': Decimal('3.00')}
        }
        assert_that(output, has_entries(expected))

    def test_it_should_standardize_replacement_data(self):
        output = self.import_service.standardize_imported_data(self.init_data)
        expected = {
            'replacement_is_terminating': False,
            'replacement_policies': [{'insured': 'Joe',
                                   'name': 'Prudential',
                                   'policy_number': '111AAA33',
                                   'replaced_or_financing': 'R',
                                   'replacement_reason': 'Needed better coverage'}],
            'replacement_read_aloud': False,
            'replacement_using_funds': False,
            'replacing_insurance': False,
        }
        assert_that(output, has_entries(expected))

    def test_it_should_add_identityToken(self):
        output = self.import_service.standardize_imported_data(self.init_data)
        expected = {
            'identityToken': self.init_data['emp_date_of_hire']
        }
        assert_that(output, has_entries(expected))

    def test_it_should_add_beneficiaries(self):
        input = self.nbene_data
        output = self.import_service.standardize_imported_data(input)
        expected = {
            'employee_beneficiary': 'other',
            'spouse_beneficiary': 'other',
            'employee_beneficiary1_name': input['emp_bene1_name'],
            'employee_beneficiary1_relationship': input['emp_bene1_relationship'],
            'employee_beneficiary1_dob': input['emp_bene1_birthdate'],
            'employee_beneficiary1_ssn': input['emp_bene1_ssn'],
            'employee_beneficiary1_percentage': input['emp_bene1_percentage'],
            'spouse_beneficiary1_name': input['sp_bene1_name'],
            'spouse_beneficiary1_relationship': input['sp_bene1_relationship'],
            'spouse_beneficiary1_dob': input['sp_bene1_birthdate'],
            'spouse_beneficiary1_ssn': input['sp_bene1_ssn'],
            'spouse_beneficiary1_percentage': input['sp_bene1_percentage'],

            'employee_contingent_beneficiary_type': 'other',
            'spouse_contingent_beneficiary_type': 'other',
            'employee_contingent_beneficiary1_name': input['emp_cont_bene1_name'],
            'employee_contingent_beneficiary1_relationship': input['emp_cont_bene1_relationship'],
            'employee_contingent_beneficiary1_dob': input['emp_cont_bene1_birthdate'],
            'employee_contingent_beneficiary1_ssn': input['emp_cont_bene1_ssn'],
            'employee_contingent_beneficiary1_percentage': input['emp_cont_bene1_percentage'],
            'spouse_contingent_beneficiary1_name': input['sp_cont_bene1_name'],
            'spouse_contingent_beneficiary1_relationship': input['sp_cont_bene1_relationship'],
            'spouse_contingent_beneficiary1_dob': input['sp_cont_bene1_birthdate'],
            'spouse_contingent_beneficiary1_ssn': input['sp_cont_bene1_ssn'],
            'spouse_contingent_beneficiary1_percentage': input['sp_cont_bene1_percentage'],
        }
        assert_that(output, has_entries(expected))

    def test_it_should_add_beneficiaries_legacy(self):
        output = self.import_service.standardize_imported_data(self.init_data)
        expected = {
            'employee_beneficiary': 'other',
            'spouse_beneficiary': 'other',
            'employee_beneficiary1_name': self.init_data['emp_bene_name'],
            'employee_beneficiary1_relationship': self.init_data['emp_bene_relationship'],
            'employee_beneficiary1_dob': self.init_data['emp_bene_birthdate'],
            'employee_beneficiary1_ssn': self.init_data['emp_bene_ssn'],
            'employee_beneficiary1_percentage': 100,
            'spouse_beneficiary1_name': self.init_data['sp_bene_name'],
            'spouse_beneficiary1_relationship': self.init_data['sp_bene_relationship'],
            'spouse_beneficiary1_dob': self.init_data['sp_bene_birthdate'],
            'spouse_beneficiary1_ssn': self.init_data['sp_bene_ssn'],
            'spouse_beneficiary1_percentage': 100,

            'employee_contingent_beneficiary_type': 'other',
            'spouse_contingent_beneficiary_type': 'other',
            'employee_contingent_beneficiary1_name': self.init_data['emp_cont_bene_name'],
            'employee_contingent_beneficiary1_relationship': self.init_data['emp_cont_bene_relationship'],
            'employee_contingent_beneficiary1_dob': self.init_data['emp_cont_bene_birthdate'],
            'employee_contingent_beneficiary1_ssn': self.init_data['emp_cont_bene_ssn'],
            'employee_contingent_beneficiary1_percentage': 100,
            'spouse_contingent_beneficiary1_name': self.init_data['sp_cont_bene_name'],
            'spouse_contingent_beneficiary1_relationship': self.init_data['sp_cont_bene_relationship'],
            'spouse_contingent_beneficiary1_dob': self.init_data['sp_cont_bene_birthdate'],
            'spouse_contingent_beneficiary1_ssn': self.init_data['sp_cont_bene_ssn'],
            'spouse_contingent_beneficiary1_percentage': 100,
        }
        assert_that(output, has_entries(expected))


    def test_it_standardizes_wizard_contingent_beneficiaries(self):
        self.wizard_data = {
            'employee_contingent_beneficiary_type': 'other',
            'spouse_contingent_beneficiary_type': 'other',
            'employee_contingent_beneficiary': dict(
                name='Joe',
                relationship='Friend',
                date_of_birth='12/12/2012',
                ssn='123-12-1234',
            ),
            'spouse_contingent_beneficiary': dict(
                name='Jane',
                relationship='Friend2',
                date_of_birth='11/11/2011',
                ssn='333-22-1234',
            ),
        }
        input_emp_cont = self.wizard_data['employee_contingent_beneficiary']
        input_sp_cont = self.wizard_data['spouse_contingent_beneficiary']

        expected = {
            'employee_contingent_beneficiary1_name': input_emp_cont['name'],
            'employee_contingent_beneficiary1_relationship': input_emp_cont['relationship'],
            'employee_contingent_beneficiary1_dob': input_emp_cont['date_of_birth'],
            'employee_contingent_beneficiary1_ssn': input_emp_cont['ssn'],

            'spouse_contingent_beneficiary1_name': input_sp_cont['name'],
            'spouse_contingent_beneficiary1_relationship': input_sp_cont['relationship'],
            'spouse_contingent_beneficiary1_dob': input_sp_cont['date_of_birth'],
            'spouse_contingent_beneficiary1_ssn': input_sp_cont['ssn'],
        }

        output = self.import_service.standardize_wizard_data(self.wizard_data)

        assert_that(output, has_entries(expected))

    def get_mock_product_service(self):
        mock_product_service = Mock()
        self.mock_product = Mock()
        self.mock_product.get_base_product_code.return_value = 'FPPTI'
        mock_product_service.get_products_by_codes.return_value = [self.mock_product]
        return mock_product_service

    def get_mock_soh_service(self):
        mock_soh_service = Mock()
        self.mock_question = Mock()
        self.mock_question_text = self.mock_question.question
        mock_soh_service.get_health_questions.return_value = [self.mock_question]
        return mock_soh_service
