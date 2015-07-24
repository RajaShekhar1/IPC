from unittest2 import TestCase
import csv

from hamcrest import assert_that, equal_to, has_entries, contains
from mock import Mock, sentinel

from taa.services.enrollments.enrollment_import import EnrollmentImportService, EnrollmentRecordParser

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
            payment_mode='weekly',
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

    def test_it_should_standardize_required_main_data(self):
        output = self.import_service.standardize_imported_data(self.init_data)
        expected = {
             'enrollCity': 'Lansing',
             'enrollState': 'MI',
             'existing_insurance': False,
             'has_spouse_been_disabled_6_months': False,
             'has_spouse_been_treated_6_months': False,
             'is_employee_actively_at_work': True,
             'is_spouse_address_same_as_employee': False,
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
            'gender': 'm',
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
            'gender': 'f',
            'height': '65',
            'is_smoker': False,
            'last': 'Doe',
            'phone': '1242223535',
            'ssn': '123-33-4444',
            'state': 'IL',
            'weight': '130',
            'zip': '11444'}

        assert_that(output['spouse'], has_entries(expected))

    def test_it_should_standardize_children_data(self):

        output = self.import_service.standardize_imported_data(self.init_data)

        expected = [{
            'address1': None,
            'address2': None,
            'birthdate': '2009-01-01',
            'city': None,
            'first': 'Johnny',
            'gender': 'm',
            'height': None,
            'is_smoker': False,
            'last': 'Doe',
            'phone': None,
            'ssn': '126-66-7777',
            'state': None,
            'weight': None,
            'zip': None},
            {'address1': None,
            'address2': None,
            'birthdate': '2009-12-01',
            'city': None,
            'first': 'Mary',
            'gender': 'f',
            'height': None,
            'is_smoker': False,
            'last': 'Doe',
            'phone': None,
            'ssn': '124-44-8888',
            'state': None,
            'weight': None,
            'zip': None}
        ]
        assert_that(output['children'][0], has_entries(expected[0]))
        assert_that(output['children'][1], has_entries(expected[1]))


    def test_it_should_standardize_coverages(self):

        output = self.import_service.standardize_imported_data(self.init_data)
        expected = {
            'child_coverages': [
                     {'face_value': '10000', 'premium': '2.50'},
                     {'face_value': '10000', 'premium': '2.50'},
            ],
            'employee_coverage': {'face_value': '50000', 'premium': '10.00'},
            'spouse_coverage': {'face_value': '10000', 'premium': '3.00'}
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

    def get_mock_product_service(self):
        mock_product_service = Mock()
        mock_product_service.get_products_by_codes.return_value = [sentinel.product]
        return mock_product_service

    def get_mock_soh_service(self):
        mock_soh_service = Mock()
        self.mock_question = Mock()
        self.mock_question_text = self.mock_question.question
        mock_soh_service.get_health_questions.return_value = [self.mock_question]
        return mock_soh_service