from unittest2 import TestCase
import csv
from hamcrest import assert_that, equal_to

from taa.services.enrollments.enrollment_import import EnrollmentImportService, EnrollmentRecordParser

class TestDataStandardization(TestCase):
    def setUp(self):
        pass

    def test_it_should_standardize_dict_passed_in(self):
        init_data = dict(
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
        )
        import_service = EnrollmentImportService()
        output = import_service.standardize_imported_data(init_data)
