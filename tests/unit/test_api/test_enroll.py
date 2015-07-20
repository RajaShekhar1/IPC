import unittest2

from mock import Mock, sentinel, patch
from hamcrest import assert_that, equal_to, has_items

from taa import app as taa_app
from taa.services.enrollments.enrollment_import_processor import EnrollmentProcessor

class TestEnrollmentProcessor(unittest2.TestCase):

    def setUp(self):
        taa_app.config['TESTING'] = True
        self.app = taa_app.test_client()

        self.url = '/enrollments'

    def tearDown(self):
        pass

    # def test_it_detects_third_party_request_from_auth_token(self):
    #
    #     resp = self.app.get(self.url)
    #
    #     #self.enrollment_processor.process_enrollment_import_request(auth_token=sentinel.token)
    #
    #     assert_that(self.enrollment_processor(), equal_to(True))


    def _create_sample_valid_enrollment(self):
        enrollment = {}

        enrollment.update(self._create_sample_valid_employee())

        return enrollment

    def _create_sample_valid_base_data(self):
        return dict(
            product_code='FPPTI',
            payment_mode='monthly',

        )

    def _create_sample_valid_employee(self):
        return dict(
            emp_first='Joe',
            emp_last='Johnson',
            emp_birthdate='1970-01-01',
            emp_gender='m',
            emp_ssn='111-11-1111',
            emp_coverage='10000',
            emp_premium='10.00',
        )




