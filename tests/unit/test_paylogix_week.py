from unittest2 import TestCase
import csv, cStringIO
from hamcrest import assert_that, equal_to, starts_with

from taa.services.enrollments.paylogix import get_deduction_week



class TestPaylogixDate(TestCase):
    
    def test_it_checks_first_week(self):
        file_obj = cStringIO.StringIO("{}\nJoe     ".format(self.headers))
        result = self.file_import_service.process_flat_file_stream(file_obj, spec=self.single_spec)
        expected = [
            {'user_token': 'e471d02990094e95b76ea096f0814783', 'case_token': 'CASE-123', 'emp_first': "Joe"}
        ]
        assert_that(result.get_data(), equal_to(expected))
