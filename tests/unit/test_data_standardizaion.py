from unittest2 import TestCase
import csv
from hamcrest import assert_that, equal_to

from taa.services.enrollments.enrollment_import import EnrollmentImportService, EnrollmentRecordParser

class TestDataStandardization(TestCase):
    def setUp(self):
        pass

    def test_it_should_standardize_dict_passed_in(self):
        init_data = {}
        for field in EnrollmentRecordParser.all_fields:
            init_data[field.dict_key_name] = ""
        import_service = EnrollmentImportService()
        output = import_service.standardize_imported_data(init_data)
