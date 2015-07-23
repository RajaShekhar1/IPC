from unittest2 import TestCase
import csv
from hamcrest import assert_that, equal_to

from taa.services.data_import.file_import import FlatFileImporter, FlatFileDocumentation, FlatFileFieldDefinition, FlatFileImportResult, FileImportService

class TestFlatFile(TestCase):
    def setUp(self):
        pass

    def test_it_should_return_a_dictionary_from_a_single_field(self):
        flat_file_spec = [
            FlatFileFieldDefinition(
                size=16,
                csv_name="auth_token",
                title="User API Authorization Token",
                description="",
            ),
        ]
        flat_file_importer = FlatFileImporter(flat_file_spec)

        result = flat_file_importer.import_data("ABCDEFGHIJKLMNOP")

        expected = [
            {'auth_token': "ABCDEFGHIJKLMNOP"}
        ]
        assert_that(result.get_data(), equal_to(expected))

    def test_it_should_parse_multiple_columns(self):
        flat_file_spec = [
            FlatFileFieldDefinition(
                size=16,
                csv_name="auth_token",
                title="User API Authorization Token",
                description="",
            ),
            FlatFileFieldDefinition(
                size=1,
                csv_name="actively_at_work",
                title="Is the employee actively at work?",
                description="",
            ),
        ]

        flat_file_importer = FlatFileImporter(flat_file_spec)

        result = flat_file_importer.import_data("1234123412341234Y")

        expected = [
            {'auth_token': "1234123412341234", 'actively_at_work':'Y'},
        ]
        assert_that(result.get_data(), equal_to(expected))

    def test_it_should_parse_multiple_records(self):
        flat_file_spec = [
            FlatFileFieldDefinition(
                size=16,
                csv_name="auth_token",
                title="User API Authorization Token",
                description="",
            ),
            FlatFileFieldDefinition(
                size=1,
                csv_name="actively_at_work",
                title="Is the employee actively at work?",
                description="",
            ),
        ]
        flat_file_importer = FlatFileImporter(flat_file_spec)

        result = flat_file_importer.import_data("1234123412341234Y\nABCDEFGHIJKLMNOPN")

        expected = [
            {'auth_token': "1234123412341234", 'actively_at_work':'Y'},
            {'auth_token': "ABCDEFGHIJKLMNOP", 'actively_at_work':'N'},
        ]
        assert_that(result.get_data(), equal_to(expected))

    def test_it_should_generate_basic_documentation(self):
        flat_file_spec = [
            FlatFileFieldDefinition(
                size=16,
                csv_name="auth_token",
                title="User API Authorization Token",
                description="",
            ),
            FlatFileFieldDefinition(
                size=1,
                csv_name="actively_at_work",
                title="Is the employee actively at work?",
                description="",
            ),
        ]

        documentation = FlatFileDocumentation(flat_file_spec)

        expected = """\
Field,From,To,Length,Description
auth_token,1,16,16,
actively_at_work,17,17,1,
"""

        assert_that(documentation.toCSV(), equal_to(expected))

    def test_it_should_generate_a_flat_file_spec_based_on_enrollment_records(self):
        importer = FileImportService()
        spec = importer.get_flat_file_spec()
        documentation = FlatFileDocumentation(spec)
        print(documentation.toCSV())
