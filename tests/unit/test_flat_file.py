from unittest2 import TestCase
import csv, cStringIO
from hamcrest import assert_that, equal_to

from taa.services.data_import.file_import import FlatFileImporter, FlatFileDocumentation, FlatFileFieldDefinition, FlatFileImportResult, FileImportService

class TestFlatFile(TestCase):
    def setUp(self):
        self.file_import_service = FileImportService()

        self.single_spec = [
            FlatFileFieldDefinition(
                size=16,
                csv_name="auth_token",
                title="User API Authorization Token",
                description="",
            ),
        ]

        self.simple_spec = [
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

    def test_it_should_return_a_dictionary_from_a_single_field(self):
        file_obj = cStringIO.StringIO("ABCDEFGHIJKLMNOP")
        result = self.file_import_service.process_flat_file_stream(file_obj, self.single_spec)
        expected = [
            {'auth_token': "ABCDEFGHIJKLMNOP"}
        ]
        assert_that(result.get_data(), equal_to(expected))

    def test_it_should_parse_multiple_columns(self):
        file_obj = cStringIO.StringIO("1234123412341234Y")
        result = self.file_import_service.process_flat_file_stream(file_obj, self.simple_spec)
        expected = [
            {'auth_token': "1234123412341234", 'actively_at_work':'Y'},
        ]
        assert_that(result.get_data(), equal_to(expected))

    def test_it_should_return_an_error_when_a_line_is_too_short(self):
        file_obj = cStringIO.StringIO("1234123412341Y")
        result = self.file_import_service.process_flat_file_stream(file_obj, self.simple_spec)
        expected = [
            "Line 1: Expected a line 17 characters long. Recieved a line 14 characters long."
        ]
        assert_that(result.get_errors(), equal_to(expected))

    def test_it_should_return_an_error_when_one_line_is_too_short_and_many_are_submitted(self):
        file_obj = cStringIO.StringIO("1234123412341Y\n1234123412341234N")
        result = self.file_import_service.process_flat_file_stream(file_obj, self.simple_spec)
        expected = [
            "Line 1: Expected a line 17 characters long. Recieved a line 14 characters long.",
        ]
        assert_that(result.get_errors(), equal_to(expected))

    def test_it_should_submit_a_flat_file_for_testing(self):
        with open("tests/data/minimal_data.flat", "r+") as f:
            result = self.file_import_service.process_flat_file_stream(f)
        # this needs an actual test written

    def test_it_should_return_an_error_when_multiple_lines_are_too_short(self):
        file_obj = cStringIO.StringIO("1234123412341Y\n123412341234123N")
        result = self.file_import_service.process_flat_file_stream(file_obj, self.simple_spec)
        expected = [
            "Line 1: Expected a line 17 characters long. Recieved a line 14 characters long.",
            "Line 2: Expected a line 17 characters long. Recieved a line 16 characters long."
        ]
        assert_that(result.get_errors(), equal_to(expected))


    def test_it_should_parse_multiple_records(self):
        file_obj = cStringIO.StringIO("1234123412341234Y\r\nABCDEFGHIJKLMNOPN")
        result = self.file_import_service.process_flat_file_stream(file_obj, self.simple_spec)
        expected = [
            {'auth_token': "1234123412341234", 'actively_at_work':'Y'},
            {'auth_token': "ABCDEFGHIJKLMNOP", 'actively_at_work':'N'},
        ]
        assert_that(result.get_data(), equal_to(expected))

    def test_it_should_generate_basic_documentation(self):
        documentation = FlatFileDocumentation(self.simple_spec)
        expected = """\
Field,From,To,Length,Description
auth_token,1,16,16,
actively_at_work,17,17,1,
"""
        assert_that(documentation.toCSV(), equal_to(expected))

    def test_it_should_generate_a_flat_file_spec_based_on_enrollment_records(self):
        spec = self.file_import_service.get_flat_file_spec()
        documentation = FlatFileDocumentation(spec)
        # documentation.toCSV("documentation.csv")
        # documentation.toHTML("documentation.html")
        # documentation.toPDF("documentation.pdf")
