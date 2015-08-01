from unittest2 import TestCase
import csv, cStringIO
from hamcrest import assert_that, equal_to, starts_with

from taa.services.data_import.file_import import (
    FileImportService,
    FlatFileDocumentation,
    FlatFileFieldDefinition,
    FlatFileSpec
)


class TestFlatFile(TestCase):
    def setUp(self):
        self.file_import_service = FileImportService()
        self.single_spec = FlatFileSpec("record")
        self.simple_spec = FlatFileSpec("record")

        self.single_spec.add_to_spec(
            FlatFileFieldDefinition(
                size=8,
                csv_name="emp_first",
                title="Employee First Name",
                description="",
            )
        )

        self.simple_spec.add_to_spec([
            FlatFileFieldDefinition(
                size=8,
                csv_name="emp_first",
                title="Employee First Name",
                description="",
            ),
            FlatFileFieldDefinition(
                size=1,
                csv_name="actively_at_work",
                title="Is the employee actively at work?",
                description="",
            ),
        ])

        self.headers_single = "TAA_ENROLLMENT1.0     1       e471d02990094e95b76ea096f0814783                                CASE-123                                                        "
        self.headers = "TAA_ENROLLMENT1.0     2       e471d02990094e95b76ea096f0814783                                CASE-123                                                        "

    def test_it_should_return_a_dictionary_from_a_single_field(self):
        file_obj = cStringIO.StringIO("{}\nJoe     ".format(self.headers_single))
        result = self.file_import_service.process_flat_file_stream(file_obj, spec=self.single_spec)
        expected = [
            {'user_token': 'e471d02990094e95b76ea096f0814783', 'case_token': 'CASE-123', 'emp_first': "Joe"}
        ]
        assert_that(result.get_data(), equal_to(expected))

    def test_it_should_parse_multiple_columns(self):
        file_obj = cStringIO.StringIO("{}\nJoe     Y".format(self.headers_single))
        result = self.file_import_service.process_flat_file_stream(file_obj, spec=self.simple_spec)
        expected = [
            {'user_token': 'e471d02990094e95b76ea096f0814783', 'case_token': 'CASE-123', 'emp_first': "Joe", 'actively_at_work':'Y'},
        ]
        assert_that(result.get_data(), equal_to(expected))

    def test_it_should_return_an_error_when_a_line_is_too_short(self):
        file_obj = cStringIO.StringIO("{}\nJoe    Y".format(self.headers_single))
        result = self.file_import_service.process_flat_file_stream(file_obj, spec=self.simple_spec)
        expected = [
            "Line 1: Expected a line 9 characters long. Received a line 8 characters long."
        ]
        assert_that([error.message for error in result.get_errors()], equal_to(expected), [error.message for error in result.get_errors()])

    def test_it_should_return_an_error_when_one_line_is_too_short_and_many_are_submitted(self):
        file_obj = cStringIO.StringIO("{}\nJoe    Y\nJohn    N".format(self.headers))
        result = self.file_import_service.process_flat_file_stream(file_obj, spec=self.simple_spec)
        expected = [
            "Line 1: Expected a line 9 characters long. Received a line 8 characters long.",
        ]
        assert_that([error.message for error in result.get_errors()], equal_to(expected), [error.message for error in result.get_errors()])

    # Removing this test - don't want to rely on an external file for these unit tests
    #def test_it_should_submit_a_flat_file_for_testing(self):
    #    with open("tests/data/minimal_data.flat", "r+") as f:
    #        result = self.file_import_service.process_flat_file_stream(f)

    #    assert_that(result.has_error(), equal_to(False), [error.message for error in result.get_errors()])

    def test_it_should_return_an_error_when_multiple_lines_are_too_short(self):
        file_obj = cStringIO.StringIO("{}\nJoe    Y\nJohn     N".format(self.headers))
        result = self.file_import_service.process_flat_file_stream(file_obj, spec=self.simple_spec)
        expected = [
            "Line 1: Expected a line 9 characters long. Received a line 8 characters long.",
            "Line 2: Expected a line 9 characters long. Received a line 10 characters long."
        ]
        assert_that([error.message for error in result.get_errors()], equal_to(expected), [error.message for error in result.get_errors()])

    def test_it_should_parse_multiple_records(self):
        file_obj = cStringIO.StringIO("{}\nJoe     Y\r\nJohn    N".format(self.headers))
        result = self.file_import_service.process_flat_file_stream(file_obj, spec=self.simple_spec)
        expected = [
            {'user_token': 'e471d02990094e95b76ea096f0814783', 'case_token': 'CASE-123', 'emp_first': "Joe", 'actively_at_work':'Y'},
            {'user_token': 'e471d02990094e95b76ea096f0814783', 'case_token': 'CASE-123', 'emp_first': "John", 'actively_at_work':'N'},
        ]
        assert_that(result.get_data(), equal_to(expected), [error.message for error in result.get_errors()])

    def test_it_should_generate_basic_documentation(self):
        documentation = FlatFileDocumentation(self.simple_spec.get_spec())
        expected = """\
Field Name,From,To,Size,Required,Description,Data Format
emp_first,1,8,8,No,,
actively_at_work,9,9,1,No,,
"""
        assert_that(documentation.toCSV(), equal_to(expected))

    def test_it_should_generate_html_docs(self):
        documentation = FlatFileDocumentation.generate_html_docs()

        assert_that(documentation, starts_with('<html>'))

