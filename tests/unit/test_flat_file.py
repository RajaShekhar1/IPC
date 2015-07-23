from unittest2 import TestCase

from hamcrest import assert_that, equal_to

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
        flat_file_importer = FlatFileImporter()

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

        flat_file_importer = FlatFileImporter()

        result = flat_file_importer.import_data("1234123412341234Y")

        expected = [
            {'auth_token': "1234123412341234", 'actively_at_work':'Y'},

        ]
        assert_that(result.get_data(), equal_to(expected))

    def test_it_should_parse_multiple_records(self):
        pass

    def test_it_should_generate_basic_documentation(self):
        # Should have csv name,
        pass




class FlatFileFieldDefinition(object):
    def __init__(self, size, csv_name, title, description):
        self.size = size
        self.csv_name = csv_name
        self.title = title
        self.description = description


class FlatFileImporter(object):
    def __init__(self):
        pass

    def import_data(self, bytes):
        return FlatFileImportResult()


class FlatFileImportResult(object):
    def get_data(self):
        pass

    def get_errors(self):