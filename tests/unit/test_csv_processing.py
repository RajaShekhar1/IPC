
import csv
from unittest import TestCase
from StringIO import StringIO

from hamcrest import assert_that, contains, has_entries, equal_to
from taa.services.data_import import FileImportService

class TestProcessCSVData(TestCase):
    """
    For parts of the site that accept CSV as input,
    test that we can detect tab-separated vs comma separated
    and extract the data as headers, rows, and dialect
    """

    def setUp(self):
        self.test_csv_data = StringIO("""\
TEST_COL1,TEST_COL2
val1,val2""")
        self.test_tab_data = StringIO("""\
TEST_COL1\tTEST_COL2
val1\tval2""")
        self.test_bad_row = StringIO("""\
TEST_COL1,TEST_COL2
val1,val2
,,,,""")


        self.sut = FileImportService()

    def test_it_extracts_headers(self):
        csv_data_file = self.test_csv_data

        importer = self.sut.process_delimited_file_stream(csv_data_file)

        expected = ['TEST_COL1', 'TEST_COL2']
        assert_that(importer.get_headers(), contains(*expected))

    def test_it_extracts_rows(self):
        csv_data_file = self.test_csv_data

        importer = self.sut.process_delimited_file_stream(csv_data_file)

        expected = [{'TEST_COL1':'val1', 'TEST_COL2':'val2'}]
        assert_that(importer.get_rows(), contains(*map(has_entries, expected)))

    def test_it_extracts_dialect(self):
        csv_data_file = self.test_csv_data

        importer = self.sut.process_delimited_file_stream(csv_data_file)

        assert_that(importer.get_dialect().delimiter, equal_to(','))

    def test_it_handles_tab_separated_values(self):
        tab_data_file = self.test_tab_data

        importer = self.sut.process_delimited_file_stream(tab_data_file)

        assert_that(importer.get_dialect().delimiter, equal_to('\t'))

    def test_it_gives_no_error_on_success(self):
        importer = self.sut.process_delimited_file_stream(self.test_csv_data)

        assert_that(importer.has_error(), equal_to(False))

    def test_it_gives_error_for_empty_file(self):
        importer = self.sut.process_delimited_file_stream(StringIO(""))

        assert_that(importer.has_error(), equal_to(True))
        
    def test_it_provides_error_line_for_bad_row(self):
        importer = self.sut.process_delimited_file_stream(self.test_bad_row)

        assert_that(importer.has_error(), equal_to(True))

    def test_it_provides_an_error_message(self):
        importer = self.sut.process_delimited_file_stream(self.test_bad_row)

        assert_that(importer.get_error_message(), not equal_to(None))

    def test_it_uppercases_headers(self):
        test_lowercase_headers = StringIO("""\
test_col1,test_col2
val1,val2""")

        importer = self.sut.process_delimited_file_stream(test_lowercase_headers)

        assert_that(importer.get_headers(), contains('TEST_COL1', 'TEST_COL2'))
