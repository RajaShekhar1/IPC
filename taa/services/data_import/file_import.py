import csv
import cStringIO

class FileImportService(object):


    def process_delimited_file_stream(self, file_obj):
        """
        Read in file data as either comma or tab separated values,
         output as rows of dictionaries.
         Also can access headers as a list,
            or the CSV dialect object.
        """
        importer = FileImporter()
        importer.import_file(file_obj)
        return importer


class FileImporter(object):
    default_error_message = """There was a problem in the file or file format that \
prevented us from accepting it. Please ensure you are \
sending a valid CSV file, compare your file with the \
provided sample CSV, or otherwise double-check the \
data you are sending. If you continue to have \
problems, please contact your 5Star representative \
for assistance."""

    def __init__(self):
        self.headers = []
        self.rows = []
        self.dialect = None
        self.error = None

    def get_headers(self):
        return self.headers

    def get_rows(self):
        return self.rows

    def get_dialect(self):
        return self.dialect

    def import_file(self, file_obj):

        bytes = file_obj.read()

        # Autodetect CSV dialect
        try:
            self.dialect = csv.Sniffer().sniff(bytes)
        except csv.Error as e:
            self.error = self.default_error_message
            self.headers = self.rows = []
            return

        # To get universal newlines (ie, cross-platform) we use splitlines()
        lines = bytes.splitlines()

        # Process headers so they are not case-sensitive.
        preprocessed_lines = self._preprocess_header_row(lines, self.dialect)

        reader = csv.DictReader(preprocessed_lines, restkey='extra', dialect=self.dialect)
        try:
            self.headers = reader.fieldnames
            self.rows = [r for r in reader]
        except csv.Error as e:
            self.error = self.default_error_message
            self.headers = self.rows = []
            return

    def _preprocess_header_row(self, lines, dialect):
        """
        To get case-insensitive parsing behaviour, we uppercase every column
        in the first line before passing it off to the DictReader
        """
        header = []
        header_reader = csv.reader([lines[0]], dialect)
        for row in header_reader:
            header = [col.upper() for col in row]
        io = cStringIO.StringIO()
        header_writer = csv.writer(io, dialect)
        header_writer.writerow(header)
        lines[0] = io.getvalue()
        return lines

    def has_error(self):
        return self.error is not None

    def get_error_message(self):
        return self.error