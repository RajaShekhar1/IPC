import csv
import cStringIO

from taa.services.enrollments.enrollment_import import EnrollmentRecordParser

class FileImportService(object):
    def get_flat_file_spec(self):
        flat_file_spec = []
        for field in EnrollmentRecordParser.all_fields:
            #self.dict_key_name = dict_key_name
            #self.database_name = database_name
            #self.description = description
            #self.title = title
            flat_file_spec.append(FlatFileFieldDefinition(
                size = field.flat_file_size,
                csv_name = field.dict_key_name,
                title = field.title,
                description = field.description
            ))
        return flat_file_spec

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

class FlatFileFieldDefinition(object):
    def __init__(self, size, csv_name, title, description):
        self.size = size
        self.csv_name = csv_name
        self.title = title
        self.description = description

class FlatFileDocumentation(object):
    def __init__(self, spec):
        self.spec = spec

    def toCSV(self):
        fieldnames = ["Field", "From", "To", "Length", "Description"]
        output = cStringIO.StringIO()
        writer = csv.DictWriter(output, fieldnames=fieldnames, lineterminator="\n")
        writer.writeheader()
        distanceRead = 1
        for s in self.spec:
            writer.writerow({"Field":s.csv_name, "From":distanceRead, "To":distanceRead+s.size-1, "Length":s.size, "Description": s.description})
            distanceRead += s.size
        return output.getvalue()

class FlatFileImporter(object):
    def __init__(self, spec):
        self.spec = spec

    def import_data(self, bytes):
        reader = cStringIO.StringIO(bytes)
        response = FlatFileImportResult()
        for line in reader:
            lineReader = cStringIO.StringIO(line)
            data = {}
            for s in self.spec:
                data[s.csv_name] = lineReader.read(s.size)
            response.data.append(data)
        return response


class FlatFileImportResult(object):
    def __init__(self, data = None):
        self.data = data if data is not None else []

    def get_data(self):
        return self.data

    def get_errors(self):
        pass
