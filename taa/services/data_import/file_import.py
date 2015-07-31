import csv
import cStringIO

from flask import render_template

from taa.services.enrollments import EnrollmentRecordParser


class FileImportService(object):
    def get_flat_file_spec(self):
        flat_file_spec = FlatFileSpec("record")
        for field in EnrollmentRecordParser.all_fields:
            flat_file_spec.add_to_spec(FlatFileFieldDefinition(
                size = field.flat_file_size,
                csv_name = field.dict_key_name,
                title = field.title,
                description = field.description
            ))
        return flat_file_spec

    def get_flat_file_header_spec(self):
        flat_file_header_spec = FlatFileSpec("header")
        flat_file_header_spec.add_standard_headers()
        return flat_file_header_spec

    def process_flat_file_stream(self, file_obj, spec=None, header_spec=None):
        """
        Read in file data according to flat file syntax,
            output as rows of dictionaries.
        If no spec is defined, use default flat_file_spec defined above
        """
        if not spec:
            spec = self.get_flat_file_spec()
        if not header_spec:
            header_spec = self.get_flat_file_header_spec()
        importer = FlatFileImporter(spec=spec, header_spec=header_spec)
        importer.import_file(file_obj)
        return importer

    def process_delimited_file_stream(self, file_obj):
        """
        Read in file data as either comma or tab separated values,
         output as rows of dictionaries.
         Also can access headers as a list,
            or the CSV dialect object.
        """
        importer = CSVFileImporter()
        importer.import_file(file_obj)
        return importer


class CSVFileImporter(object):
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


class FlatFileSpec(object):
    def __init__(self, type_name):
        self.spec = []
        self.type = type_name

    def add_to_spec(self, definition):
        if isinstance(definition, list):
            self.spec+=definition
        else:
            self.spec.append(definition)

    def get_spec(self):
        return self.spec

    def is_header_spec(self):
        return self.type == "header"

    def add_standard_headers(self):
        self.add_to_spec([
            FlatFileFieldDefinition(
                size=16,
                csv_name="FILE_TYPE",
                title="File Type",
                description="Must be TAA_ENROLLMENT"
            ),
            FlatFileFieldDefinition(
                size=8,
                csv_name="VERSION",
                title="Version Number",
                description="Must be 1.0"
            ),
            FlatFileFieldDefinition(
                size=8,
                csv_name="RECORD_COUNT",
                title="Record Count",
                description="Must match number of record in file"
            ),
            FlatFileFieldDefinition(
                size=64,
                csv_name="USER_TOKEN",
                title="User Token",
                description="The API token representing the uploading user"
            ),
            FlatFileFieldDefinition(
                size=64,
                csv_name="CASE_TOKEN",
                title="Case Token",
                description="The token representing the enrolling case"
            )
        ])

class FlatFileDocumentation(object):

    @staticmethod
    def generate_html_docs():
        file_import_service = FileImportService()
        docs = FlatFileDocumentation(
            header_spec=file_import_service.get_flat_file_header_spec(),
            row_spec=file_import_service.get_flat_file_spec(),
        )
        return docs.toHTML()

    @staticmethod
    def generate_pdf_docs(filename):
        file_import_service = FileImportService()
        docs = FlatFileDocumentation(
            header_spec=file_import_service.get_flat_file_header_spec(),
            row_spec=file_import_service.get_flat_file_spec(),
        )
        return docs.toPDF(filename)

    fieldnames = ["Field", "From", "To", "Length", "Description"]
    def __init__(self, row_spec, header_spec=None):
        self.header_spec = header_spec
        self.row_spec = row_spec

    def toCSV(self, filename = None):
        output = cStringIO.StringIO()
        writer = csv.DictWriter(output, fieldnames=self.fieldnames, lineterminator="\n")
        writer.writeheader()
        distanceRead = 1
        for s in self.row_spec:
            writer.writerow({"Field":s.csv_name, "From":distanceRead, "To":distanceRead+s.size-1, "Length":s.size, "Description": s.description})
            distanceRead += s.size
        if filename:
            with open(filename, "w+") as f:
                f.write(output.getvalue())
        return output.getvalue()

    def toHTML(self, filename=None):
        header_rows = []
        distanceRead = 1
        if self.header_spec:
            distanceRead = 1
            for s in self.header_spec.get_spec():
                header_rows.append([s.csv_name, distanceRead, distanceRead+s.size-1, s.size, s.description])
                distanceRead += s.size
        record_rows = []
        distanceRead = 1
        for s in self.row_spec.get_spec():
            record_rows.append([s.csv_name, distanceRead, distanceRead+s.size-1, s.size, s.description])
            distanceRead += s.size

        template = render_template("documentation/simple_documentation.html",
                                    headers=header_rows,
                                    records=record_rows,
                                    fieldnames=self.fieldnames)

        if filename:
            with open(filename, "w+") as f:
                f.write(template)

        return template

    def toPDF(self, filename):
        """
        Takes a file name and converts the html output from toHTML() to a PDF using PDFKit. It saves the PDF to the given file name
        """
        import pdfkit
        html = self.toHTML()
        pdfkit.from_string(html, filename)


class FlatFileImporter(object):
    def __init__(self, spec, data=None, header_spec=None):
        self.spec = spec.get_spec()
        if header_spec:
            self.header_spec = header_spec.get_spec()
        else:
            self.header_spec = None
        self.data = data if data is not None else []
        self.record_count = 0
        self.errors = []
        self.headers = {}

    def import_file(self, file_obj):
        bytes = file_obj.read()
        file_lines = bytes.splitlines()
        if self.header_spec:
            headers = file_lines[0]
            records = file_lines[1:]
        else:
            records = file_lines
        self.record_count = len(records)
        if self.has_headers():
            header_spec_size = reduce(lambda acc, spec: acc + spec.size, self.header_spec, 0)
            if header_spec_size != len(headers):
                self.errors.append("Expected a header line {} characters long. Recieved a line {} characters long.".format(header_spec_size, len(headers)))
            else:
                lineReader = cStringIO.StringIO(headers)
                for s in self.header_spec:
                    self.headers[s.csv_name.lower()] = lineReader.read(s.size).strip()
                self.validate_headers()

        spec_size = reduce(lambda acc, spec: acc + spec.size, self.spec, 0)
        for i, line in enumerate(records):
            if spec_size != len(line):
                self.errors.append("Line {}: Expected a line {} characters long. Recieved a line {} characters long.".format(i+1, spec_size, len(line)))
                continue
            lineReader = cStringIO.StringIO(line)
            data = {
                "user_token":self.headers.get("user_token"),
                "case_token":self.headers.get("case_token")
            }
            for s in self.spec:
                if s.csv_name == "user_token" or s.csv_name == "case_token":
                    continue
                data[s.csv_name] = lineReader.read(s.size).strip()
            self.data.append(data)


    def get_data(self):
        return self.data

    def get_headers(self):
        return self.headers

    def validate_headers(self):
        if not self.headers["file_type"]=="TAA_ENROLLMENT":
            expected = "TAA_ENROLLMENT"
            self.errors.append("Expected FILE_TYPE header to be {} but got {}".format(expected, self.headers["file_type"]))
        if not self.headers["version"]=="1.0":
            expected = "1.0"
            self.errors.append("Expected VERSION header to be {} but got {}".format(expected, self.headers["version"]))
        if not int(self.headers["record_count"])==self.record_count:
            expected = self.record_count
            self.errors.append("Expected RECORD_COUNT header to be {} but got {}".format(expected, self.headers["record_count"]))

    def has_headers(self):
        return self.header_spec is not None

    def has_error(self):
        return bool(self.errors)

    def get_errors(self):
        return [FlatFileFormatError(message) for message in self.errors]

    def get_error_message(self):
        return "".join("Error {}: {}".format(i, message) for i, message in enumerate(self.errors))

class FlatFileFormatError(object):
    def __init__(self, message):
        self.message = message
    def to_json(self):
        return {"type": "flat_file_format_error", "message": self.message}
