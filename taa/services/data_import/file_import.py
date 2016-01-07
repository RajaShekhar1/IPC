import csv
import cStringIO

from flask import render_template

from taa.helpers import UnicodeCsvWriter
from taa import app
from taa.services.enrollments import EnrollmentRecordParser
from taa.services.validators import (
    required_validator,
    birthdate_validator,
    coverage_validator,
    premium_validator,
    payment_mode_validator,
    gender_validator,
    enrollment_type_validator,
    zip_validator,

    email_validator, ssn_validator, state_validator, timestamp_validator, question_answered_validator,
    replaced_or_financing_validator, initials_validator, product_validator)
from taa.services.preprocessors import preprocess_date


class FileImportService(object):
    def get_flat_file_spec(self):
        return self._get_enrollment_spec(include_headers=False)

    def get_flat_file_header_spec(self):
        flat_file_header_spec = FlatFileSpec("header")
        flat_file_header_spec.add_standard_headers()
        return flat_file_header_spec

    def get_csv_spec(self):
        return self._get_enrollment_spec(include_headers=True)

    def _get_enrollment_spec(self, include_headers):
        flat_file_spec = FlatFileSpec("record")
        for field in EnrollmentRecordParser.all_fields:
            is_header_field = field.flat_file_size == 0
            if is_header_field and not include_headers:
                continue

            flat_file_spec.add_to_spec(FlatFileFieldDefinition(
                size=field.flat_file_size,
                csv_name=field.dict_key_name,
                title=field.title,
                description=field.description,
                is_required=(required_validator in field.validators),
                format=self.get_data_format(field),
            ))
        return flat_file_spec

    def get_data_format(self, field):
        format_str = ""
        if field.preprocessor is preprocess_date:
            format_str += "Date format is YYYY-MM-DD"

        if timestamp_validator in field.validators:
            # Override date preprocessor format string
            format_str = "Timestamp format is YYYY-MM-DDTHH:MM:SS"

        if birthdate_validator in field.validators:
            format_str += "; Date must not be future."

        if coverage_validator in field.validators:
            format_str = "A positive integer, only digits. If no coverage selected, leave blank."

        if premium_validator in field.validators:
            format_str = "'NNN.NN', where N is a digit. No currency sign. Premium is modal. If no coverage selected, leave blank."

        if gender_validator in field.validators:
            format_str = "Either 'M' or 'F'"

        if payment_mode_validator in field.validators:
            format_str = "Must be either '52', '26', '24', or '12'"

        if enrollment_type_validator in field.validators:
            format_str = "'A' for Enroller-Assisted enrollment, 'S' for self-enrollment"

        if zip_validator in field.validators:
            format_str = "5 or more digits"

        if email_validator in field.validators:
            format_str = "If provided, must resemble an email address, including an '@' symbol."

        if ssn_validator in field.validators:
            format_str = "NNNNNNNNN (9 digits with no dashes)"

        if state_validator in field.validators:
            format_str = "2 character state code"

        if question_answered_validator in field.validators:
            format_str = "'Y' or 'N' if provided"

        if replaced_or_financing_validator in field.validators:
            format_str = "'R' if replaced or 'F' if financing"

        if initials_validator in field.validators:
            format_str = "Must be either 2 or 3 characters long"

        if product_validator in field.validators:
            format_str = "One of 'FPPTI', 'FPPCI', or 'CIEMP'"

        return format_str

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
        # Autodetect CSV dialect
        try:
            self.dialect = csv.Sniffer().sniff(file_obj.readline(), delimiters=[',', '\t'])
        except csv.Error as e:
            self.dialect = None

        # To get universal newlines (ie, cross-platform) we use splitlines()
        file_obj.seek(0)
        lines = file_obj.read().splitlines()

        if not lines:
            self.error = "The file is empty or could not be parsed."
            self.headers = self.rows = []
            return

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
        if not lines:
            return lines

        header = []
        header_reader = csv.reader([lines[0]], dialect)
        for row in header_reader:
            header = [col.upper() for col in row]
        io = cStringIO.StringIO()
        header_writer = UnicodeCsvWriter(io, dialect)
        header_writer.writerow(header)
        lines[0] = io.getvalue()
        return lines

    def has_error(self):
        return self.error is not None

    def get_error_message(self):
        return self.error


class CSVFileDocumentation(object):
    @staticmethod
    def generate_html_docs():
        file_import_service = FileImportService()
        docs = CSVFileDocumentation(
            row_spec=file_import_service.get_csv_spec(),
        )
        return docs.toHTML()

    @staticmethod
    def generate_pdf_docs(filename):
        file_import_service = FileImportService()
        docs = CSVFileDocumentation(
            row_spec=file_import_service.get_csv_spec(),
        )
        return docs.toPDF(filename)

    fieldnames = ["Field Name", "Required", "Description", "Data Format"]

    def __init__(self, row_spec):
        self.row_spec = row_spec

    def toCSV(self, filename = None):
        output = cStringIO.StringIO()
        writer = csv.DictWriter(output, fieldnames=self.fieldnames, lineterminator="\n")
        writer.writeheader()
        distanceRead = 1
        for s in self.row_spec:
            writer.writerow({"Field Name":s.csv_name,
                             "From":distanceRead,
                             "To":distanceRead+s.size-1,
                             "Size":s.size,
                             "Required": "Yes" if s.is_required else "No",
                             "Data Format": s.format,
                             "Description": s.description})
            distanceRead += s.size
        if filename:
            with open(filename, "w+") as f:
                f.write(output.getvalue())
        return output.getvalue()

    def toHTML(self, filename=None):
        header_rows = []

        record_rows = []
        for s in self.row_spec.get_spec():
            record_rows.append([s.csv_name, "Yes" if s.is_required else "No", s.description, s.format])

        with app.test_request_context('/'):
            template = render_template("documentation/csv_import_documentation.html",
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


class FlatFileFieldDefinition(object):
    def __init__(self, size, csv_name, title, description, format="", is_required=False):
        self.size = size
        self.csv_name = csv_name
        self.title = title
        self.description = description
        self.format = format
        self.is_required = is_required


class FlatFileSpec(object):

    FLAT_FILE_TYPE = u'TAA_ENROLLMENT'
    FLAT_FILE_VERSION = u'1.0'

    def __init__(self, type_name):
        self.spec = []
        self.type = type_name

    def add_to_spec(self, definition):
        if isinstance(definition, list):
            self.spec += definition
        else:
            self.spec.append(definition)

    def get_spec(self):
        return self.spec

    def is_header_spec(self):
        return self.type == "header"

    def add_standard_headers(self):
        self.add_to_spec([
            FlatFileFieldDefinition(
                size=14,
                csv_name="FILE_TYPE",
                title="File Type",
                description="Must be {}".format(FlatFileSpec.FLAT_FILE_TYPE),
                format="",
                is_required=True,
            ),
            FlatFileFieldDefinition(
                size=8,
                csv_name="VERSION",
                title="Version Number",
                description="Must be {}".format(FlatFileSpec.FLAT_FILE_VERSION),
                format="N.N",
                is_required=True,
            ),
            FlatFileFieldDefinition(
                size=64,
                csv_name="USER_TOKEN",
                title="User Token",
                description="The API token authenticating the uploader",
                format="",
                is_required=True,
            ),
            FlatFileFieldDefinition(
                size=64,
                csv_name="CASE_TOKEN",
                title="Case Token",
                description="The token identifying the enrollment case",
                is_required=True,
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

    fieldnames = ["Field Name", "From", "To", "Size", "Required", "Description", "Data Format"]

    def __init__(self, row_spec, header_spec=None):
        self.header_spec = header_spec
        self.row_spec = row_spec

    def toCSV(self, filename = None):
        output = cStringIO.StringIO()
        writer = csv.DictWriter(output, fieldnames=self.fieldnames, lineterminator="\n")
        writer.writeheader()
        distanceRead = 1
        for s in self.row_spec:
            writer.writerow({"Field Name":s.csv_name,
                             "From":distanceRead,
                             "To":distanceRead+s.size-1,
                             "Size":s.size,
                             "Required": "Yes" if s.is_required else "No",
                             "Data Format": s.format,
                             "Description": s.description})
            distanceRead += s.size
        if filename:
            with open(filename, "w+") as f:
                f.write(output.getvalue())
        return output.getvalue()

    def toHTML(self, filename=None):
        header_rows = []

        if self.header_spec:
            distanceRead = 1
            for s in self.header_spec.get_spec():
                header_rows.append([s.csv_name, distanceRead, distanceRead+s.size-1, s.size, "Yes" if s.is_required else "No", s.description, s.format])
                distanceRead += s.size
        record_rows = []
        distanceRead = 1
        for s in self.row_spec.get_spec():
            record_rows.append([s.csv_name, distanceRead, distanceRead+s.size-1, s.size, "Yes" if s.is_required else "No", s.description, s.format])
            distanceRead += s.size

        with app.test_request_context('/'):
            template = render_template("documentation/flat_file_import_documentation.html",
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
                self.errors.append("Expected a header line {} characters long. Received a line {} characters long.".format(header_spec_size, len(headers)))
            else:
                lineReader = cStringIO.StringIO(headers)
                for s in self.header_spec:
                    self.headers[s.csv_name.lower()] = lineReader.read(s.size).strip()
                self.validate_headers()

        spec_size = reduce(lambda acc, spec: acc + spec.size, self.spec, 0)
        for i, line in enumerate(records):
            if spec_size != len(line):
                self.errors.append("Line {}: Expected a line {} characters long. Received a line {} characters long.".format(i+1, spec_size, len(line)))
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
        if not self.headers["file_type"] == FlatFileSpec.FLAT_FILE_TYPE:
            expected = FlatFileSpec.FLAT_FILE_TYPE
            self.errors.append("Expected FILE_TYPE header to be {} but got {}".format(expected, self.headers["file_type"]))
        if not self.headers["version"] == FlatFileSpec.FLAT_FILE_VERSION:
            expected = FlatFileSpec.FLAT_FILE_VERSION
            self.errors.append("Expected VERSION header to be {} but got {}".format(expected, self.headers["version"]))

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
