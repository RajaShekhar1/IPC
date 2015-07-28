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

    def process_flat_file_stream(self, file_obj, spec=None):
        """
        Read in file data accordign to flat file syntax,
            output as rows of dictionaries.
        If no spec is defined, use default flat_file_spec defined above
        """
        if not spec:
            spec = self.get_flat_file_spec()
        importer = FlatFileImporter(spec)
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


class FlatFileDocumentation(object):
    fieldnames = ["Field", "From", "To", "Length", "Description"]
    def __init__(self, spec):
        self.spec = spec

    def toCSV(self, filename = None):
        output = cStringIO.StringIO()
        writer = csv.DictWriter(output, fieldnames=self.fieldnames, lineterminator="\n")
        writer.writeheader()
        distanceRead = 1
        for s in self.spec:
            writer.writerow({"Field":s.csv_name, "From":distanceRead, "To":distanceRead+s.size-1, "Length":s.size, "Description": s.description})
            distanceRead += s.size
        if filename:
            with open(filename, "w+") as f:
                f.write(output.getvalue())
        return output.getvalue()

    def toHTML(self, filename = None):
        template = """
<html>
<head>
<body style="font-family:sans-serif;">
<h1 style="text-align:center;">5Star Flat File Syntax Documentation</h1>
<table>
{}
</table>
</body>
</html>
"""
        table_rows = "<tr style='background:rgba(200,50,45,.2);'>{}</tr>\n".format("".join(["<th style='color:#fff;text-align:left;padding:0px 10px;'>{}</th>".format(fn) for fn in self.fieldnames]))
        distanceRead = 1
        for s in self.spec:
            table_rows+="<tr>{}</tr>\n".format("<td>{}</td>\
<td>{}</td>\
<td>{}</td>\
<td>{}</td>\
<td>{}</td>".format(s.csv_name, distanceRead, distanceRead+s.size-1, s.size, s.description))
            distanceRead += s.size
        if filename:
            with open(filename, "w+") as f:
                f.write(template.format(table_rows))
        return template.format(table_rows)

    def toPDF(self, filename):
        """
        Takes a file name and converts the html output from toHTML() to a PDF using Pandoc. It saves the PDF to the given file name and returns True if it was successful.
        """
        import pdfkit
        html = self.toHTML()
        pdfkit.from_string(html, filename)


class FlatFileImporter(object):
    def __init__(self, spec, data=None):
        self.spec = spec
        self.data = data if data is not None else []
        self.errors = []

    def import_file(self, file_obj):
        bytes = file_obj.read()
        spec_size = reduce(lambda acc, spec: acc + spec.size, self.spec, 0)
        for i, line in enumerate(bytes.splitlines()):
            if spec_size != len(line):
                self.errors.append("Line {}: Expected a line {} characters long. Recieved a line {} characters long.".format(i+1, spec_size, len(line)))
                continue
            lineReader = cStringIO.StringIO(line)
            data = {}
            for s in self.spec:
                data[s.csv_name] = lineReader.read(s.size).strip()
            self.data.append(data)
    def get_data(self):
        return self.data

    def has_error(self):
        return self.errors

    def get_errors(self):
        return self.errors


class FlatFileImportResult(object):
    def __init__(self, data = None):
        self.data = data if data is not None else []
        self.errors = []

    def get_data(self):
        return self.data

    def has_error(self):
        return self.errors

    def get_errors(self):
        return self.errors
