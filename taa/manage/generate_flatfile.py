from flask import current_app
from flask_script import Command, prompt, prompt_pass, Option

from taa.services import LookupService
file_import_service = LookupService('FileImportService')

class CSVToFlatFileCommand(Command):
    """Take CSV input and convert it to Flat File format"""

    option_list = (
        Option(dest='inputfile'),
        Option(dest='outputfile'),
    )

    def run(self, inputfile, outputfile):

        with open(inputfile, "r+") as input_stream:
            output = self.convert_csv_to_flatfile(input_stream)

        with open(outputfile, "w+") as output_stream:
            output_stream.write(output)

    def convert_csv_to_flatfile(self, input_stream):
        csv = file_import_service.process_delimited_file_stream(input_stream)
        data = self.normalize_headers(csv.get_rows())
        spec = file_import_service.get_flat_file_spec()
        output = self.generate_flat_file(data, spec)
        return output

    def generate_flat_file(self, data, spec):
        """
        data is a list of dictionaries
        spec is a list of FlatFileFieldSpec objects
        """
        return "\n".join([self.format_flat_file_record(row, spec) for row in data])

    def format_flat_file_record(self, row, spec):
        row_text = ""
        for cur_spec in spec:
            current_item = row.get(cur_spec.csv_name)
            if not current_item:
                current_item = ""
            space_padding = "".join([" " for i in range(0, cur_spec.size - len(current_item))])
            row_text += "{}{}".format(current_item, space_padding)[:cur_spec.size]
        return row_text

    def normalize_headers(self, input_data):
        """
        Some CSV files might have uppercase dict keys, so we just lowercase them all for the flat file code to work
        """

        data = []
        for row in input_data:
            data.append({k.lower(): v for k, v in row.items()})
        return data
