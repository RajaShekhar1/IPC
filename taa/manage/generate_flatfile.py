from flask import current_app
from flask_script import Command, prompt, prompt_pass, Option

from ..services.data_import.file_import import FlatFileImporter, FlatFileDocumentation, FlatFileFieldDefinition, FlatFileImportResult, FileImportService

class CSVToFlatFileCommand(Command):
    """Take CSV input and convert it to Flat File format"""

    option_list = (
        Option('--input', '-i', dest='inputfile'),
        Option('--output', '-o', dest='outputfile'),
    )

    def run(self, inputfile, outputfile):
        file_import_service = FileImportService()
        with open(inputfile, "r+") as f:
            csv = file_import_service.process_delimited_file_stream(f)
        # some csv files might have uppercase dict keys, so we just lowercase them all for the flat file code to work
        data = []
        for row in csv.get_rows():
            data.append({k.lower():v for k,v in row.items()})
        spec = file_import_service.get_flat_file_spec()
        output_text = ""
        for row in data:
            for cur_spec in spec:
                current_item = row.get(cur_spec.csv_name)
                if not current_item:
                    current_item = ""
                space_padding = "".join([" " for i in range(0, cur_spec.size-len(current_item))])
                output_text+="{}{}".format(current_item, space_padding)[:cur_spec.size]
            output_text+="\n"
        with open(outputfile, "w+") as f:
            f.write(output_text)
