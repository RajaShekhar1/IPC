
from flask_script import Command, Option

from taa.services.data_import.file_import import FlatFileDocumentation

class GenFlatFileDocsCommand(Command):
    """Generate the flat file docs as either HTML or PDF"""

    option_list = [
        Option(dest='filename', help="Output filename")
    ]

    def run(self, filename):

        with open(filename, 'w+') as f:
            documentation = FlatFileDocumentation.generate_html_docs()
            f.write(documentation)
