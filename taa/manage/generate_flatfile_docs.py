
from flask_script import Command, Option

from taa.services.data_import.file_import import FlatFileDocumentation

import os

class GenFlatFileDocsCommand(Command):
    """Generate the flat file docs as either HTML or PDF"""

    option_list = [
        Option(dest='filename', help="Output filename")
    ]

    def run(self, filename):
        _, ext = os.path.splitext(filename)
        with open(filename, 'w+') as f:
            if ext == ".html":
                documentation = FlatFileDocumentation.generate_html_docs()
                f.write(documentation)
            elif ext == ".pdf":
                documentation = FlatFileDocumentation.generate_pdf_docs(filename)
