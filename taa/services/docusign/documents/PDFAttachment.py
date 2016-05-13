from reportlab.lib.units import inch
from reportlab.platypus import Spacer

from taa.services.docusign.service import BasePDFDoc, DocuSignSigTab, DocuSignTextTab
from utils import create_signature_line


class PDFAttachment(BasePDFDoc):
    def __init__(self, recipients, enrollment_data):
        BasePDFDoc.__init__(self, recipients)

        # Map recipient signers to the coords of the signature line so we can attach tabs to those lines.
        self.sig_coords = {}
        self.data = enrollment_data

    def get_spacer(self, size=.2 * inch):
        return Spacer(0, size)

    def draw_signature_line(self):
        return create_signature_line(self.page_width, self.sig_coords, self.get_signer_recipients())

    def generate_tabs(self, recipient, purpose):
        tabs = super(BasePDFDoc, self).generate_tabs(recipient, purpose)

        if self.is_recipient_signer(recipient):
            # Add a signature tab to the last page

            pdf_x, pdf_y = self.sig_coords[recipient.name]
            pix_x = pdf_x
            # Move it up a bit from where the line is.
            pix_y = (self.page_height - pdf_y)

            tab = DocuSignSigTab(x=pix_x, y=pix_y, document_id="1", page_number=str(self.get_num_pages()))
            tabs.append(tab)

            # In case this is an enrollment import, also add a text signature
            if recipient.is_employee() and self.data.has_employee_esigned():
                tabs.append(DocuSignTextTab("SignHereEmployee", self.data.get_employee_esignature()))
            if recipient.is_agent() and self.data.has_agent_esigned():
                tabs.append(DocuSignTextTab("SignHereAgent", self.data.get_agent_esignature()))

        return tabs

    def is_recipient_signer(self, recipient):
        # Employee or agent should be listed as signer on DS envelope, even if agent isn't signing.
        return recipient.is_employee() or recipient.is_agent()

    def get_signer_recipients(self):
        return [r for r in self.recipients if self.is_recipient_signer(r)]