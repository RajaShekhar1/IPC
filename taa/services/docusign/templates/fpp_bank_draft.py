

from taa.services.docusign.service import DocuSignServerTemplate, DocuSignTextTab, DocuSignRadioTab
from taa.services.docusign.DocuSign_config import get_bank_draft_template_id


class FPPBankDraftFormTemplate(DocuSignServerTemplate):
    def __init__(self, recipients, enrollment_data, should_use_docusign_renderer):

        product_type = enrollment_data["product_type"]
        state = enrollment_data["enrollState"]
        template_id = get_bank_draft_template_id(product_type, state)

        DocuSignServerTemplate.__init__(self, template_id, recipients, should_use_docusign_renderer)

        self.data = enrollment_data

    def generate_tabs(self, recipient):
        tabs = super(FPPBankDraftFormTemplate, self).generate_tabs(recipient)

        if not recipient.is_employee():
            return tabs

        employee_data = self.data["employee"]
        address = employee_data.get('address1', '')
        if employee_data.get('address2'):
            address += " " + employee_data.get('address2', '')

        tabs += [
            DocuSignTextTab('eeName', self.data.get_employee_name()),
            DocuSignTextTab('eeAddress', address),
            DocuSignTextTab('eeCity', employee_data.get('city', '')),
            DocuSignTextTab('eeState', employee_data.get('state', '')),
            DocuSignTextTab('eeZip', employee_data.get('zip', '')),
        ]

        return tabs

    def get_monthly_premium(self):
        # Look up the premium for this product using monthly payment mode
        product = self.data.get_product()

