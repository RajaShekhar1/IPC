from datetime import datetime
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
            DocuSignTextTab('MonthlyPremium', self.get_monthly_premium()),
            DocuSignTextTab('DraftDay', self.get_draft_day()),
        ]

        return tabs

    def get_monthly_premium(self):
        # For now, just add the premiums, since we know they are monthly payment mode.
        return self.data.format_money(self.data.get_total_modal_premium())

    def get_draft_day(self):
        if datetime.today().day <= 28:
            return datetime.today().day
        else:
            return 1