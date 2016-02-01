from datetime import datetime

from dateutil.relativedelta import relativedelta

from taa.services.docusign.service import DocuSignServerTemplate, DocuSignTextTab
from taa.services.docusign.DocuSign_config import get_bank_draft_template_id


class FPPBankDraftFormTemplate(DocuSignServerTemplate):
    def __init__(self, recipients, enrollment_data, should_use_docusign_renderer):

        state = enrollment_data["enrollState"]
        template_id = get_bank_draft_template_id(enrollment_data.get_product_code(), state)

        DocuSignServerTemplate.__init__(self, template_id, recipients, should_use_docusign_renderer)

        self.data = enrollment_data

    def generate_tabs(self, recipient, purpose):
        tabs = super(FPPBankDraftFormTemplate, self).generate_tabs(recipient, purpose)

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

        hire_date = self.data.get_employee_date_of_hire()
        if not hire_date:
            # Default to today if we don't have the hire date.
            hire_date = datetime.today()

        # Use the day 14 days after the hire date as the draft day.
        draft_date = hire_date + relativedelta(days=14)

        # Default day to the first if not in the range 1 to 28.
        draft_day_of_month = draft_date.day
        if draft_day_of_month <= 28:
            return draft_day_of_month
        else:
            return 1