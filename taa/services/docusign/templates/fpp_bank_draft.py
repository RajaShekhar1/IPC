from datetime import datetime

from dateutil.relativedelta import relativedelta

from taa.services.docusign.service import DocuSignServerTemplate, DocuSignTextTab, DocuSignRadioTab
from taa.services.docusign.DocuSign_config import get_bank_draft_template_id


class FPPBankDraftFormTemplate(DocuSignServerTemplate):
    def __init__(self, recipients, enrollment_data, should_use_docusign_renderer):

        state = enrollment_data["enrollState"]
        template_id = get_bank_draft_template_id(enrollment_data.get_product_code(), state)

        DocuSignServerTemplate.__init__(self, template_id, recipients, should_use_docusign_renderer)

        self.data = enrollment_data

    def generate_tabs(self, recipient, purpose):
        tabs = super(FPPBankDraftFormTemplate, self).generate_tabs(recipient, purpose)

        #if not recipient.is_employee() and not self.data.should_use_call_center_workflow():
        #    return tabs

        # Going forward enrollments will have bank draft data and should grab all account information from that
        if self.has_bank_draft_info():
            # New method that grabs data from the bank draft info entered in on the last step of the wizard
            tabs += [
                DocuSignTextTab('eeName', self.get_bank_account_holder()),
                DocuSignTextTab('eeAddress', self.get_account_address()),
                DocuSignTextTab('eeCity', self.get_account_city()),
                DocuSignTextTab('eeState', self.get_account_state()),
                DocuSignTextTab('eeZip', self.get_account_zip()),
                DocuSignTextTab('MonthlyPremium', self.get_monthly_premium()),
                DocuSignTextTab('DraftDay', self.get_draft_day()),
                DocuSignTextTab('RoutingNumber', self.get_bank_routing_number()),
                DocuSignTextTab('BankName', self.get_bank_name()),
                DocuSignTextTab('Account Number', self.get_bank_account_number()),
                DocuSignTextTab('CityStateZip', self.get_city_state_zip()),
                DocuSignRadioTab('AccountType', self.get_bank_account_type())
            ]
        else:
            # Old version to work with previously existing enrollments
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

    def has_bank_draft_info(self):
        return self.data.get('bank_info')

    def get_bank_account_type(self):
        return self.data['bank_info'].get('account_type', '')

    def get_bank_account_holder(self):
        return self.data['bank_info'].get('account_holder_name', '')

    def get_bank_account_number(self):
        return self.data['bank_info'].get('account_number', '')

    def get_bank_routing_number(self):
        return self.data['bank_info'].get('routing_number', '')

    def get_bank_name(self):
        return self.data['bank_info'].get('bank_name', '')

    def get_account_address_one(self):
        return self.data['bank_info'].get('address_one', '')

    def get_account_address_two(self):
        return self.data['bank_info'].get('address_two', '')

    def get_account_address(self):
        address_one = self.get_account_address_one()
        address_two = self.get_account_address_two()
        if address_two and len(address_two) > 0:
            return '%s %s' % (address_one, address_two)
        return address_one

    def get_account_city(self):
        return self.data['bank_info'].get('city', '')

    def get_account_state(self):
        return self.data['bank_info'].get('state', '')

    def get_account_zip(self):
        return self.data['bank_info'].get('zip', '')

    def get_city_state_zip(self):
        return '%s, %s %s' % (self.get_account_city(), self.get_account_state(), self.get_account_zip())

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