import json
from datetime import datetime

from dateutil.parser import parse
from dateutil.relativedelta import relativedelta
from taa.services.docusign.service import DocuSignServerTemplate, DocuSignTextTab, DocuSignRadioTab
from taa.services.docusign.DocuSign_config import get_bank_draft_template_id
from taa.services.enrollments.paylogix import get_week_from_date


class FPPBankDraftFormTemplate(DocuSignServerTemplate):
    def __init__(self, recipients, enrollment_data, should_use_docusign_renderer):

        state = enrollment_data["enrollState"]
        template_id = get_bank_draft_template_id(enrollment_data.get_product_code(), state)

        DocuSignServerTemplate.__init__(self, template_id, recipients, should_use_docusign_renderer)

        self.data = enrollment_data

    def generate_tabs(self, recipient, purpose):
        tabs = super(FPPBankDraftFormTemplate, self).generate_tabs(recipient, purpose)

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
        return self.data.get_billing_city()

    def get_account_state(self):
        return self.data.get_billing_state()

    def get_account_zip(self):
        return self.data.get_billing_zip()

    def get_city_state_zip(self):
        return self.data.get_city_state_zip()

    def get_monthly_premium(self):
        # For now, just add the premiums, since we know they are monthly payment mode.
        return self.data.format_money(self.data.get_total_modal_premium())

    def get_draft_day(self):
        # Get oldest Paylogix effective date if it exists
        effective_date = self.data.get_effective_date()
        if self.data.enrollment_record.is_paylogix:
            # Sort enrollments by signature date
            raw_effective_date = self.data.get('effective_date')
            for application in sorted(
                    [e for e in self.data.enrollment_record.census_record.enrollment_applications if not e.is_preview],
                    key=lambda a: a.signature_time):
                # if not application.is_paylogix:
                #     continue
                if application.signature_time >= self.data.enrollment_record.signature_time:
                    # No need to search the remaining enrollments, as they
                    # cannot be earlier than the current enrollment
                    break
                # if not application.is_paylogix:
                #     continue
                earliest_coverage_effective_date = None
                for coverage in application.coverages:
                    if coverage.product_id != self.data.get_product_id():
                        # Skip non-matching products
                        continue
                    earliest_coverage_effective_date = coverage.effective_date
                    if earliest_coverage_effective_date is not None:
                        # Use earliest Paylogix-enabled enrollment date as
                        # effective date
                        effective_date = earliest_coverage_effective_date
                        break
                if earliest_coverage_effective_date is not None:
                    break
            return self.get_paylogix_date(
                    effective_date,
                    raw_effective_date is not None)
        else:
            # Day of the month
            return effective_date.day

    def get_paylogix_date(self, effective_date, is_raw_effective_date):
        if is_raw_effective_date:
            # Nth Friday based on week of effective date
            week = get_week_from_date(effective_date)
            return self.format_deduction_week(week)
        else:
            # Older method will compute the next Friday based on 5-day
            # interval after signature time
            return self.get_paylogix_draft_day(effective_date)

    def get_paylogix_draft_day(self, date):
        from taa.services.enrollments.paylogix import get_deduction_week
        deduction_week = get_deduction_week(date)
        return self.format_deduction_week(deduction_week)

    def format_deduction_week(self, deduction_week):
        if deduction_week == 1:
            return '1st Friday'
        elif deduction_week == 2:
            return '2nd Friday'
        elif deduction_week == 3:
            return '3rd Friday'
        else:
            return '{}th Friday'.format(deduction_week)
