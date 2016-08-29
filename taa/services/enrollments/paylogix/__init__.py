import datetime
# Get the base string type
try:
  basestring
except NameError:
  basestring = str

import dateutil.parser
import csv

from taa.services.docusign.docusign_envelope import EnrollmentDataWrap
from taa.services import LookupService

__all__ = ['get_deduction_week']

# Put bank draft info from sprintly ticket in:

standardized_data = {}
standardized_data['bank_draft'] = {
    'employee_ssn': '',
    'last': '',
    'first': '',
    'routing_number': '',
    'account_number': '',
    'account_type': 'C',  # 'S'
    'bank_name': '',
    'deduction_week': 1,  # 1..4,
}

FRIDAY = 5
MAX_WEEKS_PER_MONTH = 4
ADVANCE_DAYS = 2


def get_deduction_week(application_date):
    draft_date = get_draft_day(application_date)
    # Calculated week of month rolls over if it exceeds max weeks/month limit
    week = get_week_from_date(draft_date)
    return week


def get_week_from_date(draft_date):
    return int(draft_date.day / 7) % MAX_WEEKS_PER_MONTH + 1


def get_draft_day(application_date):
    if isinstance(application_date, basestring):
        app_date = dateutil.parser.parse(application_date)
    else:
        app_date = application_date

    adv_date = app_date + datetime.timedelta(days=ADVANCE_DAYS)
    dow = adv_date.isoweekday()
    draft_date = adv_date + datetime.timedelta(days=FRIDAY - dow)
    if dow > FRIDAY:
        draft_date += datetime.timedelta(days=7)
    return draft_date


def create_paylogix_csv(applications):
    """
    Create a CSV Export file for a collection of EnrollmentApplications
    :param applications: Enrollment applications to put in the CSV
    :type applications: list[taa.services.enrollments.models.EnrollmentApplication]
    :return: The CSV
    """
    csv_buffer = csv.StringIO()
    csv_data = csv.writer(csv_buffer)

    headers = [
        'Signature Time',
        'Effective Date',
        'EE SSN',
        'EE Last Name',
        'EE First Name',
        'Account Holder Name',
        'ACH Routing Number',
        'ACH Account Number',
        'ACH Account Type',
        'Bank Name',
        'Address One',
        'Address Two',
        'City, State, Zip',
        'Deduction Week',
        'Product Code',
        'Product Name',
        'Insured Last Name',
        'Insured First Name',
        'Insured DOB',
        'Insured Premium',
        'Insured Coverage',
        'Agent Code',
    ]

    csv_data.writerow(headers)

    enrollment_service = LookupService('EnrollmentApplicationService')
    """:type: taa.services.enrollments.EnrollmentApplicationService"""

    for application in applications:

        if application.did_decline():
            continue

        enrollment_data = enrollment_service.get_standardized_json_for_enrollment(application)
        for enrollment_item in enrollment_data:
            data_wrap = EnrollmentDataWrap(enrollment_item, application.case, application)
            product = data_wrap.get_product()

            if not product.requires_paylogix_export(application) or not data_wrap.has_bank_draft_info():
                continue


            for applicant_data in data_wrap.get_applicant_data():

                # We skip over any applicants who have no premium, for instance spouse and children for membership and HI
                if not applicant_data['premium']:
                    continue

                coverage = applicant_data['coverage']

                # Use coverage tier if it is provided
                if applicant_data['coverage_tier'] is not None:
                    coverage = applicant_data['coverage_tier']

                # If coverage is a simple boolean, make it say "Selected" on the export
                if coverage is True:
                    coverage = 'Selected'

                row = [
                    application.signature_time.strftime('%Y-%m-%dT%H:%M:%S%z'),
                    data_wrap.get_effective_date().strftime('%Y-%m-%d'),
                    application.census_record.employee_ssn,
                    application.census_record.employee_last,
                    application.census_record.employee_first,
                    data_wrap.get_account_holder_name(),
                    data_wrap.get_routing_number(),
                    data_wrap.get_account_number(),
                    data_wrap.get_account_type_shorthand(),
                    data_wrap.get_bank_name(),
                    data_wrap.get_address_one(),
                    data_wrap.get_address_two(),
                    data_wrap.get_city_state_zip(),
                    get_deduction_week(application.signature_time),

                    # Product
                    data_wrap.get_product_code(),
                    data_wrap.get_product().name,

                    # Insured data
                    applicant_data['last_name'],
                    applicant_data['name'],
                    applicant_data['birthdate'],
                    applicant_data['formatted_premium'],
                    coverage,

                    data_wrap.get_agent_code(),
                ]
                csv_data.writerow(row)

    return csv_buffer.getvalue()


if __name__ == '__main__':
    assert get_deduction_week('2016-06-01') == 3
    assert get_deduction_week('2016-06-07') == 3
    assert get_deduction_week('2016-06-08') == 4
    assert get_deduction_week('2016-06-14') == 4
    assert get_deduction_week('2016-06-15') == 1
    assert get_deduction_week('2016-06-20') == 1
    assert get_deduction_week('2016-06-21') == 1
    assert get_deduction_week('2016-06-22') == 2
    assert get_deduction_week('2016-06-28') == 2
    assert get_deduction_week('2016-06-29') == 3
    assert get_deduction_week('2016-06-30') == 3
    assert get_deduction_week('2016-07-01') == 3
    assert get_deduction_week('2016-07-05') == 3
    assert get_deduction_week('2016-07-06') == 4
    assert get_deduction_week('2016-07-12') == 4
    assert get_deduction_week('2016-07-13') == 1
    assert get_deduction_week('2016-07-20') == 1
