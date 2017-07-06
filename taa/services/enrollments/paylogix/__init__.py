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
    # Get Nth Friday, where N is between 1 and 4 (5's roll over to 1)
    # Most reliable way is to count the fridays between the 1st and the given date

    num_fridays = get_friday_index(draft_date)
    
    if num_fridays >= 5:
        # Rollover to 1
        return 1
    else:
        return num_fridays


def get_friday_index(date):
    "Given a date, find the next Friday (if not a Friday), and return an index 1 through 5 of which Friday of the month it is."
    
    # If a Friday isn't given, advance to the next Friday
    if date.isoweekday() != FRIDAY:
        date += datetime.timedelta(days=FRIDAY - date.isoweekday())
    
    # Beginning with the 1st, count the Fridays, including the given date.
    day = 1
    num_fridays = 0
    for day_of_month in range(day, date.day + 1):
        check_date = datetime.datetime(date.year, date.month, day_of_month)
        if check_date.isoweekday() == FRIDAY:
            num_fridays += 1
            
    return num_fridays


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


def create_paylogix_csv(start, end):
    """
    Create a CSV Export file for a collection of EnrollmentApplications
    
    :param start: the date to start searching for entries
    :param end: the date to stop searching for entries
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
        'Group Number',
        'Company Name',
    ]

    csv_data.writerow(headers)

    enrollment_service = LookupService('EnrollmentApplicationService')
    """:type: taa.services.enrollments.EnrollmentApplicationService"""

    rows = enrollment_service.get_paylogix_report(start, end)
    [csv_data.writerow([row[column] for column in headers]) for row in rows]
    return csv_buffer.getvalue()

if __name__ == '__main__':
    # assert get_deduction_week('2016-06-01') == 3
    # assert get_deduction_week('2016-06-07') == 3
    # assert get_deduction_week('2016-06-08') == 4
    # assert get_deduction_week('2016-06-14') == 4
    # assert get_deduction_week('2016-06-15') == 1
    # assert get_deduction_week('2016-06-20') == 1
    # assert get_deduction_week('2016-06-21') == 1
    # assert get_deduction_week('2016-06-22') == 2
    # assert get_deduction_week('2016-06-28') == 2
    # assert get_deduction_week('2016-06-29') == 3
    # assert get_deduction_week('2016-06-30') == 3
    # assert get_deduction_week('2016-07-01') == 3
    # assert get_deduction_week('2016-07-05') == 3
    # assert get_deduction_week('2016-07-06') == 4
    # assert get_deduction_week('2016-07-12') == 4
    # assert get_deduction_week('2016-07-13') == 1
    # assert get_deduction_week('2016-07-20') == 1
    assert get_deduction_week('2016-10-07') == 1