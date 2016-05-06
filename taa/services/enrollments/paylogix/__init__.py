import datetime

import dateutil.parser


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
ADVANCE_DAYS = 10


def get_deduction_week(application_date):
    draft_date = get_draft_day(application_date)
    # Calculated week of month rolls over if it exceeds max weeks/month limit
    week = int(draft_date.day / 7) % MAX_WEEKS_PER_MONTH + 1
    return week


def get_draft_day(application_date):
    app_date = dateutil.parser.parse(application_date)
    adv_date = app_date + datetime.timedelta(days=ADVANCE_DAYS)
    dow = adv_date.isoweekday()
    draft_date = adv_date + datetime.timedelta(days=FRIDAY-dow)
    if dow > FRIDAY:
        draft_date += datetime.timedelta(days=7)
    return draft_date


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
