from dateutil.relativedelta import relativedelta
from datetime import timedelta
from dateutil.parser import parse
from calendar import monthrange

from taa.services.enrollments.paylogix import get_friday_index


def get_active_method(settings, signature_time):
    signature_time = signature_time.replace(hour=0, minute=0, second=0)
    method = None
    for enrollment_period in settings:
        if enrollment_period.get('type') == 'open':
            if parse(enrollment_period['enrollment_period'].get('start_date')) <= signature_time <= parse(
                     enrollment_period['enrollment_period'].get('end_date')):
                method = enrollment_period.get('method')

        if enrollment_period.get('type') == 'ongoing':
            if not method:
                method = enrollment_period.get('method')

    return method


def calculate_effective_date(case, signature_time):
    """
    instantiate effective date calculator, if enroller selects is true, the date has
    already been processed, there should be no calculate effective date
    """
    settings = case.effective_date_settings
    
    start_date = None
    end_date = None
    is_ongoing = False
    open_rule = None
    ongoing_rule = None
    for effective_date_method in settings:
        if effective_date_method.get('type') == 'open':
            if 'enrollment_period' not in effective_date_method:
                start_date = case.enrollment_periods[0].get_start_date()
                end_date = case.enrollment_periods[0].get_end_date()
            else:
                start_date = parse(effective_date_method['enrollment_period']['start_date'])
                end_date = parse(effective_date_method['enrollment_period']['end_date'])
            open_rule = create_rule(effective_date_method)
        if effective_date_method.get('type') == 'ongoing':
            is_ongoing = True
            ongoing_rule = create_rule(effective_date_method)

    effective_date_calc = EffectiveDateCalculator(period_start=start_date, period_end=end_date, is_ongoing=is_ongoing,
                                                  open_rule=open_rule, ongoing_rule=ongoing_rule)
    return effective_date_calc.get_effective_date_for_enroll_date(signature_time)


def create_rule(settings):
    """
    Based on a dictionary with effective_date_method and two optional parameters,
    return the instantiated matching rule object.
    """
    rule_class = {
        'static_date': StaticEffectiveDateRule,
        'day_of_month': CutoffEffectiveDateRule,
        'enroller_selects': EnrollerPicksRule,
        'first_friday': FirstFridayFollowingRule,
    }[settings['method']]
    info = {}

    if settings.get('method') == 'static_date':
        info['effective_date_param1'] = parse(settings.get('static_date'))

    if settings.get('method') == 'first_friday':
        info['effective_date_param1'] = int(settings.get('first_friday'))

    if settings.get('method') == 'day_of_month':
        info['effective_date_param1'] = int(settings.get('day_of_month'))

    if settings.get('method') == 'enroller_selects':
        info['effective_date_param1'] = int(settings['enroller_selects']['default'])
        info['effective_date_param2'] = int(settings['enroller_selects']['no_less'])
        info['enroller_picks_date'] = ""

    args = []

    if info.get('effective_date_param1'):
        args.append(info['effective_date_param1'])
    if info.get('effective_date_param2'):
        args.append(info['effective_date_param2'])
    if info.get('enroller_picks_date'):
        args.append(info['enroller_picks_date'])

    return rule_class(*args)


class EffectiveDateCalculator(object):
    def __init__(self, period_start, period_end, is_ongoing, open_rule, ongoing_rule):
        self.period_start = period_start
        self.period_end = period_end
        self.is_ongoing = is_ongoing
        self.open_rule = open_rule
        self.ongoing_rule = ongoing_rule

    def get_effective_date_for_enroll_date(self, enroll_date):
        if self.is_ongoing and not self.period_start:
            return self.ongoing_rule.get_effective_date(enroll_date)

        elif self.period_start and self.period_end and not self.ongoing_rule:
            # Just a date, no ongoing
            if self.period_start > enroll_date or enroll_date > self.period_end:
                return None
            return self.open_rule.get_effective_date(enroll_date) if not None else None

        elif self.period_start and self.period_end and self.ongoing_rule:
            # Both
            if self.period_start > enroll_date:
                return None
            if self.period_end < enroll_date:
                return self.ongoing_rule.get_effective_date(enroll_date)
            return self.open_rule.get_effective_date(enroll_date)


class StaticEffectiveDateRule(object):
    """
    any date or today (self.date) if later
    """

    def __init__(self, date):
        self.date = date

    def get_effective_date(self, enrollment_date):
        if enrollment_date > self.date:
            return enrollment_date
        else:
            return self.date


class CutoffEffectiveDateRule(object):
    """
    resolve to the 1st of the following month if the enrollment date
    is in between 1st of the month and the cutoff day,
    if not, it resolves to the first of the month following the following month of the cutoff day
    """

    def __init__(self, cutoff_date_int):
        self.cutoff_date = int(cutoff_date_int)

    def get_effective_date(self, enrollment_date):
        single_month = relativedelta(months=1)
        double_month = relativedelta(months=2)
        if enrollment_date.day < self.cutoff_date:
            enrollment_date = enrollment_date.replace(day=1)
            return enrollment_date + single_month
        else:
            enrollment_date = enrollment_date.replace(day=1)
            return enrollment_date + double_month


class EnrollerPicksRule(object):
    """
    enroller picks a date that must be more than minimum days,
    if no date picked, resolves to enrollment date + default_days,
    """

    def __init__(self, default_days, minimum_days, enroller_picks_date=None):
        self.enroller_picks_date = enroller_picks_date
        self.default_days = int(default_days)
        self.minimum_days = int(minimum_days)

    def get_effective_date(self, enrollment_date):
        if self.enroller_picks_date is not None:
            if (self.enroller_picks_date - enrollment_date).days < self.minimum_days:
                return None
            return self.enroller_picks_date
        else:
            return enrollment_date + timedelta(days=self.default_days)


def roll_date_to_friday(date, distance, friday_dict):
    conversion = {
        4: 4,
        3: 3,
        2: 2,
        1: 1,
        0: 0,
        -1: 6,
        -2: 5,
    }
    roll_forward = timedelta(days=conversion[distance])
    new_date = date + roll_forward
    if friday_dict[new_date] == 5:
        new_date += timedelta(days=7)
    return new_date


class FirstFridayFollowingRule(object):
    """
    pick the first Friday that occurs on or after today + specified calendar days,
    provided that is one of the first 4 Fridays.  Should there be a 5th Friday in the month,
    then we need to roll-forward to next Friday
    """

    def __init__(self, minimum_days):
        self.minimum_days = int(minimum_days)

    def get_effective_date(self, enrollment_date):
        
        loop_date = enrollment_date + timedelta(days=self.minimum_days)
        FRIDAY_DOW_INT = 4
        while loop_date.weekday() != FRIDAY_DOW_INT:
            loop_date += timedelta(days=1)
            
        # loop_date is now the first friday on or after today + self.minimum_days.
        # Return the index of which friday it is in the month.
        friday_index = self.get_friday_index(loop_date)
        
        if friday_index >= 5:
            # Add a week to get to the first friday of the next month
            loop_date += timedelta(days=7)
            
        return loop_date
        
    def get_friday_index(self, date):
        # This function takes a date and returns an int 1 to 4, so it includes the rollover logic we want (from 5 to 1).
        return get_friday_index(date)
        