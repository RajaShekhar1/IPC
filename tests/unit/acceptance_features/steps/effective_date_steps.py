from decimal import Decimal

from behave import *
from hamcrest import assert_that, equal_to
from dateutil.parser import parse

from taa.services.products.RatePlan import ApplicantQuery, ApplicantDemographics, ApplicantQueryOptions
from taa.services.products.plan_codes import get_plan_code


@given("I have a case with an enrollment period from '{enrollment_start}' to '{enrollment_end}' and ongoing is {checked_or_unchecked}")
def step_impl(context, enrollment_start, enrollment_end, checked_or_unchecked):
    context.enrollment_start = enrollment_start
    context.enrollment_end = enrollment_end
    context.enrollment_ongoing = checked_or_unchecked == 'checked'


@step("the '{effective_date_setting_type}' effective date is set to '{effective_date_setting}' with parameter '{effective_date_param1}'")
def step_impl(context, effective_date_setting_type, effective_date_setting, effective_date_param1):
    settings = {}
    if effective_date_setting_type == 'open':
        context.effective_date_open = settings
    elif effective_date_setting_type == 'ongoing':
        context.effective_date_ongoing = settings
    else:
        raise ValueError("Invalid effective_date_setting_type '{}'".format(effective_date_setting_type))

    settings['effective_date_setting'] = effective_date_setting
    settings['effective_date_param1'] = effective_date_param1

# Just a copy of above with effective_date_param2 added
@step(
    "the '{effective_date_setting_type}' effective date is set to '{effective_date_setting}' with parameters '{effective_date_param1}' and '{effective_date_param2}")
def step_impl(context, effective_date_setting_type, effective_date_setting, effective_date_param1, effective_date_param2):
    settings = {}
    if effective_date_setting_type == 'open':
        context.effective_date_open = settings
    elif effective_date_setting_type == 'ongoing':
        context.effective_date_ongoing = settings
    else:
        raise ValueError("Invalid effective_date_setting_type '{}'".format(effective_date_setting_type))

    settings['effective_date_setting'] = effective_date_setting
    settings['effective_date_param1'] = effective_date_param1
    settings['effective_date_param2'] = effective_date_param2


@step("I enroll an applicant on '{enroll_date}'")
def step_impl(context, enroll_date):
    context.enroll_date = parse(enroll_date)


@then("I should see the effective date is '{expected_effective_date}'")
def step_impl(context, expected_effective_date):

    effective_date_calc = EffectiveDateCalculator()

    effective_date = effective_date_calc.get_effective_date_for_enroll_date(context.enroll_date)

    assert_that(effective_date, equal_to(expected_effective_date))


class StaticEffectiveDateRule(object):
    def __init__(self, date):
        self.date = date

    def get_effective_date(self, enrollment_date):
        if enrollment_date > self.date:
            return enrollment_date
        else:
            return self.date


class CutoffEffectiveDate(object):
    "Either the 1st of next month, or the 1st of the following month if after the cutoff"
    def __init__(self, cutoff_date_int):
        self.cutoff_date = int(cutoff_date_int)

    def get_effective_date(self, enrollment_date):
        pass
        #monthly_cutoff = datetime.datetime(enrollment_date.year, )
        #if enrollment_date

class EffectiveDateCalculator(object):
    def __init__(self, open_rule, ongoing_rule):
        self.open_rule = open_rule
        self.ongoing_rule = ongoing_rule

    def get_effective_date_for_enroll_date(self, enroll_date):
        pass