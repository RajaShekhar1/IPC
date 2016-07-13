from behave import *
from dateutil.parser import parse
from hamcrest import assert_that, equal_to

from taa.services.enrollments.effective_date import StaticEffectiveDateRule, CutoffEffectiveDateRule, EnrollerPicksRule, \
    FirstFridayFollowingRule, EffectiveDateCalculator


@given("I have a case with an enrollment period from '{enrollment_start}' to '{enrollment_end}' and ongoing is {checked_or_unchecked}")
def step_impl(context, enrollment_start, enrollment_end, checked_or_unchecked):
    context.enrollment_start = enrollment_start
    context.enrollment_end = enrollment_end
    context.enrollment_ongoing = checked_or_unchecked == 'checked'


@given("I have a case without an enrollment period but it has ongoing enrollments")
def step_impl(context):
    context.enrollment_start = None
    context.enrollment_end = None
    context.enrollment_ongoing = True


@step("the '{effective_date_setting_type}' effective date is set to '{effective_date_method}' with parameter '{effective_date_param1}'")
def step_impl(context, effective_date_setting_type, effective_date_method, effective_date_param1):
    settings = {
        'effective_date_method': effective_date_method,
        'effective_date_param1': effective_date_param1,
    }

    if effective_date_setting_type == 'open':
        context.effective_date_open = settings
    elif effective_date_setting_type == 'ongoing':
        context.effective_date_ongoing = settings
    else:
        raise ValueError("Invalid effective_date_setting_type '{}'".format(effective_date_setting_type))

# Just a copy of above with effective_date_param2 added
@step(
    "the '{effective_date_setting_type}' effective date is set to '{effective_date_method}' with parameters '{effective_date_param1}' and '{effective_date_param2}")
def step_impl(context, effective_date_setting_type, effective_date_method, effective_date_param1, effective_date_param2):
    settings = {
        'effective_date_setting': effective_date_method,
        'effective_date_param1': effective_date_param1,
        'effective_date_param2': effective_date_param2,
    }

    if effective_date_setting_type == 'open':
        context.effective_date_open = settings
    elif effective_date_setting_type == 'ongoing':
        context.effective_date_ongoing = settings
    else:
        raise ValueError("Invalid effective_date_setting_type '{}'".format(effective_date_setting_type))


@step("I enroll an applicant on '{enroll_date}'")
def step_impl(context, enroll_date):
    context.enroll_date = parse(enroll_date)


@step("The enroller picks '{enroller_picks_date}' and I enroll an applicant on '{enroll_date}'")
def step_impl(context, enroller_picks_date, enroll_date):
    context.enroll_date = parse(enroll_date)
    context.enroller_picks_date = parse(enroller_picks_date)


def create_rule(settings):
    "Based on a dictionary with effective_date_method and two optional parameters, return the instantiated matching rule object."
    rule_class = {
        'Static Date': StaticEffectiveDateRule,
        'Cutoff nth of month': CutoffEffectiveDateRule,
        'Enroller Picks': EnrollerPicksRule,
        'Friday grouping': FirstFridayFollowingRule,
    }[settings['effective_date_method']]

    args = []
    if settings.get('effective_date_param1'):
        args.append(settings['effective_date_param1'])
    elif settings.get('effective_date_param2'):
        args.append(settings['effective_date_param2'])

    return rule_class(*args)


@then("I should see the effective date is '{expected_effective_date}'")
def step_impl(context, expected_effective_date):
    # Set the open rule
    if hasattr(context, 'effective_date_open'):
        open_rule = create_rule(context.effective_date_open)
    else:
        open_rule = None

    # Set the ongoing rule
    if hasattr(context, 'effective_date_ongoing'):
        ongoing_rule = create_rule(context.effective_date_ongoing)
    else:
        ongoing_rule = None


    effective_date_calc = EffectiveDateCalculator(context.enrollment_start, context.enrollment_end, context.enrollment_ongoing,
                                                  open_rule=open_rule,
                                                  ongoing_rule=ongoing_rule)

    effective_date = effective_date_calc.get_effective_date_for_enroll_date(context.enroll_date)

    # We can use None as an error / invalid state for now
    if effective_date is None:
        effective_date = 'N/A'

    assert_that(effective_date, equal_to(expected_effective_date))


