
from dateutil.parser import parse
from behave import *
from hamcrest import assert_that, equal_to

from taa.services.enrollments.paylogix import get_week_from_date


@given("the draft day is '{draft_date}'")
def step_impl(context, draft_date):
    context.draft_date = parse(draft_date)


@when("I convert the date to a draft week")
def step_impl(context):
    context.result = get_week_from_date(context.draft_date)


@then("I should see '{week_int}'")
def step_impl(context, week_int):
    assert_that(context.result, equal_to(int(week_int)))