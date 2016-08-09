from decimal import Decimal

from behave import *
from hamcrest import assert_that, equal_to

from taa.services.products.RatePlan import ApplicantQuery, ApplicantDemographics, ApplicantQueryOptions
from taa.services.products.plan_codes import get_plan_code


@given("The applicant type is '{applicant_type}'")
def step_impl(context, applicant_type):
    context.applicant_type = applicant_type


@step("the {rider_code} rider state is '{rider_state}'")
def step_impl(context, rider_code, rider_state):
    if not hasattr(context, 'riders'):
        context.riders = {}

    context.riders[rider_code] = rider_state == 'Y'


@step("the state is '{state}'")
def step_impl(context, state):
    context.state = state


@when("I look up the plan code for base product '{base_product_code}'")
def step_impl(context, base_product_code):
    context.base_product_code = base_product_code


@then("I should see the plan code '{expected_plan_code}'")
def step_impl(context, expected_plan_code):
    # Create the applicant request

    query = ApplicantQuery(
        applicant_type=context.applicant_type.lower(),
        product_options={'riders': get_rider_list(context)},
        demographics=ApplicantDemographics(demographics_object={'applicant_type': context.applicant_type}),
        state=context.state,
        mode=52,
        rate_options=ApplicantQueryOptions({})
    )

    plan_code = get_plan_code(context.base_product_code, query)

    # If a query does not have a plan code, we use 'None' as expected output.
    if not plan_code:
        plan_code = "None"

    assert_that(plan_code, equal_to(expected_plan_code))


def get_rider_list(context):
    rider_list = []
    if context.riders['AIR']:
        rider_list.append('AIR')
    if context.riders['WP']:
        rider_list.append('WP')
    if context.riders['QOL3']:
        rider_list.append('QOL3')
    if context.riders['QOL4']:
        rider_list.append('QOL4')

    return rider_list
