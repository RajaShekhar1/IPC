from behave import *
from hamcrest import assert_that, equal_to, has_items, has_item

from taa.services.products.riders import RiderConfiguration
from taa.services.products.riders import RiderService


@given("I am setting up a case enrolling the '{product}' base product")
def step_impl(context, product):
    context.product = product


@when("I check the following riders for compatibility")
def step_impl(context):
    rows = [dict(zip(row.headings, row.cells)) for row in context.table]
    context.riders = [code for code in rows[0] if rows[0][code] == 'Y']

    rider_config = RiderConfiguration(context.product)
    context.rider_compatibility = rider_config.check_compatibility(context.riders)


@then("It should say the riders are allowed.")
def step_impl(context):
    assert_that(context.rider_compatibility.is_compatible(), equal_to(True))


@then("It should show that the rider '{rider_code}' is not compatible with this product.")
def step_impl(context, rider_code):
    assert_that(context.rider_compatibility.is_compatible(), equal_to(False))

    bad_riders = set([r for r in context.rider_compatibility.get_incompatible_riders()])
    assert_that(bad_riders, has_item(rider_code))


@then("It should show that the riders '{rider_code1}' and '{rider_code2}' cannot be combined.")
def step_impl(context, rider_code1, rider_code2):
    assert_that(context.rider_compatibility.is_compatible(), equal_to(False))

    bad_riders = set([r for r in context.rider_compatibility.get_incompatible_riders()])
    expected_bad_riders = {rider_code1, rider_code2}
    assert_that(bad_riders, equal_to(expected_bad_riders))
