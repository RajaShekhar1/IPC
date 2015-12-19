from decimal import Decimal

from behave import *
from hamcrest import assert_that, equal_to


from taa.services.products.RatePlan import (
    ApplicantQuery,
    ApplicantDemographics,
    ApplicantQueryOptions, load_rate_plan_for_base_product)



@when("I lookup premiums by coverage with the above data for the following ages")
def step_impl(context):

    context.computed_premiums = {}
    applicant_query_data = [dict(zip(row.headings, row.cells)) for row in context.table]
    for row in applicant_query_data:
        premiums = {}
        for coverage in context.rate_plan.get_coverage_options_by_face(None):
            applicant_query = ApplicantQuery(
                demographics=ApplicantDemographics({'age': int(row['Age'])}),
                applicant_type=row['Applicant Type'],
                mode=int(row['Mode']),
                product_options={'riders': get_riders(row)},
                rate_options=ApplicantQueryOptions({'by_coverage': coverage}),
                state='IN'
            )
            premium = context.rate_plan.calculate_premium(applicant_query)
            premiums[coverage] = premium
        context.computed_premiums[int(row['Age'])] = premiums

def get_riders(row):
    riders = []
    if row.get('WP Rider') == 'Y':
        riders.append('WP')
    if row.get('QOL Rider') == 'Y':
        riders.append('QOL3')
    if row.get('AIR Rider') == 'Y':
        riders.append('AIR')
    return riders

@then("I should see the following premiums")
def step_impl(context):
    expected_premiums = [dict(zip(row.headings, row.cells)) for row in context.table]
    for row in expected_premiums:
        age = int(row["Age"])
        for coverage in context.rate_plan.get_coverage_options_by_face(None):
            computed = context.computed_premiums[age][coverage]
            formatted_coverage = format_coverage(coverage)
            expected = row[formatted_coverage].replace('$', '').replace(',', '')
            if expected == 'N/A':
                expected = None
            else:
                expected = Decimal(expected)

            assert_that(computed, equal_to(expected), "Premium for %s %s"%(age, coverage))


def format_coverage(coverage):
    "Add dollar sign and thousands separator to coverage integer."
    import locale
    locale.setlocale(locale.LC_ALL, 'en_US.UTF-8')
    return '$' + locale.format("%d", coverage, grouping=True)


@when("I lookup coverages by premium with the above data for the following ages")
def step_impl(context):
    context.computed_coverages = {}
    applicant_query_data = [dict(zip(row.headings, row.cells)) for row in context.table]
    for row in applicant_query_data:
        coverages = {}
        for premium in context.rate_plan.get_premium_options(int(row['Mode'])):
            applicant_query = ApplicantQuery(
                demographics=ApplicantDemographics({'age': int(row['Age'])}),
                applicant_type=row['Applicant Type'],
                mode=int(row['Mode']),
                product_options={'riders': get_riders(row)},
                rate_options=ApplicantQueryOptions({'by_premium': premium}),
                state='IN'
            )
            coverage = context.rate_plan.calculate_coverage(applicant_query)
            coverages[premium] = coverage
        context.computed_coverages[int(row['Age'])] = coverages
        # Save the mode we used for next step
        context.computed_coverages['mode'] = int(row['Mode'])


def format_premium(premium):
    import locale
    locale.setlocale(locale.LC_ALL, 'en_US.UTF-8')
    return '$' + locale.format("%.2f", premium, grouping=True)


@then("I should see the following coverages")
def step_impl(context):
    expected_coverages = [dict(zip(row.headings, row.cells)) for row in context.table]
    for row in expected_coverages:
        age = int(row["Age"])
        mode = int(row["Mode"])
        for premium in context.rate_plan.get_premium_options(mode):
            computed = context.computed_coverages[age][premium]
            formatted_coverage = format_coverage(computed)
            formatted_premium = format_premium(premium)
            expected = row[formatted_premium]
            if expected == None:
                expected = 'N/A'

            assert_that(formatted_coverage, equal_to(expected), "Error in computed coverage for age %s, premium %s"%(age, premium))


@given("I want rates for the '{product}' product")
def step_impl(context, product):
    context.product = product
    context.rate_plan = load_rate_plan_for_base_product(product)
