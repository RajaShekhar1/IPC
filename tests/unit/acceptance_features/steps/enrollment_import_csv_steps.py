
from behave import use_step_matcher, given, then, when, step
from hamcrest import assert_that, equal_to, has_items, has_entries

use_step_matcher("parse")

from taa.services import LookupService
enrollment_import_service = LookupService('EnrollmentImportService')

@step(u"I have prepared the following csv file")
def step_impl(context):
    context.csv_text = context.text

@step(u"I convert the CSV to JSON")
def step_impl(context):
    context.result = enrollment_import_service.convert_csv_to_json(context.csv_text)

@step(u"I should see the following JSON data")
def step_impl(context):
    expected = dict(zip(context.table[0].headings, context.table[0].cells))
    assert_that(context.result, has_entries(expected))
