from behave import use_step_matcher, given, then, when, step
from hamcrest import assert_that, equal_to, has_items

use_step_matcher("parse")

from taa.services import LookupService
enrollment_import_service = LookupService('EnrollmentImportService')

@given(u"I have an API User named {user_name} with token {user_token}")
def step_impl(context, user_name, user_token):
    if not hasattr(context, 'users'):
        context.users = []

    context.users.append(APIUser(user_name, user_token))

@given("I have a Case with the token {token}")
def step_impl(context, token):
    context.case_token = token

@given("The following are valid product codes")
def step_impl(context):
    context.valid_product_codes = context.table.rows

@given(u'I add the following enrollment data columns')
def step_impl(context):
    if not hasattr(context, 'import_record'):
        context.import_record = dict()

    table = context.passed_table if hasattr(context, 'passed_table') else context.table
    for row in table:
        context.import_record.update(dict(zip(row.headings, row.cells)))

@given(u'I prepare an enrollment file with data')
def step_impl(context):
    context.passed_table = context.table
    context.execute_steps(u"Given I add the following enrollment data columns")


@given("I prepare an enrollment file with basic valid enrollment data")
def step_impl(context):
    context.import_record = dict(
        user_token='ABC',
        case_token='XYZ',
        product_code='FPPTI',
        payment_mode='weekly',
        emp_first='Joe',
        emp_last='Johnson',
        emp_ssn='123121234',
        emp_birthdate='01/31/1980',
        emp_coverage='50000',
        emp_premium='10.00',
        emp_street='123 Sesame',
        emp_street2='',
        emp_city='Indianapolis',
        emp_state='IN',
        emp_zipcode='47999',
        emp_phone='',
        emp_pin='12341234',
        emp_sig_txt='esign by Joe Johnson',
        emp_application_date='06/01/2015',
        time_stamp='06/01/2015T23:00:00',
        signed_at_city='Lansing',
        signed_at_state='MI',
        agent_name='Andy Agent',
        agent_code='26CODE',
        agent_sig_txt='esign by Andy Agent'
    )

@given(u"I add valid spouse enrollment data")
def step_impl(context):
    context.import_record.update(dict(
        sp_first="Jane",
        sp_last="Doe",
        sp_birthate="1990-01-01",
        sp_ssn="123-33-4444",
        sp_premium="3.00",
        sp_coverage="10000",
    ))

@given(u'I add valid child enrollment data')
def step_impl(context):
    context.import_record.update(dict(
        ch1_first="Johnny",
        ch1_last="Doe",
        ch1_birthate="2009-01-01",
        ch1_ssn="126-66-7777",
        ch1_premium="2.50",
        ch1_coverage="10000",
    ))

@given(u"I substitute '{bad_value}' for the column '{column_name}'")
def step_impl(context, bad_value, column_name):
    context.import_record[column_name] = bad_value

@given(u"I clear the data on column '{column_name}'")
def step_impl(context, column_name):
    context.import_record[column_name] = ""

@when(u"I submit the file to the Enrollment API")
def step_impl(context):
    context.result = enrollment_import_service.submit_file_records([context.import_record])

@then(u'I should see a success response')
def step_impl(context):
    assert_that(context.result.is_success(), equal_to(True))


@then(u'I should see the following errors in the response')
def step_impl(context):
    expected_errors = [(row['error_type'], row['error_field']) for row in context.table]
    actual_errors = [(e.get_type(), e.get_fields()[0] if e.get_fields() else "")
                     for e in context.result.get_errors()]

    assert_that(actual_errors, has_items(expected_errors))
    assert_that(context.result.is_error(), equal_to(True))

# Environment stubs

class APIUser(object):
    def __init__(self, user_name, user_token):
        self.user_name = user_name
        self.user_token = user_token

