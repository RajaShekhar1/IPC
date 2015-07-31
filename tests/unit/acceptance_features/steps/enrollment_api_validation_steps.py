from behave import use_step_matcher, given, then, when, step
from hamcrest import assert_that, equal_to, has_items, has_item, greater_than_or_equal_to

use_step_matcher("parse")

from taa.services import LookupService, services_broker
enrollment_parser_service = LookupService("EnrollmentRecordParser")

# Environment stubs
class MockApiTokenService(object):
    def __init__(self):
       self.valid_tokens = {}

    def is_valid_token(self, token):
        return token in self.valid_tokens

class MockCaseService(object):
    def __init__(self):
        self.valid_tokens = {}

    def is_valid_case_token(self, token):
        return token in self.valid_tokens

    def get_case_for_token(self, token):
        class Case:
            id = 2
        return Case()

    def is_enrolling(self, case):
        return True

class MockProductService(object):
    def __init__(self):
        self._valid_product_codes = {}
        self.health_questions = {}
        self.valid_statecodes = {}

    @property
    def valid_product_codes(self):
        return self._valid_product_codes

    @valid_product_codes.setter
    def valid_product_codes(self, value):
        for v in value:
            self.health_questions[v] = {
                "employee": [],
                "spouse": [],
                "child": []
            }
            self.valid_statecodes[v] = [u"IN", u"MI", u"OH"]
        self._valid_product_codes = value

    def is_valid_product_code(self, code):
        return code in self.valid_product_codes

    def get_num_health_questions(self, product_code, statecode, applicant_type):
        return len(self.health_questions[product_code][applicant_type])

    def is_valid_statecode_for_product(self, product_code, statecode):
        return statecode in self.valid_statecodes[product_code]

    def invalidate_statecode(self, product_code, statecode):
        if self.is_valid_statecode_for_product(product_code, statecode):
            self.valid_statecodes[product_code].remove(statecode)


@given(u"I have an API User named {user_name} with token {user_token}")
def step_impl(context, user_name, user_token):
    if not hasattr(context, 'mock_token_service'):
        context.mock_token_service = MockApiTokenService()
        services_broker.Provide('ApiTokenService', context.mock_token_service)

    context.mock_token_service.valid_tokens[user_token] = user_name

@given(u"I have a Case with the token {token}")
def step_impl(context, token):
    if not hasattr(context, 'mock_case_service'):
        context.mock_case_service = MockCaseService()
        services_broker.Provide('CaseService', context.mock_case_service)

    context.mock_case_service.valid_tokens[token] = True


@given(u"The following are valid product codes")
def step_impl(context):
    if not hasattr(context, 'mock_product_service'):
        context.mock_product_service = MockProductService()
        services_broker.Provide('ProductService', context.mock_product_service)

    context.mock_product_service.valid_product_codes = [r[0] for r in context.table.rows]

@given(u'I add the following enrollment data columns')
def step_impl(context):
    if not hasattr(context, 'import_record'):
        context.import_record = dict()

    table = context.table if getattr(context, 'table') else context.passed_table
    for row in table:
        context.import_record.update(dict(zip(row.headings, row.cells)))

@step("I remove the column '{col_name}'")
def step_impl(context, col_name):
    del context.import_record[col_name]

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
        payment_mode='52',
        enrollment_type='S',
        emp_first='Joe',
        emp_last='Johnson',
        emp_gender='m',
        emp_ssn='123121234',
        emp_birthdate='1980-01-31',
        emp_coverage='50000',
        emp_premium='10.00',
        emp_street='123 Sesame',
        emp_street2='',
        emp_city='Indianapolis',
        emp_state='IN',
        emp_zipcode='47999',
        emp_phone='',
        emp_pin='12341234',
        emp_date_of_hire="2012-01-01",
        emp_sig_txt='esign by Joe Johnson',
        application_date='2015-06-01',
        time_stamp='2015-06-01 23:00:00',
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
        sp_gender="f",
        sp_birthdate="1990-01-01",
        sp_ssn="123-33-4444",
        sp_premium="3.00",
        sp_coverage="10000",
    ))

@given(u'I add valid child enrollment data')
def step_impl(context):
    context.import_record.update(dict(
        ch1_first="Johnny",
        ch1_last="Doe",
        ch1_birthdate="2009-01-01",
        ch1_ssn="126-66-7777",
        ch1_premium="2.50",
        ch1_coverage="10000",
    ))

@given(u'I add a valid second child enrollment data')
def step_impl(context):
    context.import_record.update(dict(
        ch2_first="Mary",
        ch2_last="Doe",
        ch2_birthdate="2009-12-01",
        ch2_ssn="124-44-8888",
        ch2_premium="2.50",
        ch2_coverage="10000",
    ))

@given(u"I add valid optional enrollment data")
def step_impl(context):
    context.import_record.update(dict(
        actively_at_work="Y",
        emp_email="joe@gmail.com",
        emp_date_of_hire="2010-01-31",
        emp_height_inches="70",
        emp_weight_pounds="150",
        emp_smoker="N",
        sp_height_inches="65",
        sp_weight_pounds="130",
        sp_smoker="N",
        sp_email="spouse@spouse.com",
        sp_street="Other st",
        sp_street2="",
        sp_city="Chicago",
        sp_state="IL",
        sp_zipcode="11444",
        sp_phone="1242223535",
        existing_insurance="N",
        replacing_insurance="N",
        sp_treated_6_months="N",
        sp_disabled_6_months="N",
        replacement_read_aloud="N",
        replacement_is_terminating="N",
        replacement_using_funds="N",
        replacement_policy1_name="Prudential",
        replacement_policy1_number="111AAA33",
        replacement_policy1_insured="Joe",
        replacement_policy1_replaced_or_financing="R",
        replacement_policy1_reason="Needed better coverage",
        emp_bene_name="Emp. Prim. Bene",
        emp_bene_birthdate="1990-10-10",
        emp_bene_relationship="Brother",
        emp_bene_ssn="555-55-5555",
        sp_bene_name="Sp prim bene",
        sp_bene_birthdate="1980-11-11",
        sp_bene_relationship="daughter",
        sp_bene_ssn="111-11-1112",
        emp_cont_bene_name="Emp. Cont. Bene ",
        emp_cont_bene_birthdate="1989-01-10",
        emp_cont_bene_relationship="Relative",
        emp_cont_bene_ssn="666-55-5555",
        sp_cont_bene_name="Sp cont bene",
        sp_cont_bene_birthdate="1985-11-12",
        sp_cont_bene_relationship="friend",
        sp_cont_bene_ssn="121-12-1112"
    ))

@given(u"I substitute '{bad_value}' for the column '{column_name}'")
def step_impl(context, bad_value, column_name):
    context.import_record[column_name] = bad_value

@given(u"I clear the data on column '{column_name}'")
def step_impl(context, column_name):
    context.import_record[column_name] = ""

@given(u"The product '{product_code}' has the following health questions")
def step_impl(context, product_code):
    if not hasattr(context, 'mock_product_service'):
        context.mock_product_service = MockProductService()
        services_broker.Provide('ProductService', context.mock_product_service)
    for r in context.table.rows:
        context.mock_product_service.health_questions[product_code][r[0]].append(r[1])

@given(u"'{statecode}' is not a valid state for the '{product_code}' product")
def step_impl(context, statecode, product_code):
    context.mock_product_service.invalidate_statecode(product_code, statecode)

@when(u"I submit the file to the Enrollment API")
def step_impl(context):
    parser = enrollment_parser_service()
    parser.process_records([context.import_record], case=None)
    context.errors = parser.errors
    context.records = parser.get_valid_data()

@then(u'I should see a success response')
def step_impl(context):
    assert_that(len(context.errors), equal_to(0), "Errors in response: {}".format(context.errors))


@then(u'I should see the following errors in the response')
def step_impl(context):
    expected_errors = [(row['error_type'], row['error_field']) for row in context.table]
    actual_errors = [(e['type'], e['field_name'] if e['field_name'] else "")
                     for e in context.errors]

    assert_that(actual_errors, has_items(*expected_errors))
    #assert_that(len(context.errors), greater_than_or_equal_to(1))


@then("the parsed record should include the following attributes")
def step_impl(context):
    record = context.records[0]
    keys = [row['attribute_name'] for row in context.table]
    for key in keys:
        assert_that(record.keys(), has_item(key))
