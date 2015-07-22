import json
import urllib
from collections import defaultdict

from behave import use_step_matcher, given, then, when, step
from hamcrest import assert_that, equal_to, has_item, greater_than
from mock import Mock, call

use_step_matcher("parse")

from taa.services import LookupService, services_broker

def provide_mock(context, service_name, mock_name, obj=None):
    if not hasattr(context, mock_name):
        mock_service = obj if obj else Mock()
        services_broker.Provide(service_name, mock_service)
        setattr(context, mock_name, mock_service)

    return getattr(context, mock_name)

class MockUserService(object):
    def __init__(self):
        self.logged_in_user = None
        self.user_groups = defaultdict(list)

    def can_current_user_submit_enrollments(self):
        return 'api_users' in self.user_groups.get(self.logged_in_user, [])

class MockTokenService(object):
    def __init__(self):
        self.valid_tokens = []

    def is_valid_token(self, token):
        return token in self.valid_tokens


@given("I have a case that is enrolling with an api token 'CASE-123' and self-enroll token 'SE-123'")
def step_impl(context):
    """
    :type context behave.runner.Context
    """
    pass

@given("I have a user named {name} with token {user_token} in group {group}")
def step_impl(context, name, user_token, group):
    user_service = provide_mock(context, 'UserService', 'mock_user_service', MockUserService())
    user_service.user_groups[name].append(group)
    token_service = provide_mock(context, 'ApiTokenService', 'mock_token_service', MockTokenService())
    if group == 'api_users':
        token_service.valid_tokens.append(user_token)

@given("I log in as the user '{user_name}'")
def step_impl(context, user_name):
    user_service = provide_mock(context, 'UserService', 'mock_user_service', MockUserService())
    user_service.logged_in_user = user_name

@given(u"I create a minimally valid CSV file with case_token '{case_token}'")
def step_impl(context, case_token):
    context.csv_data = """\
USER_TOKEN,CASE_TOKEN,PRODUCT_CODE,PAYMENT_MODE,EMP_FIRST,EMP_LAST,EMP_SSN,EMP_BIRTHDATE,EMP_GENDER,EMP_COVERAGE,EMP_PREMIUM,\
EMP_STREET,EMP_STREET2,EMP_CITY,EMP_STATE,EMP_ZIPCODE,EMP_PHONE,EMP_PIN,EMP_SIG_TXT,APPLICATION_DATE,\
TIME_STAMP,SIGNED_AT_CITY,SIGNED_AT_STATE,AGENT_NAME,AGENT_CODE,AGENT_SIG_TXT,\
EMP_QUESTION_1_ANSWER,EMP_QUESTION_2_ANSWER,EMP_QUESTION_3_ANSWER,EMP_QUESTION_4_ANSWER,EMP_QUESTION_5_ANSWER,EMP_QUESTION_6_ANSWER,EMP_QUESTION_7_ANSWER
USER-123,{case_token},FPPTI,monthly,Joe,Smith,111223333,1980-01-31,m,50000,33.25,\
123 Sesame,,Chicago,IL,45555,,11441144,esigned by JOE SMITH,2015-01-01,\
2015-01-01T10:30:00,Chicago,IL,Test Agent,26TEST,esigned by TEST AGENT,\
n,n,n,n,n,n,n
""".format(case_token=case_token)

@step(u"I submit the enrollment data to the API using the auth_token '{auth_token}' and case_token '{case_token}'")
def step_impl(context, auth_token, case_token):
    # Override key services
    context.mock_case_service = Mock()
    services_broker.Provide("CaseService", context.mock_case_service)

    # Create params
    params = {'format':'csv'}
    if auth_token.strip():
        params['auth_token'] = auth_token
    if case_token.strip():
        params['case_token'] = case_token

    param_text = urllib.urlencode(params)
    if param_text:
        param_text = '?' + param_text

    # Make request
    context.resp = context.app.post("/enrollments{}".format(param_text),
                                    data=context.csv_data)

@step(u"I should see a {status_code:d} response")
def step_impl(context, status_code):
    assert_that(context.resp.status_code, equal_to(status_code), str(context.resp.data))

@then("I should see a positive number of records processed in the result")
def step_impl(context):
    json_body = json.loads(context.resp.get_data())
    assert_that(json_body['data']['num_processed'], greater_than(0))


@then("It should look up the case with token '{case_token}' in the database")
def step_impl(context, case_token):
    call_list = context.mock_case_service.mock_calls
    expected = call().get_case_by_token(case_token)
    assert_that(call_list, has_item(expected))

