import json
import urllib
from collections import defaultdict

from behave import use_step_matcher, given, then, when, step
import datetime

from hamcrest import assert_that, equal_to, has_item, greater_than
from mock import Mock, call


from taa.core import db
from taa.models import EnrollmentApplication
from taa.services import LookupService
from tests.db_util import create_case, create_user_in_groups

use_step_matcher("parse")

test_agent_stormpath_url = 'https://api.stormpath.com/v1/accounts/2qQtvZLi6tpUGjXFYtVSRK'


@given("I have a case that is enrolling with an api token '{case_token}' and self-enroll token '{self_enroll_token}'")
def step_impl(context, case_token, self_enroll_token):
    # Create case
    context.case = create_case(case_token=case_token)


@given("I have a user named {name} with token {user_token} in group {group}")
def step_impl(context, name, user_token, group):
    create_user_in_groups(name, api_token=user_token, groups=[group])


@given("I log in as the user '{user_name}'")
def step_impl(context, user_name):
    # Will interact with UserService and / or flask session - maybe should also use stormpath api
    pass


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




@given("I deactivate the case with token '{case_token}'")
def step_impl(context, case_token):
    case_service = LookupService("CaseService")
    case = case_service.get_case_for_token(case_token)
    case.active = False
    db.session.commit()


@then("I should see an enrollment record in the database with the following data")
def step_impl(context):

    case = context.case

    from sqlalchemy import create_engine
    from sqlalchemy.orm import create_session
    engine = create_engine('postgresql://taa:fQj9lJTFbOQUBYo@localhost/taa-test')
    db_session = create_session(engine)
    enrollment_data = db_session.query(EnrollmentApplication).first()
    assert enrollment_data, "Enrollment data was not saved"

    for key, expected_value in context.table[0].items():
        assert_that("%s"%getattr(enrollment_data, key), equal_to(expected_value))
