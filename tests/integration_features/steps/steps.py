import datetime

from behave import *
from hamcrest import assert_that, equal_to
use_step_matcher("parse")

from taa.core import db
from taa.services import LookupService
from taa.services.cases.models import Case, CaseOpenEnrollmentPeriod
from taa.services.products import payment_modes
from pages.login_page import LoginPage
from pages.case_enrollment import CaseEnrollmentPage
from pages.wizard_page import WizardPage

test_agent_stormpath_url = 'https://api.stormpath.com/v1/accounts/2qQtvZLi6tpUGjXFYtVSRK'
test_agent_stormpath_url2 = 'https://api.stormpath.com/v1/accounts/3vwaoifZRl8Z1pHIzdvjwe'

@given("I have a case that is actively enrolling named '{case_name}'")
def step_impl(context, case_name):
    case_service = LookupService('CaseService')
    product_service = LookupService('ProductService')
    agent_service = LookupService('AgentService')
    agent = agent_service.find(stormpath_url=test_agent_stormpath_url).first()
    context.case = case_service.create_new_case(**dict(
        company_name=case_name,
        group_number="GRP-NUM-EX123",
        situs_state='MI',
        situs_city='Lansing',
        agent_id=agent.id,
        active=True,
        created_date=datetime.datetime(year=2012, month=1, day=1),
        enrollment_period_type=Case.OPEN_ENROLLMENT_TYPE,
        payment_mode=payment_modes.MODE_MONTHLY,
        is_self_enrollment=False,
    ))
    fppti = product_service.search(by_code='FPPTI')[0]
    context.case.products.append(fppti)

    periods = [dict(period_type=CaseOpenEnrollmentPeriod.PERIOD_TYPE, start_date='2000-01-01', end_date=None)]
    case_service.update_enrollment_periods(context.case, periods)

    db.session.commit()


@step("I have an agent '{agent_name}'")
def step_impl(context, agent_name):
    """
    creates agent in the database
    """
    agent_service = LookupService('AgentService')
    context.agent = agent_service.find(stormpath_url=test_agent_stormpath_url2).first()


@step("I make '{agent_name}' a partner agent on '{case_name}'")
def step_impl(context, agent_name, case_name):
    """
    :type context behave.runner.Context
    """
    case_service = LookupService('CaseService')
    agent_service = LookupService('AgentService')

    if context.agent not in context.case.partner_agents:
        case_service.update_partner_agents(context.case, [a for a in context.case.partner_agents]+[context.agent])

    db.session.commit()

@step("I log in as '{agent_name}'")
def step_impl(context, agent_name):
    """
    :type context behave.runner.Context
    """
    login_page = LoginPage(context)
    login_page.navigate()
    login_page.login_as_agent(context.agent)


@step("I navigate to the enrollment page for '{case_name}'")
def step_impl(context, case_name):
    """
    :type context behave.runner.Context
    """
    case_enrollment_page = CaseEnrollmentPage(context)
    case_enrollment_page.navigate()


@when("I add a new enrollment with '{ssn}' as the SSN")
def step_impl(context, ssn):
    """
    :type context behave.runner.Context
    """
    case_enrollment_page = CaseEnrollmentPage(context)
    case_enrollment_page.add_enrollment(ssn=ssn)


@then("I should be redirected to the enrollment wizard")
def step_impl(context):
    """
    :type context behave.runner.Context
    """
    wizard_page = WizardPage(context)
    did_load = wizard_page.wait_until_loaded()
    assert_that(did_load, equal_to(True))

