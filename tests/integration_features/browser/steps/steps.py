

from behave import *
from hamcrest import assert_that, equal_to
use_step_matcher("parse")

from taa.core import db
from taa.services import LookupService

from pages.login_page import LoginPage
from pages.case_enrollment import CaseEnrollmentPage
from pages.wizard_page import WizardPage
from tests.db_util import create_case, create_agent


@given("I have a case that is actively enrolling named '{case_name}'")
def step_impl(context, case_name):
    context.case = create_case(company_name=case_name)


@step("I have an agent '{agent_name}'")
def step_impl(context, agent_name):
    """
    creates agent in the database
    """
    context.agent = create_agent(first=agent_name.split()[0],
                                 last=agent_name.split()[1],
                                 agent_code='26BPT',
                                 email='test-agent@5starenroll.com',
                                 activated=True
                                 )


@step("I make '{agent_name}' a partner agent on '{case_name}'")
def step_impl(context, agent_name, case_name):
    """
    :type context behave.runner.Context
    """
    case_service = LookupService('CaseService')

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

