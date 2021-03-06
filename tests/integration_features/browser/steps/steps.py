import time

from behave import *
from hamcrest import assert_that, equal_to

use_step_matcher("parse")

from taa import db
from taa.services import LookupService

from pages.login_page import LoginPage
from pages.case_enrollment import CaseEnrollmentPage
from pages.wizard_page import WizardPage
from pages.case_setup import CasePage
from pages.self_enrollment import SelfEnrollmentPage
from tests.db_util import create_case, create_agent, create_case_call_center, create_case_self_enroll


case_service = LookupService('CaseService')


@given("I have a case that is actively enrolling named '{case_name}'")
def step_impl(context, case_name):
    context.case = create_case(company_name=case_name)


@given("I have a case that is actively enrolling named '{company_name}' with products")
def step_impl(context, company_name):
    product_codes = [r[0] for r in context.table.rows]
    context.case = create_case(agent=context.agent, company_name=company_name, product_codes=product_codes)


@given("I have a case that is actively enrolling named '{company_name}' with products and call center")
def step_impl(context, company_name):
    product_codes = [r[0] for r in context.table.rows]
    context.case = create_case_call_center(agent=context.agent, company_name=company_name, product_codes=product_codes)


@given("I have a case that is actively enrolling named '{company_name}' with products and is self enroll")
def step_impl(context, company_name):
    product_codes = [r[0] for r in context.table.rows]
    context.case = create_case_self_enroll(agent=context.agent, company_name=company_name, product_codes=product_codes)


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
        case_service.update_partner_agents(context.case, [a for a in context.case.partner_agents] + [context.agent])

    db.session.commit()


@step("I log in as '{agent_name}'")
def step_impl(context, agent_name):
    """
    :type context behave.runner.Context
    """
    login_page = LoginPage(context)
    login_page.navigate()
    login_page.login_as_agent(context.agent)


@step("I have logged in as an agent")
def step_impl(context):
    """
    We don't care about which agent, just create one and log in
    """
    context.execute_steps(u"""
    Given I have an agent 'Agent Bob'
    And I log in as 'Agent Bob'
    """)


@step("I navigate to the case setup page")
def step_impl(context):
    """
    :type context behave.runner.Context
    """
    case_enrollment_page = CaseEnrollmentPage(context)

    case_enrollment_page.navigate_to_setup_page()


@step("I click on the self enroll link")
def step_impl(context):
    """
    :type context behave.runner.Context
    """
    case_setup_page = CasePage(context)

    time.sleep(2)
    case_setup_page.switch_to_popup_window()


@then("I should click the self enroll button")
def step_impl(context):
    """
    :type context behave.runner.Context
    """
    time.sleep(2)
    self_enroll_page = SelfEnrollmentPage(context)

    time.sleep(2)
    self_enroll_page.click_self_enroll_btn()


@step("I navigate to the enrollment page for '{case_name}'")
def step_impl(context, case_name):
    """
    :param case_name: Name of the case to navigate to
    :type context behave.runner.Context
    """
    cases = case_service.search_cases(by_name=case_name)
    case = cases[0]
    context.case = case
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


@step("I begin a new empty enrollment for the case '{case_name}'")
def step_impl(context, case_name):
    """
    :type context behave.runner.Context
    """
    context.execute_steps(u"""
        When I navigate to the enrollment page for '{}'
        And I add a new enrollment with '{}' as the SSN
        """.format(case_name, '123121234')
                          )


@when("I enter the following information into the wizard step 1")
def step_impl(context):
    """
    :type context behave.runner.Context
    """
    wizard_page = WizardPage(context)
    did_load = wizard_page.wait_until_loaded()
    assert_that(did_load, equal_to(True))

    time.sleep(5)
    wizard_page.wait_until_step_1_loaded()
    time.sleep(0.25)
    data = get_data_from_first_row_of_table(context)
    wizard_page.fill_out_step1_data(**data)
    if data.get('emp_coverage'):
        if data['emp_coverage'].isdigit():
            wizard_page.select_custom_coverage('employee', data['emp_coverage'])
        else:
            wizard_page.select_recommended_coverage(data['emp_coverage'])

    wizard_page.click_next()


@step("I select 'No' for every question on step 2 of the wizard")
def step_impl(context):
    """
    :type context behave.runner.Context
    """
    wizard_page = WizardPage(context)
    did_load = wizard_page.wait_until_step_2_loaded()
    assert_that(did_load, equal_to(True))

    wizard_page.select_no_for_all_questions()
    wizard_page.click_next()


@step("I enter the following data for step 3 of the wizard")
def step_impl(context):
    """
    :type context behave.runner.Context
    """
    wizard_page = WizardPage(context)
    did_load = wizard_page.wait_until_step_3_loaded()
    assert_that(did_load, equal_to(True))

    wizard_page.fill_out_step3_data(**(get_data_from_first_row_of_table(context)))
    wizard_page.click_next()


@step("I enter nothing for step 4 of the wizard")
def step_impl(context):
    """
    :type context behave.runner.Context
    """
    wizard_page = WizardPage(context)
    did_load = wizard_page.wait_until_step_4_loaded()
    assert_that(did_load, equal_to(True))

    wizard_page.click_next()


@step("I enter the following data for step 4 of the wizard")
def step_impl(context):
    """
    :type context behave.runner.Context
    """
    wizard_page = WizardPage(context)
    did_load = wizard_page.wait_until_step_4_loaded()
    assert_that(did_load, equal_to(True))

    wizard_page.fill_out_step_4_data(**get_data_from_first_row_of_table(context))

    wizard_page.click_next()


@step("I enter the following for step 5 of the wizard")
def step_impl(context):
    """
    :type context behave.runner.Context
    """
    wizard_page = WizardPage(context)
    did_load = wizard_page.wait_until_step_5_loaded()
    assert_that(did_load, equal_to(True))

    wizard_page.fill_out_step_5_data(**get_data_from_first_row_of_table(context))
    wizard_page.click_next()


@step("I enter the following for step 6 of the wizard")
def step_impl(context):
    """
    :type context behave.runner.Context
    """
    wizard_page = WizardPage(context)
    did_load = wizard_page.wait_until_step_6_loaded()
    assert_that(did_load, equal_to(True))

    wizard_page.fill_out_step_6_data(**get_data_from_first_row_of_table(context))
    wizard_page.click_next()


@then("I should be redirected to the DocuSign website")
def step_impl(context):
    """
    :type context behave.runner.Context
    """
    wizard_page = WizardPage(context)
    did_redirect = wizard_page.wait_until_docusign_redirect()
    time.sleep(2)
    assert_that(did_redirect, equal_to(True))


@step("I click on Back to Home")
def step_impl(context):
    """
    :type context behave.runner.Context
    """
    wizard_page = WizardPage(context)
    wizard_page.click_home_btn()


@step("I click on that person's Enrollment")
def step_impl(context):
    """
    :type context behave.runner.Context
    """
    wizard_page = WizardPage(context)
    time.sleep(3)

    wizard_page.find_enrollment()
    time.sleep(2)


@then("I should see that the person Declined")
def step_impl(context):
    """
    :type context behave.runner.Context
    """
    wizard_page = WizardPage(context)
    time.sleep(5)
    val = wizard_page.find_status_decline()
    assert_that(val, equal_to(True))


@then("I should see that the person is Enrolled")
def step_impl(context):
    """
    :type context behave.runner.Context
    """
    wizard_page = WizardPage(context)
    time.sleep(5)
    assert_that(wizard_page.find_status_enrolled(), equal_to(True))


@then("I should be redirected to the Home page")
def step_impl(context):
    """
    :type context behave.runner.Context
    """
    wizard_page = WizardPage(context)
    did_redirect = wizard_page.wait_until_home_page()
    time.sleep(2)
    assert_that(did_redirect, equal_to(True))


@then("I should be redirected to the Application Declined page")
def step_impl(context):
    """
    :type: context behave.runner.Context
    """
    wizard_page = WizardPage(context)
    did_redirect = wizard_page.wait_until_application_decline()
    time.sleep(2)
    assert_that(did_redirect, equal_to(True))


@then("I should be prompted to close the browser window")
def step_impl(context):
    """
    :type context behave.runner.Context
    """
    case_page = CasePage(context)
    did_redirect = case_page.wait_until_redirected_declined()
    assert_that(did_redirect, equal_to(True))


@then("I should be redirected to the Enrollment Page")
def step_impl(context):
    """
    :type context behave.runner.Context
    """
    wizard_page = WizardPage(context)
    did_redirect = wizard_page.wait_until_can_enroll_applicant()
    time.sleep(2)
    assert_that(did_redirect, equal_to(True))


@step("I check all boxes on the Agreement disclaimer")
def step_impl(context):
    """
    :type context behave.runner.Context
    """
    wizard_page = WizardPage(context)
    time.sleep(3)

    wizard_page.select_app_sig_options_as_agent()
    wizard_page.click_agent_sign()


@step("I click sign on the Other Insurance Questions")
def step_impl(context):
    """
    :type context behave.runner.Context
    """
    wizard_page = WizardPage(context)

    time.sleep(2)
    wizard_page.submit_agent_sig()
    time.sleep(10)


def get_data_from_first_row_of_table(context):
    return dict(zip(context.table[0].headings, context.table[0].cells))


@step("I select the following recommended coverages")
def step_impl(context):
    """
    :type context: behave.runner.Context
    """
    product_coverage_pairs = [(row[0], row[1]) for row in context.table.rows]
    wizard_page = WizardPage(context)
    wizard_page.click_show_rates()
    time.sleep(0.5)
    wizard_page.select_coverage_options(product_coverage_pairs)
    wizard_page.click_next()
