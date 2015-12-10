"""
AGENT pages and DOCUSIGN inbox
"""
import os

from flask import render_template, redirect, url_for, flash, send_file, request
from flask_stormpath import login_required, groups_required, current_user

from taa import app, db
from nav import get_nav_menu
from taa.api.cases import census_records
from taa.services.docusign.docu_console import console_url
from taa.services.cases import CaseService, SelfEnrollmentSetup
from taa.services.products.riders import RiderService
from taa.services.cases.forms import (CensusRecordForm,
                                      NewCaseEnrollmentPeriodForm,
                                      SelfEnrollmentSetupForm,
                                      UpdateCaseForm
                                      )
from taa.services.enrollments import SelfEnrollmentLinkService, SelfEnrollmentEmailService, EnrollmentApplicationService
from taa.services.agents import AgentService
from taa.services.products import ProductService, get_all_states
from taa.services.products import get_payment_modes
from taa.services.docusign.DocuSign_config import sessionUserApprovedForDocusign
from taa.services import LookupService

case_service = CaseService()
rider_service = RiderService()
agent_service = AgentService()
product_service = ProductService()
enrollment_service = EnrollmentApplicationService()
self_enrollment_link_service = SelfEnrollmentLinkService()
self_enrollment_email_service = SelfEnrollmentEmailService()

@app.route('/inbox', methods=['GET'])
@login_required
def inbox():
    if sessionUserApprovedForDocusign():
        return render_template('agent/agent-inbox.html',
                               inboxURL=console_url(),
                               nav_menu=get_nav_menu())
    else:
        flash("You are not yet authorized for signing applications. "
              "Please see your Regional Director for assistance.")
        return redirect(url_for('home'))


@app.route("/enrollment-cases")
@groups_required(["agents", "home_office", "admins"], all=False)
@login_required
def manage_cases():
    agent = agent_service.get_logged_in_agent()

    if agent:
        user_cases = case_service.search_cases(by_agent=agent.id)
        header_title = ''
        can_create_case = False
    else:
        # Admin or home office user
        user_cases = case_service.search_cases()
        header_title = 'Home Office'
        can_create_case = True

    vars = {
        'agent_cases': user_cases,
        'all_states': get_all_states(),
        'nav_menu': get_nav_menu(),
        'header_title': header_title,
        'can_create_case': can_create_case
    }
    return render_template('agent/manage_cases.html', **vars)


@app.route('/enrollment-case/<case_id>')
@groups_required(['agents', 'home_office', 'admins'], all=False)
def manage_case(case_id):
    api_token_service = LookupService('ApiTokenService')
    case = case_service.get_if_allowed(case_id)
    vars = {'case': case, 'can_edit_case': False}

    agent = agent_service.get_logged_in_agent()
    if agent:
        products = product_service.get_products_for_agent(agent)
        is_agent_case_owner = case_service.is_agent_case_owner(agent, case)
        # No agents can edit cases anymore
        vars['can_edit_case'] = False
        vars['can_download_enrollments'] = case_service.is_agent_allowed_to_view_full_census(agent, case)
        vars['can_view_report_tab'] = case_service.is_agent_allowed_to_view_full_census(agent, case)
        agent_name = agent.name()
        agent_id = agent.id
        agent_email = agent.email
    else:
        # Admin or home office
        products = product_service.get_all_enrollable_products()
        vars['is_admin'] = True
        vars['can_edit_case'] = True
        vars['can_download_enrollments'] = True
        vars['can_view_report_tab'] = True
        vars['active_agents'] = agent_service.get_active_agents()
        vars['header_title'] = 'Home Office'
        agent_name = ""
        agent_id = None
        agent_email = ""

    vars['case_agents'] = case_service.get_agents_for_case(case)
    vars['product_choices'] = products
    vars['all_states'] = get_all_states()
    vars['payment_modes'] = get_payment_modes()
    vars['product_state_mapping'] = product_service.get_product_states(products)

    case_setup_form = UpdateCaseForm(obj=case)
    vars['case_setup_form'] = case_setup_form

    enrollment_periods = NewCaseEnrollmentPeriodForm(
        **case_service.get_case_enrollment_period_data(case))
    vars['enrollment_period_form'] = enrollment_periods

    vars['nav_menu'] = get_nav_menu()

    # Has active enrollments?
    vars['case_has_enrollments'] = case_service.does_case_have_enrollments(case)

    vars['default_email_message'] = """
This is your online enrollment notice for {company_name} benefits from 5Star Life Insurance Company. As an employee/affiliate of {company_name}, you are eligible for the benefits listed below. <strong><emphasis>This email is unique to you specifically</strong></emphasis>; please do not forward this link to anyone else, as it will contain access to your personal information. <br>
<br>
Nearly 85% of Americans say most people need life insurance, yet only 62% have coverage and a staggering 33% say they don't have enough life insurance, including one-fourth who already have life insurance coverage. Financial experts recommend having enough life insurance to replace income for 7 to 10 years. This offering from 5Star Life is a highly affordable and convenient way to meet the needs of your family when they need it most. <br>
<br>
You may review the benefits from the brochure link provided. When ready, click the button below to begin a brief enrollment online form that should take just a few minutes to complete.
""".format(company_name=case.company_name)

    vars['default_email_subject'] = "Benefit enrollment - your action needed"

    vars['default_page_text'] = """
You have reached the enrollment page for {company_name}, which is offering you important insurance benefits from 5Star Life. To view details of the offered products, see the link(s) below. This enrollment should take less than 5 minutes to complete. <br>
<br>
Please follow the instructions carefully on the next page, stepping through the simple interview using the next/previous buttons at the lower right of the page. At the end you will be presented with a complete application form to sign electronically. <br>
""".format(company_name=case.company_name)

    # Self-enrollment settings - always ensure the self-enrollment setup object exists for a case, even if not active
    self_enrollment_setup = case_service.get_self_enrollment_setup(case)
    if self_enrollment_setup is None:
        self_enrollment_setup = case_service.create_self_enrollment_setup(case, {
            'self_enrollment_type': SelfEnrollmentSetup.TYPE_CASE_GENERIC,
            'created_by': agent_id,
            'page_title': 'Welcome to your Benefit Enrollment',
            'page_text': vars['default_page_text'],
            'email_sender_name': agent_name,
            'email_sender_email': agent_email,
            'email_subject': vars['default_email_subject'],
            'email_greeting_salutation': "Dear",
            'email_greeting_type': SelfEnrollmentSetup.EMAIL_GREETING_FIRST_NAME,
            'email_message': vars['default_email_message'],
            # If owner set, use owner as agent, otherwise logged-in agent ID
            'enrolling_agent_id': case.agent_id if case.agent_id else agent_id,
        })
        case.self_enrollment_setup = self_enrollment_setup

        # Generate generic self-enrollment link
        self_enrollment_link_service.generate_link(request.url_root, case)
        db.session.commit()

    form = SelfEnrollmentSetupForm(obj=self_enrollment_setup, case=case)

    vars['setup'] = case.self_enrollment_setup
    vars['form'] = form
    vars['case_owner'] = case_service.get_case_owner(case)
    vars['products'] = case.products
    vars['company_name'] = case.company_name
    vars['agent_id'] = agent_id
    vars['agent_name'] = agent_name
    vars['agent_email'] = agent_email
    vars['generic_link'] = self_enrollment_link_service.get_generic_link(request.url_root, case)

    vars["current_user_groups"] = [g.group.name for g in current_user.group_memberships]

    vars["current_user_token"] = api_token_service.get_token_by_sp_href(current_user.href)

    case_riders = rider_service.get_rider_info_for_case(case)

    vars['riders'] = case_riders

    return render_template('agent/case.html', **vars)


@app.route('/enrollment-case/<case_id>/census/<census_record_id>')
@groups_required(['agents', 'home_office', 'admins'], all=False)
def edit_census_record(case_id, census_record_id):
    case = case_service.get_if_allowed(case_id)
    census_record = case_service.get_census_record(case, census_record_id)
    record_form = CensusRecordForm(obj=census_record)
    agent = agent_service.get_logged_in_agent()
    # Get the child entries out
    child_form_fields = []
    for x in range(1, 6+1):
        child_fields = [
            getattr(record_form, 'child{}_first'.format(x)),
            getattr(record_form, 'child{}_last'.format(x)),
            getattr(record_form, 'child{}_birthdate'.format(x)),
        ]
        child_form_fields.append(child_fields)

    is_admin = agent_service.can_manage_all_cases(current_user)

    enrollment_records = enrollment_service.get_enrollment_records_for_census(census_record.case, census_record.id)

    enroll_data = []
    for enrollment_data in enrollment_records:
        enroll_data += [format_enroll_data(enrollment_data, product_num) for product_num in range(1, 6+1)]

    vars = dict(
        case=case,
        census_record=census_record,
        form=record_form,
        child_form_fields=child_form_fields,
        enrollment_status=enrollment_service.get_enrollment_status(census_record),
        enrollment_data=enroll_data,
        is_admin=is_admin,
        case_is_enrolling=case_service.is_enrolling(case),
        header_title='Home Office' if is_admin else '',
        nav_menu=get_nav_menu()
    )
    if agent:
        vars['can_edit_case'] = (agent is case_service.get_case_owner(case))
    else:
        vars['can_edit_case'] = True
    return render_template('agent/census_record.html', **vars)


def format_enroll_data(enrollment_data, product_number):
    if enrollment_data["product_{}_name".format(product_number)]:
        data = dict(
            id=enrollment_data['enrollment_id'],
            product_name=enrollment_data["product_{}_name".format(product_number)],
            time=enrollment_data["signature_time"],
            coverage=[get_coverage_for_product(enrollment_data, product_number, j) for j in ["emp","sp","ch"]],
            status=enrollment_data["application_status"],
            total=reduce(lambda coverage_type, accum: calc_total(enrollment_data, product_number, coverage_type, accum),
                         ["emp","sp","ch"], 0)
        )
    else:
        data = None

    return data


def get_coverage_for_product(enrollment_data, product_number, coverage_type):
    if coverage_type == "emp":
        coverage_label = "Employee"
    elif coverage_type == "sp":
        coverage_label = "Spouse"
    else:
        coverage_label = "Child"
    return dict(
        who=coverage_label,
        annual_premium=enrollment_data["product_{}_{}_annual_premium".format(product_number, coverage_type)],
        coverage=enrollment_data["product_{}_{}_coverage".format(product_number, coverage_type)],
    )


def calc_total(enrollment_data, product_number, x, y):
    premium = enrollment_data["product_{}_{}_annual_premium".format(product_number, y)]
    if not premium:
        return x
    return x + premium


@app.route('/sample-census-upload.csv')
def sample_upload_csv():
    sample_path = os.path.join(app.root_path, 'frontend', 'static', 'misc',
                               'sample_census_upload.csv')
    return send_file(sample_path, as_attachment=True,
                     attachment_filename='sample_census_upload.csv')


@app.route('/manage-case/<int:case_id>/self-enrollment')
@app.route('/enrollment-case/<int:case_id>/self-enrollment')
@groups_required(['agents', 'home_office', 'admins'], all=False)
def edit_self_enroll_setup(case_id=None):
    agent = agent_service.get_logged_in_agent()
    case = case_service.get_if_allowed(case_id)
    self_enrollment_setup = case_service.get_self_enrollment_setup(case)
    form = SelfEnrollmentSetupForm(obj=self_enrollment_setup, case=case)
    vars = {
        'nav_menu': get_nav_menu(),
        'agent': agent,
        'case': case,
        'company_name': case.company_name,
        'form': form,
        'products': case.products,
    }
    if case.self_enrollment_setup is not None:
        vars['setup'] = case.self_enrollment_setup
    return render_template('agent/self_enrollment_setup.html', **vars)


@app.route('/batch-info/<int:case_id>/preview/<batch_id>')
@groups_required(['agents', 'home_office', 'admins'], all=False)
def view_batch_email_preview(case_id, batch_id=None):
        batch = self_enrollment_email_service.get_batch_for_case(case_id, batch_id)
        case = case_service.get_if_allowed(case_id)
        setup = case.self_enrollment_setup
        email_test = batch.email_logs[0]
        return render_template(
            "agent/preview_email.html",
            custom_message=batch.email_body,
            greeting=build_fake_email_greeting(setup),
            enrollment_url="#",
            company_name=case.company_name,
            products = case.products
        )


@app.route('/batch-info/<int:case_id>/logs/<batch_id>')
@groups_required(['agents', 'home_office', 'admins'], all=False)
def view_batch_email_logs(case_id, batch_id=None):
        case = case_service.get_if_allowed(case_id)
        batch = self_enrollment_email_service.get_batch_for_case(case_id, batch_id)
        email_logs = []

        for email in batch.email_logs:
            census_record = case_service.get_census_record(case, email.census_id)
            email.enrollment_status = enrollment_service.get_enrollment_status(census_record)
            email_logs.append(email)
        return render_template(
            "agent/email_logs.html",
            batch_emails=email_logs,
        )


def build_fake_email_greeting(setup):
    salutation = ''
    if setup.email_greeting_salutation:
        salutation = '{} '.format(setup.email_greeting_salutation)
    greeting_end = ''
    if setup.email_greeting_type == SelfEnrollmentSetup.EMAIL_GREETING_FIRST_NAME:
        greeting_end = "Firstname,"
    elif setup.email_greeting_type == SelfEnrollmentSetup.EMAIL_GREETING_FULL_NAME:
        greeting_end = "Firstname Lastname,"
    elif setup.email_greeting_type == SelfEnrollmentSetup.EMAIL_GREETING_LAST_NAME:
        greeting_end = "Lastname,"
    elif setup.email_greeting_type == SelfEnrollmentSetup.EMAIL_GREETING_TITLE_LAST:
        title = 'Mr./Ms.'

        greeting_end = "{} Lastname,".format(title)
    elif setup.email_greeting_type == SelfEnrollmentSetup.EMAIL_GREETING_BLANK:
        greeting_end = ''
    greeting = "{}{}".format(salutation, greeting_end)
    return greeting
