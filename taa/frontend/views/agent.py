"""
AGENT pages and DOCUSIGN inbox
"""
import os

from flask import render_template, redirect, url_for, flash, send_file
from flask_stormpath import login_required, groups_required, current_user

from taa import app
from nav import get_nav_menu
from taa.api.cases import census_records
from taa.services.docusign.docu_console import console_url
from taa.services.cases import CaseService
from taa.services.cases.forms import (CensusRecordForm,
                                      NewCaseEnrollmentPeriodForm,
                                      SelfEnrollmentSetupForm, UpdateCaseForm)
from taa.services.agents import AgentService
from taa.services.products import ProductService, get_all_states
from taa.services.products import get_payment_modes
from taa.services.docusign.DocuSign_config import sessionUserApprovedForDocusign

case_service = CaseService()
agent_service = AgentService()
product_service = ProductService()


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
        user_cases = case_service.get_agent_cases(agent)
        header_title = ''
    else:
        # Admin or home office user
        user_cases = case_service.all()
        header_title = 'Home Office'

    vars = {
        'agent_cases': user_cases,
        'all_states': get_all_states(),
        'nav_menu': get_nav_menu(),
        'header_title': header_title,
    }
    return render_template('agent/manage_cases.html', **vars)


@app.route('/manage-case/<case_id>')
@groups_required(['agents', 'home_office', 'admins'], all=False)
def manage_case(case_id):
    case = case_service.get_if_allowed(case_id)
    vars = {'case': case}

    agent = agent_service.get_logged_in_agent()
    if agent:
        products = product_service.get_products_for_agent(agent)
    else:
        products = product_service.get_all_enrollable_products()
        vars['is_admin'] = True
        vars['active_agents'] = agent_service.get_active_agents()
        vars['header_title'] = 'Home Office'

    vars['product_choices'] = products
    vars['all_states'] = get_all_states()
    vars['payment_modes'] = get_payment_modes()
    vars['product_state_mapping'] = product_service.get_product_states(products)

    case_setup_form = UpdateCaseForm(obj=case)
    vars['case_setup_form'] = case_setup_form
    if not case.products:
        vars['case_product'] = None
    else:
        vars['case_product'] = case.products[0]
    if not case.situs_state:
        vars['case_state'] = None
    else:
        vars['case_state'] = case.situs_state

    enrollment_periods = NewCaseEnrollmentPeriodForm(
        **case_service.get_case_enrollment_period_data(case))
    vars['enrollment_period_form'] = enrollment_periods

    vars['census_records'] = [
        case_service.census_records.get_record_dict(record)
        for record in census_records(case_id)
        ]
    vars['nav_menu'] = get_nav_menu()

    # Has active enrollments?
    vars['case_has_enrollments'] = case_service.does_case_have_enrollments(case)

    return render_template('agent/case.html', **vars)


@app.route('/manage-case/<case_id>/census/<census_record_id>')
@groups_required(['agents', 'home_office', 'admins'], all=False)
def edit_census_record(case_id, census_record_id):
    case = case_service.get_if_allowed(case_id)
    census_record = case_service.get_census_record(case, census_record_id)
    record_form = CensusRecordForm(obj=census_record)

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

    vars = dict(
        case=case,
        census_record=census_record,
        form=record_form,
        child_form_fields=child_form_fields,
        is_admin=is_admin,
        header_title='Home Office' if is_admin else '',
        nav_menu=get_nav_menu()
    )
    return render_template('agent/census_record.html', **vars)


@app.route('/sample-census-upload.csv')
def sample_upload_csv():
    sample_path = os.path.join(app.root_path, 'frontend', 'static', 'misc',
                               'sample_census_upload.csv')
    return send_file(sample_path, as_attachment=True,
                     attachment_filename='sample_census_upload.csv')


@app.route('/manage-case/<int:case_id>/self-enrollment')
@groups_required(['agents', 'home_office', 'admins'], all=False)
def edit_self_enroll_setup(case_id=None):
    agent = agent_service.get_logged_in_agent()
    if case_id is None:
        # Ad-hoc self-enrollment setup
        case = None
    else:
        # Case-based self-enrollment setup
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
