"""
AGENT pages and DOCUSIGN inbox
"""
import os
import csv

from flask import render_template, redirect, url_for, flash, send_file
from flask_stormpath import login_required, groups_required

from taa import app
from taa.api.cases import census_records, post_census_records
from taa.services.docusign.docu_console import console_url
from taa.services.cases import CaseService
from taa.services.cases.forms import (
    CensusRecordForm, 
    NewCaseEnrollmentPeriodForm,
    UpdateCaseForm
)
from taa.services.agents import AgentService
from taa.model.DocuSign_config import sessionUserApprovedForDocusign
from taa.model.Enrollment import get_product_states, get_product_choices, get_all_states

case_service = CaseService()
agent_service = AgentService()

@app.route("/inbox", methods =['GET'])
@login_required
def inbox():
    if sessionUserApprovedForDocusign():
        return render_template('agent/agent-inbox.html',
                               inboxURL = console_url())
    else:
        flash("You are not yet authorized for signing applications.  Please see your Regional Director for assistance.")
        return redirect(url_for("home"))

@app.route("/manage-cases")
@groups_required(["agents", "admins"], all=False)
@login_required
def manage_cases():
    agent = agent_service.get_logged_in_agent()
    agent_cases = case_service.get_agent_cases(agent)
    vars = {'agent_cases':agent_cases, 
            'all_states': get_all_states(),
            'product_choices': get_product_choices(),
            'product_states': get_product_states()} 
    return render_template('agent/manage_cases.html', **vars)


@app.route("/manage-case/<case_id>")
@groups_required(["agents", "admins"], all=False)
def manage_case(case_id):
    
    case = case_service.get_if_allowed(case_id)
    
    vars = {'case':case}
    
    vars['product_choices'] = get_product_choices()
    vars['product_states'] = get_product_states()
    vars['all_states'] = get_all_states()

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

    
    enrollment_periods = NewCaseEnrollmentPeriodForm(**case_service.get_case_enrollment_period_data(case))
    vars['enrollment_period_form'] = enrollment_periods 
    
    vars['census_records'] = [
        case_service.census_records.get_record_dict(record) for record in census_records(case_id)
    ]
    
    return render_template('agent/case.html', **vars)



@app.route("/manage_case/<case_id>/census_upload", methods=['POST'])
@groups_required(["agents", "admins"], all=False)
def upload_census_record(case_id):
    case = case_service.get_if_allowed(case_id)
    
    result = post_census_records(case_id)
    if result['errors']:
        return render_template('agent/upload_errors.html', errors=result['errors'], records=result['records'],
                               case=case)
    
    return redirect(url_for('manage_case', case_id=case_id))
    
    
@app.route("/manage_case/<case_id>/census/<census_record_id>")
@groups_required(["agents", "admins"], all=False)
def edit_census_record(case_id, census_record_id):
    case = case_service.get_if_allowed(case_id)
    census_record = case_service.get_census_record(case, census_record_id)
    vars = dict(
        case=case, 
        census_record=census_record,
        form=CensusRecordForm(obj=census_record)
    )
    return render_template('agent/census_record.html', **vars)


@app.route("/sample-census-upload.csv")
def sample_upload_csv():
    sample_path = os.path.join(app.root_path, 'frontend', 'static', 'misc', 'sample_census_upload.csv')
    return send_file(sample_path, as_attachment=True, attachment_filename="sample_census_upload.csv")