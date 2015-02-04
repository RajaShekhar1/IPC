"""--------------------------------------------------------------
ENROLLMENT pages and handling, DOCUSIGN interaction
"""

import os
import json

from flask import request, render_template, jsonify, session, send_from_directory, url_for
from flask.ext.stormpath import login_required

from taa import app
from nav import get_nav_menu
from taa.models import db
from taa.old_model.States import get_states
from taa.old_model.Enrollment import (
    get_enrollment_setup_form_for_product,
    get_product_states,
)
from taa.services.cases import CaseService
from taa.services.agents import AgentService
from taa.services.products import ProductService
from taa.services.enrollments import EnrollmentApplicationService

product_service = ProductService()    
case_service = CaseService()
agent_service = AgentService()
enrollment_service = EnrollmentApplicationService()

@app.route("/enroll")
@login_required
def enroll_start():
    
    should_show_next_applicant = bool(request.args.get('next'))
    
    if session.get('active_case_id') and should_show_next_applicant:
        case = case_service.get_if_allowed(session['active_case_id'])
        
        product_code = case.products[0].code if case.products else ""
        
        form = get_enrollment_setup_form_for_product(product_code)()
        form.companyName.data = case.company_name
        form.enrollmentCity.data = case.situs_city
        form.enrollmentState.data = case.situs_state
        form.productID.data = product_code
    else:
        # Clear session variables
        session['active_case_id'] = None
        session['enrolling_census_record_id'] = None
        
        case = None
        form = get_enrollment_setup_form_for_product(None)()
    
    agent = agent_service.get_logged_in_agent()
    agent_products = product_service.get_products_for_agent(agent)
    return render_template('enrollment/setup-enrollment.html', 
                           form=form, 
                           product_states=get_product_states(),
                           agent_products=agent_products,
                           agent_cases=case_service.get_agent_cases(agent, only_enrolling=True),
                           active_case=case,
                           should_show_next_applicant=should_show_next_applicant and case,
                           nav_menu=get_nav_menu(),
    )

@app.route("/select-case", methods=['GET', 'POST'])
@login_required
def select_case():
    case_id = request.form['case_id']
    case = case_service.get_if_allowed(case_id)
    
    # Save so we can come back here afterwards
    session['active_case_id'] = case_id
    census_records = [r.to_json() for r in case_service.get_census_records(case)]
    return jsonify(dict(
        table=render_template('enrollment/setup-enrollment-census-select.html', 
                              case=case, 
                              census_records=census_records,
                              nav_menu=get_nav_menu(),
                              )
    ))

# Wizard
@app.route("/in-person-enrollment", methods=['POST'])
@login_required
def in_person_enrollment():
    
    if request.form.get('record_id'):
        # Enrolling from a case census record
        record_id = int(request.form['record_id'])
        record = case_service.get_census_record(None, record_id)
        
        # Set a flag that we are currently enrolling from this case
        session['active_case_id'] = record.case_id
        session['enrolling_census_record_id'] = record.id
        
        state = record.case.situs_state
        enroll_city = record.case.situs_city
        company_name = record.case.company_name
        #product_code = record.case.products[0].code if record.case.products else None
        products = record.case.products
        employee_data = record.get_employee_data()
        spouse_data = record.get_spouse_data()
        children_data = record.get_children_data()
    else:   
        # TODO: Create one-off census record
        state = request.form['enrollmentState']
        enroll_city = request.form['enrollmentCity']
        company_name = request.form['companyName']
        product_id = request.form['productID']
        product = product_service.get_if_allowed(product_id)
        
        products = [product] if product else []
        employee_data = dict(
            first=request.form['eeFName'],
            last=request.form['eeLName'],
            email=request.form['email'],
            state=state,
        )
        spouse_data = None
        children_data = []
    
    # refresh active_case
    session['active_case'] = {
        'company_name': company_name,
        'situs_state': state,
        'situs_city': enroll_city,
    }
    
    
    # Get SOH Questions
    from taa.services.products import StatementOfHealthQuestionService
    soh_questions = {}
    for product in products:
        soh_questions[product.id] = StatementOfHealthQuestionService().get_health_questions(product, state)
    
    wizard_data = {
        'state': state if state != 'XX' else None,
        'enroll_city': enroll_city,
        'company_name': company_name,
        'products': products,
        'employee_data':employee_data,
        'spouse_data':spouse_data,
        'children_data':children_data,
        'is_in_person':True,
        'health_questions': soh_questions,
    }
    
    # Commit any changes made (none right now)
    db.session.commit()
    
    return render_template(
        'enrollment/main-wizard.html', 
        wizard_data=wizard_data,
        states=get_states(),
        nav_menu=get_nav_menu(),
    )


@app.route("/submit-wizard-data", methods=['POST'])
def submit_wizard_data():
    
    data = request.json
    wizard_results = data['wizard_results']
    
    # Save enrollment information and updated census data prior to DocuSign hand-off 
    if session.get('enrolling_census_record_id'):
        census_record = case_service.get_census_record(None, session['enrolling_census_record_id'])
        print("here")
    else:
        census_record = None
        
    # TODO: Handle ad-hoc enrollments
    if census_record:
        enrollment_application = enrollment_service.save_enrollment_data(wizard_results, census_record)
    else:
        enrollment_application = None
        
    if not wizard_results.get('did_decline'):
        pass
        # Hand off wizard_results to docusign
        #
        #is_error, error_message, redirect = create_envelope_and_get_signing_url(wizard_results);
        #
        # Return the redirect url or error
        #resp = {'error': is_error, 'error_message': error_message, "redirect": redirect}

        resp = {
            'error': False,
            'error_message': '',
            'redirect': url_for("ds_landing_page",
                                event="signing_complete",
                                name=wizard_results['employee']['first'],
                                type='inperson'
            )
        }
    else:
        # Declined
        resp = {
            'error': False,
            'error_message': '',
            'redirect': url_for("ds_landing_page",
                                event="decline",
                                name=wizard_results['employee']['first'],
                                type='inperson'
            )
        }
    
    
    
    data = jsonify(**resp)
    
    # need to manually commit all changes since this doesn't go through the API right now
    db.session.commit()
    
    return data
    

@app.route("/application_completed", methods=['GET'])
def ds_landing_page():
    """
    Handles simple responses to completing the enrollment page
    """

    session_type = request.args['type']
    name = request.args['name']
    ds_event = request.args['event']

    return render_template('enrollment/completed-session.html',
                           session_type=session_type,
                           name=name,
                           ds_event=ds_event,
                           nav_menu=get_nav_menu(),
                           )


 


@app.route("/FPPTI_disclosure.pdf")
def FPPTI_disclosure_generic():
    return send_from_directory(os.path.join(app.root_path, 'frontend', 'static'), 'pdfs/FPPTI_disclosure_generic.pdf')
@app.route("/FPPTI_disclosure_TX.pdf")
def FPPTI_disclosure_TX():
    return send_from_directory(os.path.join(app.root_path, 'frontend', 'static'), 'pdfs/FPPTI_disclosure_TX.pdf')
@app.route("/FPPTI_disclosure_KS.pdf")
def FPPTI_disclosure_KS():
    return send_from_directory(os.path.join(app.root_path, 'frontend', 'static'), 'pdfs/FPPTI_disclosure_KS.pdf')
@app.route("/FPPTI_disclosure_OR.pdf")
def FPPTI_disclosure_OR():
    return send_from_directory(os.path.join(app.root_path, 'frontend', 'static'), 'pdfs/FPPTI_disclosure_OR.pdf')
@app.route("/FPPTI_disclosure_VA.pdf")
def FPPTI_disclosure_VA():
    return send_from_directory(os.path.join(app.root_path, 'frontend', 'static'), 'pdfs/FPPTI_disclosure_VA.pdf')
@app.route("/FPPCI_disclosure.pdf")
def FPPCI_disclosure_generic():
    return send_from_directory(os.path.join(app.root_path, 'frontend', 'static'), 'pdfs/FPPCI_disclosure_generic.pdf')
@app.route("/FPPCI_disclosure_KS.pdf")
def FPPCI_disclosure_KS():
    return send_from_directory(os.path.join(app.root_path, 'frontend', 'static'), 'pdfs/FPPCI_disclosure_KS.pdf')
@app.route("/FPPCI_disclosure_OR.pdf")
def FPPCI_disclosure_OR():
    return send_from_directory(os.path.join(app.root_path, 'frontend', 'static'), 'pdfs/FPPCI_disclosure_OR.pdf')
@app.route("/FPPCI_disclosure_VA.pdf")
def FPPCI_disclosure_VA():
    return send_from_directory(os.path.join(app.root_path, 'frontend', 'static'), 'pdfs/FPPCI_disclosure_VA.pdf')
