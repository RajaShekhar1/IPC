"""--------------------------------------------------------------
ENROLLMENT pages and handling, DOCUSIGN interaction
"""

import os
import json

import requests

from flask import (abort, jsonify, render_template, request,
                   send_from_directory, session, url_for)
from flask.ext.stormpath import login_required
from flask_stormpath import current_user

from taa import app
from nav import get_nav_menu
from taa.models import db
from taa.old_model.States import get_states

from taa.services.cases import CaseService
from taa.services.agents import AgentService
from taa.services.products import ProductService
from taa.services.products import get_payment_modes, is_payment_mode_changeable
from taa.services.enrollments import (EnrollmentApplicationService,
                                      SelfEnrollmentLinkService)
from taa.services.docusign.docusign_envelope import create_envelope_and_get_signing_url

product_service = ProductService()
case_service = CaseService()
agent_service = AgentService()
enrollment_service = EnrollmentApplicationService()
self_enrollment_link_service = SelfEnrollmentLinkService()

@app.route('/enroll')
@login_required
def enroll_start():
    should_show_next_applicant = bool(request.args.get('next'))
    if session.get('active_case_id') and should_show_next_applicant:
        case = case_service.get_if_allowed(session['active_case_id'])
    else:
        # Clear session variables
        session['active_case_id'] = None
        session['enrolling_census_record_id'] = None
        case = None
    agent = agent_service.get_logged_in_agent()
    agent_products = product_service.get_products_for_agent(agent)
    product_states = product_service.get_product_states(agent_products)
    all_states = product_service.get_all_states()
    return render_template(
        'enrollment/setup-enrollment.html',
        # form=form,
        product_state_mapping=product_states,
        all_states=all_states,
        agent_products=agent_products,
        agent_cases=case_service.get_agent_cases(agent, only_enrolling=True),
        active_case=case,
        should_show_next_applicant=should_show_next_applicant and case,
        nav_menu=get_nav_menu(),
    )


# Wizard
@app.route('/in-person-enrollment', methods=['POST'])
@login_required
def in_person_enrollment():
    record_id = request.form.get('record_id')
    is_self_enroll = session.get('is_self_enrollment', False)
    if record_id is not None:
        # In-person enrollment
        return _in_person_enrollment(record_id=int(record_id),
                                     is_self_enroll=is_self_enroll)
    else:
        # Ad-hoc enrollment
        return _in_person_enrollment(data=request.form,
                                     is_self_enroll=is_self_enroll)


def _in_person_enrollment(record_id=None, data=None, is_self_enroll=False):
    is_in_person = True
    if record_id is not None:
        # Enrolling from a case census record
        record_id = int(record_id)
        record = case_service.get_census_record(None, record_id)
        # Set a flag that we are currently enrolling from this case
        session['active_case_id'] = record.case_id
        session['enrolling_census_record_id'] = record.id
        if is_self_enroll:
            state = record.employee_state or record.case.situs_state
            enroll_city = record.employee_city or record.case.situs_city
            is_in_person = False
        else:
            state = record.case.situs_state
            enroll_city = record.case.situs_city
        company_name = record.case.company_name
        group_number = record.case.group_number
        products = record.case.products
        employee_data = record.get_employee_data()
        spouse_data = record.get_spouse_data()
        children_data = record.get_children_data()
        payment_mode = record.case.payment_mode
        if is_payment_mode_changeable(payment_mode):
            # User can select payment mode
            payment_mode_choices = get_payment_modes(True)
        else:
            # Payment mode is set on case and cannot be changed
            payment_mode_choices = get_payment_modes(single=payment_mode)
    else:
        # Ad-hoc enrollment
        state = data['enrollmentState']
        enroll_city = data['enrollmentCity']
        company_name = data['companyName']
        group_number = data['groupNumber']
        product_id = data['productID']
        product = product_service.get_if_allowed(product_id)
        payment_mode = None
        payment_mode_choices = get_payment_modes(True)
        products = [product] if product else []
        employee_data = dict(
            first=data['eeFName'],
            last=data['eeLName'],
            email=data['email'],
            state=state,
        )
        spouse_data = None
        children_data = []
    # refresh active_case
    session['active_case'] = {
        'company_name': company_name,
        'group_number': group_number,
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
        'group_number': group_number,
        'products': products,
        'employee_data': employee_data,
        'spouse_data': spouse_data,
        'children_data': children_data,
        'is_in_person': is_in_person,
        'health_questions': soh_questions,
        'payment_mode_choices': payment_mode_choices,
        'payment_mode': payment_mode,
    }
    # Commit any changes made (none right now)
    db.session.commit()
    return render_template(
        'enrollment/main-wizard.html',
        wizard_data=wizard_data,
        states=get_states(),
        nav_menu=get_nav_menu(),
    )


@app.route('/self-enroll/<string:company_name>/<string:uuid>')
def self_enrollment(company_name, uuid):
    setup, census_record = self_enrollment_link_service.get_self_enrollment_data_for(uuid,
                                                                                     current_user.is_anonymous())
    vars = {'is_valid': False}
    if setup is not None:
        session['is_self_enroll'] = True
        vars.update({
            'is_valid': True,
            'page_title': setup.page_title,
            'page_text': setup.page_text,
            'page_disclaimer': setup.page_disclaimer,
            'census_record': census_record,
            'self_enrollment_setup': setup,
        })
    return render_template('enrollment/landing_page.html', **vars)


@app.route('/self-enrollment', methods=['POST'])
def self_enrollment2():
    id = request.form.get('record_id')
    if session.get('is_self_enroll') is not None:
        return _in_person_enrollment(record_id=int(id) if id else None,
                                     is_self_enroll=True)
    else:
        abort(404)


@app.route('/submit-wizard-data', methods=['POST'])
def submit_wizard_data():
    data = request.json
    wizard_results = data['wizard_results']
    print("[ENROLLMENT SUBMITTED]: {}".format(wizard_results))
    # Save enrollment information and updated census data prior to
    # DocuSign hand-off
    if session.get('enrolling_census_record_id'):
        census_record = case_service.get_census_record(
            None, session['enrolling_census_record_id'])
    else:
        census_record = None
    # Get the agent for this session - for now, it is the logged-in user, but
    # will need to be determined differently for self-enroll situations
    agent = agent_service.get_logged_in_agent()
    # Create and save the enrollment data
    enrollment_application = enrollment_service.save_enrollment_data(
        wizard_results, census_record, agent)
    if not wizard_results.get('did_decline'):
        # Hand off wizard_results to docusign
        is_error, error_message, redirect = create_envelope_and_get_signing_url(wizard_results, census_record);
        # Return the redirect url or error
        resp = {'error': is_error,
                'error_message': error_message,
                'redirect': redirect}
    else:
        # Declined
        resp = {
            'error': False,
            'error_message': '',
            'redirect': url_for('ds_landing_page',
                                event='decline',
                                name=wizard_results['employee']['first'],
                                type='inperson'
                                )
        }
    data = jsonify(**resp)
    # Need to manually commit all changes since this doesn't go through the API
    # right now
    db.session.commit()
    return data


@app.route('/application_completed', methods=['GET'])
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


# TODO: just use this route in the future rather than adding more individual
# routes for files
@app.route('/pdfs/<file_name>')
def serve_pdf(file_name):
    return send_from_directory(
        os.path.join(app.root_path, 'frontend', 'static', 'pdfs'), file_name)


@app.route('/FPPTI_disclosure.pdf')
def FPPTI_disclosure_generic():
    return send_from_directory(
        os.path.join(app.root_path, 'frontend', 'static'),
        'pdfs/FPPTI_disclosure_generic.pdf')


@app.route('/FPPTI_disclosure_TX.pdf')
def FPPTI_disclosure_TX():
    return send_from_directory(
        os.path.join(app.root_path, 'frontend', 'static'),
        'pdfs/FPPTI_disclosure_TX.pdf')


@app.route('/FPPTI_disclosure_KS.pdf')
def FPPTI_disclosure_KS():
    return send_from_directory(
        os.path.join(app.root_path, 'frontend', 'static'),
        'pdfs/FPPTI_disclosure_KS.pdf')


@app.route('/FPPTI_disclosure_OR.pdf')
def FPPTI_disclosure_OR():
    return send_from_directory(
        os.path.join(app.root_path, 'frontend', 'static'),
        'pdfs/FPPTI_disclosure_OR.pdf')


@app.route('/FPPTI_disclosure_VA.pdf')
def FPPTI_disclosure_VA():
    return send_from_directory(
        os.path.join(app.root_path, 'frontend', 'static'),
        'pdfs/FPPTI_disclosure_VA.pdf')


@app.route('/FPPCI_disclosure.pdf')
def FPPCI_disclosure_generic():
    return send_from_directory(
        os.path.join(app.root_path, 'frontend', 'static'),
        'pdfs/FPPCI_disclosure_generic.pdf')


@app.route('/FPPCI_disclosure_KS.pdf')
def FPPCI_disclosure_KS():
    return send_from_directory(
        os.path.join(app.root_path, 'frontend', 'static'),
        'pdfs/FPPCI_disclosure_KS.pdf')


@app.route('/FPPCI_disclosure_OR.pdf')
def FPPCI_disclosure_OR():
    return send_from_directory(
        os.path.join(app.root_path, 'frontend', 'static'),
        'pdfs/FPPCI_disclosure_OR.pdf')


@app.route('/FPPCI_disclosure_VA.pdf')
def FPPCI_disclosure_VA():
    return send_from_directory(
        os.path.join(app.root_path, 'frontend', 'static'),
        'pdfs/FPPCI_disclosure_VA.pdf')
