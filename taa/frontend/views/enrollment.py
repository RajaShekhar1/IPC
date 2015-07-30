"""--------------------------------------------------------------
ENROLLMENT pages and handling, DOCUSIGN interaction
"""

import os
import json

from flask import (abort, jsonify, render_template, request,
                   send_from_directory, session, url_for, redirect, Response)
from flask.ext.stormpath import login_required
from flask_stormpath import current_user

from nav import get_nav_menu
from taa import app
from taa.models import db
from taa.old_model.States import get_states
from taa.services.products.states import get_all_states
from taa.services.products import get_payment_modes, is_payment_mode_changeable
from taa.services.docusign.service import create_envelope_and_get_signing_url
from taa.services import LookupService

product_service = LookupService('ProductService')
product_form_service = LookupService('ProductFormService')
case_service = LookupService('CaseService')
agent_service = LookupService('AgentService')
enrollment_service = LookupService('EnrollmentApplicationService')
self_enrollment_service = LookupService('SelfEnrollmentService')
self_enrollment_link_service = LookupService('SelfEnrollmentLinkService')
enrollment_import_service = LookupService('EnrollmentImportService')

@app.route('/enroll')
@login_required
def enroll_start():
    """This is a placeholder route that just redirects to the manage case page"""
    should_show_next_applicant = bool(request.args.get('next'))
    if session.get('active_case_id') and should_show_next_applicant:
        # We no longer use the separate setup enrollment page, forward agent to manage_case page
        case = case_service.get_if_allowed(session['active_case_id'])
        return redirect(location=url_for('manage_case', case_id=case.id)+"#enrollment")

    abort(404)

# Wizard
@app.route('/in-person-enrollment', methods=['POST'])
@login_required
def in_person_enrollment():
    """
    Agent sitting down with employee for the enrollment wizard. Always done via a case census record.
    """
    record_id = request.form.get('record_id')
    enrollment_city_override = request.form.get('enrollment_city')
    enrollment_state_override = request.form.get('enrollment_state')

    record = case_service.census_records.get(record_id)

    return _setup_enrollment_session(record.case, record_id=int(record_id),
                                     is_self_enroll=False, data={
            'enrollmentCity': enrollment_city_override,
            'enrollmentState': enrollment_state_override,
        })



def _setup_enrollment_session(case, record_id=None, data=None, is_self_enroll=False):

    # Defaults for session enrollment variables.
    session['active_case_id'] = case.id
    session['enrolling_census_record_id'] = None

    payment_mode = case.payment_mode
    if is_payment_mode_changeable(payment_mode):
        # User can select payment mode
        payment_mode_choices = get_payment_modes(True)
    else:
        # Payment mode is set on case and cannot be changed
        payment_mode_choices = get_payment_modes(single=payment_mode)

    if record_id is not None:
        # Enrolling from a case census record
        record_id = int(record_id)
        record = case_service.get_census_record(None, record_id)

        # Set in the session that we are currently enrolling from this case and record
        session['enrolling_census_record_id'] = record.id

        override_state = data.get('enrollmentState') if data and data.get('enrollmentState') else None
        override_city = data.get('enrollmentCity') if data and data.get('enrollmentCity') else None

        if is_self_enroll:
            # Data is user-provided enrollment location, and trumps the usual defaults
            state = override_state or record.employee_state or record.case.situs_state
            city = override_city or record.employee_city or record.case.situs_city
        else:
            # Data is agent-provided override of case defaults. Ignore employee location since this is in-person,
            #   presumably at the worksite.
            state = override_state or record.case.situs_state
            city = override_city or record.case.situs_city

        company_name = record.case.company_name
        group_number = record.case.group_number
        products = record.case.products
        employee_data = record.get_employee_data()
        spouse_data = record.get_spouse_data()
        children_data = record.get_children_data()

    else:
        # Generic case-link enrollment
        state = data['enrollmentState']
        city = data['enrollmentCity']
        company_name = data['companyName']
        group_number = data['groupNumber']
        product_id = data['productID']
        product = product_service.get(product_id)
        products = [product] if product else []
        employee_data = dict(
            first=data['eeFName'],
            last=data['eeLName'],
            email=data['email'],
            state=state,
        )
        spouse_data = None
        children_data = []

    # Validate that we can enroll in the product for this state - do we have a form?
    if not any(product_form_service.form_for_product_code_and_state(p.get_base_product_code(), state) for p in products):
        # Change the state back to the case state to allow them to continue?
        state = case.situs_state

    # Get SOH Questions and other form or product specific questions
    from taa.services.products import StatementOfHealthQuestionService
    soh_questions = {}
    for product in products:
        soh_questions[product.id] = StatementOfHealthQuestionService().get_health_questions(product, state)

    spouse_questions = {}
    for product in products:
        spouse_questions[product.id] = StatementOfHealthQuestionService().get_spouse_questions(product, state)

    wizard_data = {
        'state': state if state != 'XX' else None,
        'enroll_city': city,
        'company_name': company_name,
        'group_number': group_number,
        'products': products,
        'employee_data': employee_data,
        'spouse_data': spouse_data,
        'children_data': children_data,
        'is_in_person': not is_self_enroll,
        'health_questions': soh_questions,
        'spouse_questions': spouse_questions,
        'payment_mode_choices': payment_mode_choices,
        'payment_mode': payment_mode,
        'case_id': case.id,
    }

    # Commit any changes made (none right now)
    db.session.commit()

    return render_template(
        'enrollment/main-wizard.html',
        wizard_data=wizard_data,
        states=get_states(),
        nav_menu=get_nav_menu(),
    )

# Self Enrollment Landing Page
@app.route('/self-enroll/<string:company_name>/<string:uuid>')
def self_enrollment(company_name, uuid):
    setup, census_record = self_enrollment_link_service.get_self_enrollment_data_for(uuid,
                                                                                     current_user.is_anonymous())
    case = self_enrollment_link_service.get_case_for_link(uuid)

    vars = {'is_valid': False, 'allowed_states': []}
    if setup is not None:
        session['is_self_enroll'] = True

        # Store these in session rather than as a form submission for security purposes
        session['self_enrollment_setup_id'] = setup.id
        session['census_record_id'] = census_record.id if census_record else None

        # Find out what states are allowed
        allowed_statecodes = set()
        product_states = product_service.get_product_states(setup.case.products)
        for product_id, states in product_states.items():
            for state in states:
                allowed_statecodes.add(state)

        # Defaults for enrollment city / state
        if census_record is not None:
            selected_state = census_record.employee_state or census_record.case.situs_state
            selected_city = census_record.employee_city or census_record.case.situs_city
            vars.update(
                {'is_enrolled':
                 enrollment_service.get_enrollment_status(
                     census_record) == 'enrolled'})
        else:
            selected_state = setup.case.situs_state
            selected_city = setup.case.situs_city

        vars.update({
            'is_valid': True,
            'page_title': setup.page_title,
            'company_name': setup.case.company_name,
            'products': setup.case.products,
            'page_text': setup.page_text,
            'page_disclaimer': setup.page_disclaimer,
            'all_states': [s['statecode'] for s in get_all_states()],
            'allowed_states': list(allowed_statecodes),
            'selected_state': selected_state,
            'selected_city': selected_city,
        })
    elif case:
        vars.update({
            'page_title': 'Enrollment service for {} not available'.format(case.company_name),
            'error_message': '''We're sorry for the inconvenience, but {} is not currently accepting benefit enrollments.<br><br>
            Please contact your enrollment or benefit representative if you have any questions.'''.format(case.company_name)

        })
    else:
        vars.update({
            'page_title': 'Enrollment service not available',
            'error_message': '''We're sorry for the inconvenience, but the link you have followed does not permit enrollment at this time.<br><br>
            Please contact your enrollment or benefit representative if you have any questions.'''
        })

    return render_template('enrollment/landing_page.html', **vars)

# Begin application from self-enrollment landing page.
@app.route('/self-enrollment', methods=['POST'])
def self_enrollment2():
    """
    This is the submission handler for the self-enrollment landing page when
        a user is starting an enrollment using the wizard.
    """

    if session.get('is_self_enroll') is None:
        abort(401)

    census_record_id = session.get('census_record_id', None)
    enrollment_setup = self_enrollment_service.get(session['self_enrollment_setup_id'])

    data = {}
    if not census_record_id:
        # Generic link
        case = enrollment_setup.case
        data['enrollmentState'] = enrollment_setup.case.situs_state
        data['enrollmentCity'] = enrollment_setup.case.situs_city
        data['companyName'] = enrollment_setup.case.company_name
        data['groupNumber'] = enrollment_setup.case.group_number
        data['productID'] = enrollment_setup.case.products[0].id
        data['eeFName'] = ""
        data['eeLName'] = ""
        data['email'] = ""

    if 'enrollmentCity' in request.form:
        data['enrollmentCity'] = request.form['enrollmentCity']
    if 'enrollmentState' in request.form:
        data['enrollmentState'] = request.form['enrollmentState']

    return _setup_enrollment_session(enrollment_setup.case, record_id=census_record_id, data=data, is_self_enroll=True)

@app.route('/submit-wizard-data', methods=['POST'])
def submit_wizard_data():
    data = request.json
    case_id = session['active_case_id']
    case = case_service.get(case_id)

    wizard_results = data['wizard_results']
    print("[ENROLLMENT SUBMITTED]")
    # for troubledshooting..   print("[ENROLLMENT SUBMITTED]: (case {}) {}".format(case_id, wizard_results))
    # Save enrollment information and updated census data prior to
    # DocuSign hand-off
    if session.get('enrolling_census_record_id'):
        census_record = case_service.get_census_record(
            case, session['enrolling_census_record_id'])
    else:
        census_record = None

    # Get the agent for this session
    # For self-enroll situations, the owner agent is used
    # TODO: Use agent who sent emails for targeted links
    agent = agent_service.get_logged_in_agent()
    if (agent is None and session.get('is_self_enroll') is not None):
        agent = case.owner_agent

    # Create and save the enrollment data. Creates a census record if this is a generic link, and in
    #   either case updates the census record with the latest enrollment data.
    enrollment_application = enrollment_service.save_enrollment_data(
        wizard_results, case, census_record, agent)

    if not wizard_results.get('did_decline'):
        # Hand off wizard_results to docusign
        is_error, error_message, redirect = create_envelope_and_get_signing_url(wizard_results, census_record, case)
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
                                type='inperson' if wizard_results["agent_data"]["is_in_person"] else 'email',
                                )
        }
    data = jsonify(**resp)
    # Need to manually commit all changes since this doesn't go through the API
    # right now
    db.session.commit()
    return data


@app.route('/submit-enrollment-records', methods=["POST"])
def submit_enrollment_records():
    file = request.files["csv-file"]
    if not file:
        return "No File Submitted!"
    lineData = [line.strip().split(",") for line in file]
    data = []
    headers = lineData.pop(0)
    for line in lineData:
        if line is not "":
            curLine = {}
            for i in range(0, len(line)):
                curLine[headers[i].strip(' \t\r\n')] = line[i].strip(' \t\r\n')
            data.append(curLine)
    response = enrollment_import_service.submit_file_records(data)
    if response.is_success():
        return Response(json.dumps(data), status=200, mimetype='application/json')
    else:
        errors = [{"type": e.get_type(), "fields": e.get_fields(), "message": e.get_message()} for e in response.get_errors()]
        return Response(json.dumps(errors), status=400, mimetype='application/json')


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
