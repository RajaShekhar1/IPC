"""--------------------------------------------------------------
ENROLLMENT pages and handling, DOCUSIGN interaction
"""

import os
import json

from flask import (abort, jsonify, render_template, request,
                   send_from_directory, session, url_for, redirect, Response)
from flask.ext.stormpath import login_required
from flask_stormpath import current_user
from taa.services.docusign.docusign_envelope import EnrollmentDataWrap, build_callcenter_callback_url, \
    build_callback_url

from taa.services.enrollments import EnrollmentApplication

from nav import get_nav_menu
from taa import app
from taa.models import db
from taa.old_model.States import get_states
from taa.services.products.states import get_all_states
from taa.services.products import get_payment_modes, is_payment_mode_changeable, get_full_payment_modes
from taa.services.docusign.service import DocusignEnvelope
from taa.services.products.riders import RiderService
from taa.services import LookupService

product_service = LookupService('ProductService')
product_form_service = LookupService('ProductFormService')
case_service = LookupService('CaseService')
rider_service = RiderService()
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
        return redirect(location=url_for('manage_case', case_id=case.id) + "#enrollment")

    abort(404)


#
# # Wizard
# @app.route('/test-wizard')
# def test_wizard():
#     #case_id = request.params.get('case_id')
#     #case = case_service.get(case_id)
#     fppti = product_service.query().filter_by(code='FPPTI').first()
#     fppci = product_service.query().filter_by(code='FPPCI').first()
#     products = [fppti, fppci]
#     state = 'MI'
#
#     # Get SOH Questions and other form or product specific questions
#     from taa.services.products import StatementOfHealthQuestionService
#     soh_questions = {}
#     for product in products:
#         soh_questions[product.id] = StatementOfHealthQuestionService().get_health_questions(product, state)
#
#     spouse_questions = {}
#     for product in products:
#         spouse_questions[product.id] = StatementOfHealthQuestionService().get_spouse_questions(product, state)
#
#     payment_mode = 26
#     if is_payment_mode_changeable(payment_mode):
#         # User can select payment mode
#         payment_mode_choices = get_payment_modes(True)
#     else:
#         # Payment mode is set on case and cannot be changed
#         payment_mode_choices = get_payment_modes(single=payment_mode)
#
#     case_riders = [] #rider_service.get_selected_case_rider_info(case)
#     enrollment_riders = [] # rider_service.get_enrollment_rider_info()
#
#     return render_template(
#         'enrollment/main-wizard.html',
#         wizard_data={
#             'products':products,
#             'state':'MI',
#             'enroll_city':'Lansing',
#             'children_data':[],
#             'payment_mode': payment_mode,
#             'payment_mode_choices':payment_mode_choices,
#             'health_questions':soh_questions,
#             'spouse_questions':spouse_questions,
#             'is_in_person': True,
#             'selected_riders':[],
#         },
#         states=get_states(),
#         nav_menu=get_nav_menu(),
#         case_riders=case_riders,
#         enrollment_riders=enrollment_riders,
#     )


@app.route('/in-person-enrollment', methods=['POST'])
@login_required
def in_person_enrollment():
    """
    Agent sitting down with employee for the enrollment wizard.
    """

    record_id = request.form.get('record_id')
    if record_id:
        # Get the case from the record if provided
        record = case_service.get_census_record(None, record_id)
        if not record:
            abort(400, "Invalid census record ID")
        case = record.case
    else:
        # Otherwise the case must be provided explicitly
        case_id = request.form['case_id']
        case = case_service.get_if_allowed(case_id)

    ssn = request.form.get('ssn')
    enrollment_city_override = request.form.get('enrollment_city')
    enrollment_state_override = request.form.get('enrollment_state')

    return _setup_enrollment_session(
        case,
        record_id=record_id,
        is_self_enroll=False,
        data={
            'enrollmentCity': enrollment_city_override,
            'enrollmentState': enrollment_state_override,
            'ssn': ssn,
        })


def _setup_enrollment_session(case, record_id=None, data=None, is_self_enroll=False):
    # Defaults for session enrollment variables.
    session['active_case_id'] = case.id
    session['enrolling_census_record_id'] = None

    payment_mode = case.payment_mode
    if is_payment_mode_changeable(payment_mode):
        # User can select payment mode
        payment_mode_choices = get_full_payment_modes()
    else:
        # Payment mode is set on case and cannot be changed
        payment_mode_choices = filter(lambda pm: pm['frequency'] == payment_mode, get_full_payment_modes())

    if record_id is not None:
        # Enrolling from a case census record
        record_id = int(record_id)
        record = case_service.get_census_record(None, record_id)

        # Set in the session that we are currently enrolling from this case and record
        session['enrolling_census_record_id'] = record.id

        override_state = data.get('enrollmentState') if data.get('enrollmentState') else None
        override_city = data.get('enrollmentCity') if data.get('enrollmentCity') else None

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
        # Pull in the generic data from the case rather than from a record.
        override_state = data.get('enrollmentState') if data.get('enrollmentState') else None
        override_city = data.get('enrollmentCity') if data.get('enrollmentCity') else None

        data.update(get_case_enrollment_data(case))

        state = override_state if override_state else data['enrollmentState']
        city = override_city if override_city else data['enrollmentCity']

        if 'occupation_class' not in data:
            data['occupation_class'] = None

        company_name = data['companyName']
        group_number = data['groupNumber']
        products = case.products
        employee_data = dict(
            first=data['eeFName'],
            last=data['eeLName'],
            email=data['email'],
            ssn=data.get('ssn', ''),
            state=state,
            occupation=data['occupation_class'],
        )
        spouse_data = None
        children_data = []

    # Validate that we can enroll in the product for this state - do we have a form?
    if not any(
            product_form_service.form_for_product_code_and_state(p.get_base_product_code(), state) for p in products):
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

    # New wizard formatting for multiproduct.
    applicants = []
    employee_data['type'] = u'employee'
    applicants.append(employee_data)
    if spouse_data:
        spouse_data['type'] = u'spouse'
        applicants.append(spouse_data)
    if children_data:
        for child in children_data:
            child['type'] = u'children'
            applicants.append(child)

    # Any FPP product for acknowledgment / disclosure box?
    fpp_products = [p for p in products if p.is_fpp()]
    any_fpp_product = bool(fpp_products)
    fpp_base_product_code = None
    if any_fpp_product:
        fpp_base_product_code = fpp_products[0].get_base_product_code()

    occupations = case.occupation_class_settings if case.occupation_class_settings else []

    from taa.services.products.rates import get_height_weight_table_for_product

    height_weight_tables = dict()
    for product in products:
        table = get_height_weight_table_for_product(product)
        if table is not None:
            height_weight_tables[product.get_base_product_code()] = table

    wizard_data = dict(
        is_in_person=not is_self_enroll,
        case_data={
            'id': case.id,
            'situs_state': state if state != 'XX' else None,
            'situs_city': city,
            'enroll_city': city,
            'company_name': company_name,
            'group_number': group_number,
            'payment_mode': payment_mode,
            'product_settings': case.product_settings if case.product_settings else {},
            'account_href': current_user.get_id(),
            'record_id': record_id,
            'product_height_weight_tables': height_weight_tables,
            'occupations': occupations,
        },
        applicants=applicants,
        products=[serialize_product_for_wizard(p, soh_questions) for p in case.products],
        payment_modes=payment_mode_choices,
        spouse_questions=spouse_questions,
        health_questions=soh_questions,
        any_fpp_product=any_fpp_product,
        fpp_base_product_code=fpp_base_product_code,
    )

    # Commit any changes made (none right now)
    db.session.commit()

    return render_template(
        'enrollment/main-wizard.html',
        wizard_data=wizard_data,
        states=get_states(),
        nav_menu=get_nav_menu(),
    )


def serialize_product_for_wizard(product, all_soh_questions):
    data = product.to_json()
    # Override the name to be the base product name
    data['name'] = product.get_base_product().name

    # Override code to be the base product code and alias it to base_product_type.
    data['code'] = product.get_base_product_code()
    data['base_product_type'] = data['code']
    data['soh_questions'] = all_soh_questions.get(product.id, [])
    return data


# Self Enrollment Landing Page
@app.route('/self-enroll/<string:company_name>/<string:uuid>')
def self_enrollment(company_name, uuid):
    setup, census_record = self_enrollment_link_service.get_self_enrollment_data_for(uuid,
                                                                                     current_user.is_anonymous())
    case = self_enrollment_link_service.get_case_for_link(uuid)

    is_self_enrollable = True
    if setup.self_enrollment_type == setup.TYPE_CASE_GENERIC:
        for product in case.products:
            if product.code in ['ACC', 'HI']:
                # Disallow generic-link self-enrollment cases containing
                # these products
                is_self_enrollable = False
                break

    if case_service.requires_occupation(case) and census_record.occupation_class not in map(lambda cr: cr['label'], case.occupation_class_settings):
        is_self_enrollable = False

    vars = {'is_valid': False, 'allowed_states': []}
    if setup is not None and is_self_enrollable:
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
            'page_title': u'Enrollment service for {} not available'.format(case.company_name),
            'error_message': u'''We're sorry for the inconvenience, but {} is not currently accepting benefit enrollments.<br><br>
            Please contact your enrollment or benefit representative if you have any questions.'''.format(
                case.company_name)

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
        data.update(get_case_enrollment_data(enrollment_setup.case))

    if 'enrollmentCity' in request.form:
        data['enrollmentCity'] = request.form['enrollmentCity']
    if 'enrollmentState' in request.form:
        data['enrollmentState'] = request.form['enrollmentState']

    return _setup_enrollment_session(enrollment_setup.case, record_id=census_record_id, data=data, is_self_enroll=True)


def get_case_enrollment_data(case):
    data = {}
    data['enrollmentState'] = case.situs_state
    data['enrollmentCity'] = case.situs_city
    data['companyName'] = case.company_name
    data['groupNumber'] = case.group_number
    data['productID'] = case.products[0].id
    data['eeFName'] = ""
    data['eeLName'] = ""
    data['email'] = ""
    return data


@app.route('/submit-wizard-data', methods=['POST'])
def submit_wizard_data():
    if session.get('active_case_id') is None:
        abort(401)

    case_id = session['active_case_id']
    case = case_service.get(case_id)

    data = request.json
    wizard_results = data['wizard_results']
    print("[ENROLLMENT SUBMITTED]")

    try:
        enrollment = process_wizard_submission(case, wizard_results)

        # Store the enrollment record ID in the session for now so we can access it on the landing page.
        session['enrollment_application_id'] = enrollment.id

        # Queue this call for a worker process to handle.
        enrollment_submission_service = LookupService('EnrollmentSubmissionService')
        enrollment_submission_service.submit_wizard_enrollment(enrollment)

        return jsonify(**{
            'error': False,
            'poll_url': url_for('check_submission_status', enrollment_id=enrollment.id),
            'enrollment_id': enrollment.id,
        })
    except Exception:
        print(u"[ENROLLMENT SUBMISSION ERROR]: (case {}) {}".format(case_id, wizard_results))
        raise


def process_wizard_submission(case, wizard_results):
    # Standardize the wizard data for submission processing
    standardized_data = enrollment_import_service.standardize_wizard_data(wizard_results)
    enrollment_data = EnrollmentDataWrap(standardized_data[0], case)

    # Save enrollment information and updated census data prior to DocuSign hand-off
    census_record = get_or_create_census_record(case, enrollment_data)
    enrollment_application = get_or_create_enrollment(case, census_record, standardized_data, wizard_results)
    db.session.commit()

    submission_service = LookupService('EnrollmentSubmissionService')
    submission_service.create_submissions_for_application(enrollment_application)

    return enrollment_application


@app.route('/check-submission-status', methods=['GET'])
def check_submission_status():
    enrollment_id = int(request.args['enrollment_id'])

    # For security, make sure we just submitted this enrollment (set up in submit_wizard_data.
    if not session.get('enrollment_application_id') == enrollment_id:
        abort(403)

    enrollment = enrollment_service.get(enrollment_id)
    received_enrollment_data = json.loads(enrollment.received_data)
    standardized_enrollment_data = json.loads(enrollment.standardized_data)

    if are_all_products_declined(received_enrollment_data):
        # Declined enrollment, return redirect to our landing page.
        redirect_url = url_for('ds_landing_page',
                               event='decline',
                               name=received_enrollment_data[0]['employee']['first'],
                               type='inperson' if received_enrollment_data[0][
                                                      "method"] == EnrollmentApplication.METHOD_INPERSON else 'email',
                               )
        return jsonify(status="declined", redirect_url=redirect_url)
    elif enrollment.docusign_envelope_id is not None:
        # Done processing this envelope, get the signing URL
        envelope = DocusignEnvelope(enrollment.docusign_envelope_id, enrollment_record=enrollment)
        data_wrap = EnrollmentDataWrap(standardized_enrollment_data[0], enrollment.case)
        return jsonify(**{'status': 'ready', 'redirect_url': get_envelope_signing_url(data_wrap, envelope)})
    else:
        # Not done processing yet
        return jsonify(status="pending")


def are_all_products_declined(standardized_data):
    return all(map(lambda data: data.get('did_decline'), standardized_data))


# def submit_enrollment_to_docusign(case, enrollment_application, enrollment_data, standardized_data, wizard_results):
#     # DocuSign submission
#     # Get or create envelope.
#     envelope = get_or_create_envelope(case, enrollment_application, standardized_data)
#
#     # Fetch signing URL
#     return get_envelope_signing_url(enrollment_data, envelope)


def get_enrollment_agent(case):
    # For self-enroll situations, the owner agent is used
    agent = agent_service.get_logged_in_agent()
    if (agent is None and session.get('is_self_enroll')):
        agent = case.owner_agent
    return agent


def get_or_create_census_record(case, enrollment_data):
    if session.get('enrolling_census_record_id'):
        census_record = case_service.get_census_record(case, session['enrolling_census_record_id'])
    else:
        # Attempt to match against SSN, Name, and DOB in case of a resubmission.
        census_record = case_service.match_census_record_to_wizard_data(enrollment_data)

    return census_record


def get_or_create_enrollment(case, census_record, standardized_data, wizard_results):
    # Get the agent for this session
    agent = get_enrollment_agent(case)

    # Don't create a new enrollment if there is an outstanding pending enrollment for this applicant.
    if census_record and census_record.get_pending_enrollments():
        enrollment_application = census_record.get_pending_enrollments()[0]
    else:
        # Create and save the enrollment data. Creates a census record if this is a generic link, and in
        #   either case updates the census record with the latest enrollment data.
        enrollment_application = enrollment_service.save_multiproduct_enrollment_data(
            standardized_data, case, census_record, agent,
            received_data=wizard_results,
        )

        # Save to DB now in case DocuSign takes too long and we time out.
        db.session.commit()

    return enrollment_application


def get_envelope_signing_url(enrollment_data, envelope):
    if enrollment_data.should_use_call_center_workflow():
        callback_url = build_callcenter_callback_url(enrollment_data.case)
        signing_url = envelope.get_agent_signing_url(callback_url)
    else:
        callback_url = build_callback_url(enrollment_data, enrollment_data.get_session_type())
        signing_url = envelope.get_employee_signing_url(callback_url)

    return signing_url


@app.route('/application_completed', methods=['GET'])
def ds_landing_page():
    """
    Handles simple responses to completing the enrollment page
    """
    session_type = request.args['type']
    name = request.args['name']
    ds_event = request.args['event']

    enrollment_application_id = session.get('enrollment_application_id')
    if enrollment_application_id:
        enrollment_application = enrollment_service.get(enrollment_application_id)
        if enrollment_application and enrollment_application.docusign_envelope_id and enrollment_application.is_terminal_status():
            enrollment_service.update_applicant_signing_status(enrollment_application, ds_event)

    # Need to commit all database changes.
    db.session.commit()

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


# Public flat file documenation endpoints
@app.route('/flat_file_documentation.pdf')
def flat_file_documentation():
    return send_from_directory(
        os.path.join(app.root_path, 'frontend', 'static'),
        'pdfs/documentation/flat_file_documentation.pdf')


@app.route('/flat_file_documentation.html')
def flat_file_documentation_html():
    from taa.services.data_import.file_import import FlatFileDocumentation
    documentation = FlatFileDocumentation.generate_html_docs()
    return Response(documentation)


@app.route('/delimited_file_import_documentation.html')
def delimited_file_documentation_html():
    from taa.services.data_import.file_import import CSVFileDocumentation
    documentation = CSVFileDocumentation.generate_html_docs()
    return Response(documentation)
