"""--------------------------------------------------------------
ENROLLMENT pages and handling, DOCUSIGN interaction
"""

import os
import json

from flask import request, render_template, jsonify, session, send_from_directory
from flask.ext.stormpath import login_required

from taa import app
from taa.model.ProductData import get_age_from_birthday, get_product_by_code
from taa.model.States import get_states
from taa.model.Enrollment import (
    get_enrollment_setup_form_for_product,
    get_product_states,
)
from taa.services.docusign.docusign_envelope import create_envelope_and_get_signing_url
#from taa.model.Database import Database

@app.route("/get_rates", methods=['POST'])
def rates():
    
    # Pull parameters from the request
    employee_birthdate = request.form['employee_birthdate']
    spouse_birthdate = request.form.get('spouse_birthdate', None)
    num_children = int(request.form.get('num_children', 0))
    product_code = request.form['product_type']
    
    product = get_product_by_code(product_code)
    employee_age = get_age_from_birthday(employee_birthdate)
    spouse_age = get_age_from_birthday(spouse_birthdate) if spouse_birthdate else None
    
    response = {
        'employee_rates': product.get_employee_rates(employee_age),
        'spouse_rates': product.get_spouse_rates(spouse_age),
        'children_rates': product.get_children_rates(num_children),
        'recommendations': product.get_recommended_coverages(employee_age, spouse_age, num_children),
    }
    
    return jsonify(**response)

@app.route("/enroll")
@login_required
def enroll_start():
    
    if session.get('active_case'):
        product_code = session['active_case']['product_code']
        
        form = get_enrollment_setup_form_for_product(product_code)()
        form.companyName.data = session['active_case']['company_name']
        form.enrollmentCity.data = session['active_case']['situs_city']
        form.enrollmentState.data = session['active_case']['situs_state']
        form.productID.data = product_code
    else:
        form = get_enrollment_setup_form_for_product(None)()
        
    return render_template('setup-enrollment.html', 
                           form=form, 
                           product_states=get_product_states())

@app.route("/in-person-enrollment", methods=['POST'])
@login_required
def in_person_enrollment():
    state = request.form['enrollmentState']
    enroll_city = request.form['enrollmentCity']
    company_name = request.form['companyName']
    product_code = request.form['productID']
    employee_first = request.form['eeFName']
    employee_last = request.form['eeLName']
    employee_email = request.form['email']
    
    product = get_product_by_code(product_code)
    
    # refresh active_case
    session['active_case'] = {
        'company_name': company_name,
        'situs_state': state,
        'situs_city': enroll_city,
        'product_code': product_code
    }
    
    wizard_data = {
        'state': state if state != 'XX' else None,
        'enroll_city': enroll_city,
        'company_name': company_name,
        'product_id':product_code,
        'product_name': product.name,
        'employee_first':employee_first,
        'employee_last':employee_last,
        'employee_email':employee_email,
        'is_in_person':True,
        'health_questions':product.get_health_questions(state),
    }
    
    return render_template(
        'main-wizard.html', 
        wizard_data=wizard_data,
        states=get_states()
    )


@app.route("/submit-wizard-data", methods=['POST'])
def submit_wizard_data():
    
    data = json.loads(request.data)
    
    wizard_results = data['wizard_results']
    # When DEBUG...
    #print "--------------------"
    #print wizard_results
    #print "--------------------"
    #sys.stdout.flush()
    
    
    # Do docusign with data in wizard_results
    #
    is_error, error_message, redirect = create_envelope_and_get_signing_url(wizard_results);
    
    # Return the redirect url or error
    resp = {'error': is_error, 'error_message': error_message, "redirect": redirect}
    return jsonify(**resp)
    

@app.route("/application_completed", methods=['GET'])
def ds_landing_page():
    """
    Handles simple responses to completing the enrollment page
    """

    session_type = request.args['type']
    name = request.args['name']
    ds_event = request.args['event']

    return render_template('completed-session.html',
                           session_type=session_type,
                           name=name,
                           ds_event=ds_event)


 


@app.route("/email-enrollment", methods=['POST'])
@login_required
def email_enrollment():
    enrollment_state = request.form['enrollmentState']
    enrollment_city = request.form['enrollmentCity']
    company_name = request.form['companyName']
    product_code = request.form['productID']
    employee_first = request.form['eeFName']
    employee_last = request.form['eeLName']
    employee_email = request.form['email']

    db = get_database()
    
    # product = get_product_by_code(product_code)
    product = db.get_product_by_code(product_code)
    
    # May not want to create a case for each time this is called 
    case = Case(
        id=None,
        company_name=company_name,
        situs_city=enrollment_city,
        situs_state=enrollment_state,
        product=product,
    )
    db.save_case(case)
    
    enrollment = Enrollment(
        id=None,
        case=case,
        employee_first=employee_first,
        employee_last=employee_last,
        employee_email=employee_email,
    )
    db.save_enrollment(enrollment)

    enrollment_request = enrollment.generate_enrollment_request()
    db.save_enrollment_request(enrollment_request)
    
    email_config = dict(
        smtp_server=config.get('email', 'smtp_server'),
        smtp_port=config.get('email', 'smtp_port'),
        smtp_user=config.get('email', 'smtp_username'),
        smtp_password=config.get('email', 'smtp_password'),
        from_address=config.get('email', 'from_address'),
    )

    EnrollmentEmail(**email_config).send_enrollment_request(enrollment_request)
    
    return jsonify(**dict(success=True))

@app.route("/enrollment_request", methods=['GET'])
def email_link_handler():
    """
    Handles someone clicking the link in the enrollment request email
    """
    
    token = request.args['token']
    db = get_database()
    
    enrollment_request = db.get_enrollment_request_by_token(token)
    
    # TODO
    #if enrollment_request.is_expired():
    #    return render_template('token_expired.html')
    
    enrollment = enrollment_request.enrollment
    case = enrollment.case
    # 2014-06-05 product.get_health_questions() blowing up below - not yet handled in DB?
    # product = case.product
    product = get_product_by_code(case.product.code)

    wizard_data = {
        'state': case.situs_state,
        'enrollment_city': case.situs_city,
        'company_name': case.company_name,
        'product_id': case.product.code,
        'employee_first': enrollment.employee_first,
        'employee_last': enrollment.employee_last,
        'employee_email': enrollment.employee_email,
        'is_in_person':False,
        'health_questions': product.get_health_questions(),
    }

    return render_template('main-wizard.html',
                           wizard_data=wizard_data,
                           states=get_states(),
    )


def get_database():
   return Database(config.get('database', 'connection_string'))


@app.route("/FPPTI_disclosure.pdf")
def FPPTI_disclosure_generic():
    return send_from_directory(os.path.join(app.root_path, 'static'), 'pdfs/FPPTI_disclosure_generic.pdf')
@app.route("/FPPTI_disclosure_TX.pdf")
def FPPTI_disclosure_TX():
    return send_from_directory(os.path.join(app.root_path, 'static'), 'pdfs/FPPTI_disclosure_TX.pdf')
@app.route("/FPPTI_disclosure_KS.pdf")
def FPPTI_disclosure_KS():
    return send_from_directory(os.path.join(app.root_path, 'static'), 'pdfs/FPPTI_disclosure_KS.pdf')
@app.route("/FPPTI_disclosure_OR.pdf")
def FPPTI_disclosure_OR():
    return send_from_directory(os.path.join(app.root_path, 'static'), 'pdfs/FPPTI_disclosure_OR.pdf')
@app.route("/FPPTI_disclosure_VA.pdf")
def FPPTI_disclosure_VA():
    return send_from_directory(os.path.join(app.root_path, 'static'), 'pdfs/FPPTI_disclosure_VA.pdf')
@app.route("/FPPCI_disclosure.pdf")
def FPPCI_disclosure_generic():
    return send_from_directory(os.path.join(app.root_path, 'static'), 'pdfs/FPPCI_disclosure_generic.pdf')
@app.route("/FPPCI_disclosure_KS.pdf")
def FPPCI_disclosure_KS():
    return send_from_directory(os.path.join(app.root_path, 'static'), 'pdfs/FPPCI_disclosure_KS.pdf')
@app.route("/FPPCI_disclosure_OR.pdf")
def FPPCI_disclosure_OR():
    return send_from_directory(os.path.join(app.root_path, 'static'), 'pdfs/FPPCI_disclosure_OR.pdf')
@app.route("/FPPCI_disclosure_VA.pdf")
def FPPCI_disclosure_VA():
    return send_from_directory(os.path.join(app.root_path, 'static'), 'pdfs/FPPCI_disclosure_VA.pdf')
