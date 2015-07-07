from flask import Blueprint, request, abort, make_response, jsonify, redirect, url_for, render_template
from flask_stormpath import current_user, groups_required, login_required


from taa import app
from taa.core import TAAFormError, db
from taa.helpers import get_posted_data
from taa.api import route

bp = Blueprint('enrollments', __name__, url_prefix='/enrollments')

# Wizard original endpoint data
# health_questions: $.map(window.ui.health_questions(), function(q) {return q.question}),
# agent_data: agent_data,
# enrollCity:  window.ui.enrollCity(),
# enrollState:  window.ui.enrollState,
# product_type: window.ui.insurance_product.product_type,
# payment_mode: window.ui.payment_mode(),
# payment_mode_text: window.ui.payment_mode_text_lower(),
#
# method: (ui.is_in_person_application()) ? 'in_person': 'self_enroll_email',
# did_decline: ui.did_decline(),
#
# identityToken: window.ui.identityToken(),
# identityType: window.ui.identityType(),
#
# employee: window.ui.employee().serialize_data(),
# spouse: window.ui.spouse().serialize_data(),
#
# is_spouse_address_same_as_employee: ui.is_spouse_address_same_as_employee(),
# is_spouse_email_same_as_employee: ui.is_spouse_email_same_as_employee(),
#
# existing_insurance:  window.ui.existing_insurance(),
# replacing_insurance:  window.ui.replacing_insurance(),
# is_employee_actively_at_work: ui.is_employee_actively_at_work(),
# has_spouse_been_treated_6_months: ui.has_spouse_been_treated_6_months(),
# has_spouse_been_disabled_6_months: ui.has_spouse_been_disabled_6_months(),
#
# employee_owner:  window.ui.policy_owner(),
# employee_other_owner_name:  window.ui.other_owner_name(),
# employee_other_owner_ssn:  window.ui.other_owner_ssn(),
# spouse_owner:  window.ui.spouse_policy_owner(),
# spouse_other_owner_name:  window.ui.spouse_other_owner_name(),
# spouse_other_owner_ssn:  window.ui.spouse_other_owner_ssn(),
#
# employee_beneficiary:  window.ui.employee_beneficiary_type(),
# spouse_beneficiary:  window.ui.spouse_beneficiary_type(),
# employee_contingent_beneficiary_type: window.ui.employee_contingent_beneficiary_type(),
# employee_contingent_beneficiary: window.ui.employee_contingent_beneficiary().serialize(),
#
# employee_beneficiary_name:  window.ui.employee_other_beneficiary().name(),
# employee_beneficiary_relationship:  window.ui.employee_other_beneficiary().relationship(),
# employee_beneficiary_ssn:  window.ui.employee_other_beneficiary().ssn(),
# employee_beneficiary_dob:  window.ui.employee_other_beneficiary().date_of_birth(),
#
# spouse_beneficiary_name:  window.ui.spouse_other_beneficiary().name(),
# spouse_beneficiary_relationship:  window.ui.spouse_other_beneficiary().relationship(),
# spouse_beneficiary_ssn:  window.ui.spouse_other_beneficiary().ssn(),
# spouse_beneficiary_dob:  window.ui.spouse_other_beneficiary().date_of_birth(),
# spouse_contingent_beneficiary_type: window.ui.spouse_contingent_beneficiary_type(),
# spouse_contingent_beneficiary: window.ui.spouse_contingent_beneficiary().serialize()
# };

@route(bp, '/')
def home():
    return "Hallo world"

@route(bp, '/<case_id>/submit-data', methods=["POST"])
def submit_data(case_id):
    case = case_service.get(case_id)
    data = request.form or request.json
    """
    wizard_results = data['wizard_results']
    print("[ENROLLMENT SUBMITTED]: (case {}) {}".format(case_id, wizard_results))
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
                                type=wizard_results["agent_data"]["is_in_person"],
                                )
        }
    data = jsonify(**resp)
    # Need to manually commit all changes since this doesn't go through the API
    # right now
    db.session.commit()
    """
    return data
