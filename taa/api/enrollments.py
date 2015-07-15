from flask import Blueprint, request, abort, make_response, jsonify, redirect, url_for, render_template
from flask_stormpath import current_user, groups_required, login_required


from taa import app
from taa.core import TAAFormError, db
from taa.helpers import get_posted_data
from taa.api import route

from taa.services.cases import CaseService
from taa.services.agents import AgentService
from taa.services.enrollments import EnrollmentApplicationService

from taa.services.docusign.docusign_envelope import create_envelope_and_get_signing_url

case_service = CaseService()
agent_service = AgentService()
enrollment_service = EnrollmentApplicationService()

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

@route(bp, '/submit-data', methods=["POST"])
def submit_data():
    data = request.json
    case = case_service.get(data["agent_data"]["case_id"])
    # Save enrollment information and updated census data prior to
    # DocuSign hand-off
    if data['census_record_id']:
        census_record = case_service.get_census_record(
            case, data['census_record_id'])
    else:
        census_record = None
    agent = agent_service.get_logged_in_agent()
    enrollment_application = enrollment_service.save_enrollment_data(
        data, case, census_record, agent)

    # Create and save the enrollment data. Creates a census record if this is a generic link, and in
    #   either case updates the census record with the latest enrollment data.

    if not data.get('did_decline'):
        # Hand off wizard_results to docusign
        is_error, error_message, redirect = create_envelope_and_get_signing_url(data, census_record, case)
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
                                name=data['employee']['first'],
                                type=data["agent_data"]["is_in_person"],
                                )
        }

    return resp
