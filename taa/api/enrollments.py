from StringIO import StringIO

from flask import Blueprint, request

from taa.api import route
from taa.services import LookupService


bp = Blueprint('enrollments', __name__, url_prefix='/enrollments')
case_service = LookupService("CaseService")
enrollment_import_service = LookupService("EnrollmentImportService")

@route(bp, '/', methods=["POST"])
def submit_data():
    case_token = request.args.get('case_token')
    auth_token = request.args.get('auth_token')
    email_errors = bool(request.args.get('email_errors'))
    data_format = request.args.get('format', 'json')
    data = StringIO(request.data)

    import_results = enrollment_import_service.process_enrollment_data(
        data,
        data_format,
        case_token=case_token,
        auth_token=auth_token,
        email_errors=email_errors
    )

    return {
        'num_processed': import_results.get_num_processed(),
        'num_errors': len(import_results.get_errors())
    }

    # data = request.json
    # case = case_service.get(data["agent_data"]["case_id"])
    # # Save enrollment information and updated census data prior to
    # # DocuSign hand-off
    # if data['census_record_id']:
    #     census_record = case_service.get_census_record(
    #         case, data['census_record_id'])
    # else:
    #     census_record = None
    # agent = agent_service.get_logged_in_agent()
    # enrollment_application = enrollment_service.save_enrollment_data(
    #     data, case, census_record, agent)
    #
    #
    # if not data.get('did_decline'):
    #     # Hand off wizard_results to docusign
    #     is_error, error_message, redirect = create_envelope_and_get_signing_url(data, census_record, case)
    #     # Return the redirect url or error
    #     resp = {'error': is_error,
    #             'error_message': error_message,
    #             'redirect': redirect}
    # else:
    #     # Declined
    #     resp = {
    #         'error': False,
    #         'error_message': '',
    #         'redirect': url_for('ds_landing_page',
    #                             event='decline',
    #                             name=data['employee']['first'],
    #                             type=data["agent_data"]["is_in_person"],
    #                             )
    #     }
    #
    # return resp
