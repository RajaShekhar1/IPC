from flask import Blueprint, request, session
from flask_stormpath import login_required, groups_required, current_user
from taa.services.enrollments import EnrollmentApplicationService

from taa.api import route

bp = Blueprint('envelopes', __name__, url_prefix='/envelopes')
read_api_groups = ['agents', 'home_office', 'admins']


@route(bp, '/', methods=['GET'])
@login_required
@groups_required(read_api_groups, all=False)
def get_envelopes():
    """Get all the envelopes that the current user can see."""
    from taa.services.docusign import DocuSignService
    docusign_service = DocuSignService()

    return docusign_service.search_envelopes(for_user=current_user)

# This function is here for legacy reasons, but we don't use DocuSign redirect for signing anymore.
@route(bp, '/<envelope_id>/sign', methods=["POST"])
@login_required
@groups_required(read_api_groups, all=False)
def sign_envelope(envelope_id):
    from taa.services.docusign import DocuSignService
    docusign_service = DocuSignService()

    enrollment_record = docusign_service.get_enrollment_record_from_envelope(envelope_id)

    # To allow the callback page to know that we may have modified an enrollment, set a session variable.
    session['enrollment_application_id'] = enrollment_record.id

    if request.args.get('from') == 'inbox':
        callback_url = docusign_service.build_inbox_callback_url(enrollment_record)
    else:
        # Return the user to the census record view.
        callback_url = docusign_service.build_census_record_callback_url(enrollment_record)

    url, errors = docusign_service.get_envelope_signing_url(current_user, envelope_id, callback_url)

    return {'url': url, 'errors': errors}


@route(bp, '/sign-enrollment/<enrollment_id>', methods=["POST"])
@login_required
@groups_required(read_api_groups, all=False)
def sign_enrollment(enrollment_id):
    from taa.services.docusign import DocuSignService
    docusign_service = DocuSignService()
    enrollment_application_service = EnrollmentApplicationService()
    
    enrollment_record = enrollment_application_service.get(enrollment_id)

    # To allow the callback page to know that we may have modified an enrollment, set a session variable.
    session['enrollment_application_id'] = enrollment_record.id

    if request.args.get('from') == 'inbox':
        callback_url = docusign_service.build_inbox_callback_url(enrollment_record)
    else:
        # Return the user to the census record view.
        callback_url = docusign_service.build_census_record_callback_url(enrollment_record)

    #url, errors = docusign_service.get_envelope_signing_url(current_user, envelope_id, callback_url)
    
    errors = docusign_service.sign_enrollment(current_user, enrollment_record)
    
    return {'errors': errors}
