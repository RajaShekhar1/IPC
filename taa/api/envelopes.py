from flask import Blueprint, request
from flask_stormpath import login_required, groups_required, current_user

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


@route(bp, '/<envelope_id>/sign', methods=["POST"])
@login_required
@groups_required(read_api_groups, all=False)
def sign_envelope(envelope_id):
    from taa.services.docusign import DocuSignService
    docusign_service = DocuSignService()

    url = docusign_service.get_envelope_signing_url(current_user, envelope_id)

    return {'url': url}
