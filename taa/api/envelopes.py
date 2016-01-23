from flask import Blueprint, request
from flask_stormpath import login_required, groups_required, current_user

from taa.core import TAAFormError
from taa.api import route
from taa.helpers import get_posted_data
from taa.services.docusign import DocuSignService

bp = Blueprint('envelopes', __name__, url_prefix='/envelopes')
read_api_groups = ['agents', 'home_office', 'admins']

docusign_service = DocuSignService()

@route(bp, '/', methods=['GET'])
@login_required
@groups_required(read_api_groups, all=False)
def get_envelopes():
    "Get all the envelopes that the current user can see."

    return docusign_service.search_envelopes(for_user=current_user)


@route(bp, '/<envelope_id>')
@login_required
@groups_required(read_api_groups, all=False)
def get_product(envelope_id):
    return docusign_service.get_or_404(envelope_id)


