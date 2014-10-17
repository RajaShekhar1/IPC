
from flask import Blueprint, request
from flask_stormpath import user, groups_required

from taa.core import TAAFormError
from taa.api import route
from taa.services.cases import CaseService
from taa.services.cases.forms import NewCaseForm, UpdateCaseForm

bp = Blueprint("cases", __name__, url_prefix='/cases')

case_service = CaseService()

@route(bp, "/")
@groups_required(['agents', 'admins'], all=False)
def get_cases():
    # TODO: limit to current agent
    agent_id = user.custom_data.get('agent_code')
    
    return case_service.all()
    

@route(bp, "/<case_id>")
@groups_required(['agents', 'admins'], all=False)
def get_case(case_id):
    return case_service.get_or_404(case_id)
    
@route(bp, "/", methods=["POST"])
@groups_required(['agents', 'admins'], all=False)
def create_case():
    data = request.get_json()
    if data.get('agent_id'):
        # TODO: Verify we can create for this agent
        #    if can_create_case_for_agent(request.form['agent_id']):
        #        raise 
        agent_id = data['agent_id']
    else:
        agent_id = user.custom_data.get('agent_id', 1)
        data['agent_id'] = agent_id
    
    form = NewCaseForm(form_data=data)
    form.agent_id.data = agent_id
    if form.validate_on_submit():
        return case_service.create(**request.json)
    
    raise TAAFormError(form.errors)

def can_create_case_for_agent(agent_id):
    logged_in_agent_id = user.custom_data.get('agent_code')
    return unicode(request.form['agent_id']) != unicode(agent_id) and not is_admin()
    
def is_admin():
    return 'admins' in [g.name for g in user.groups]

@route(bp, "/<case_id>", methods=["PUT"])
def update_case(case_id):
    form = UpdateCaseForm()
    if form.validate_on_submit():
        return case_service.update(case_service.get_or_404(case_id), **request.json)
    
    raise TAAFormError(form.errors)

@route(bp, "/<case_id>", methods=["DELETE"])
def delete_case(case_id):
    case_service.delete(case_service.get_or_404(case_id))
    return None, 204
