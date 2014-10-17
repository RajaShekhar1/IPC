import os
import csv

from flask import Blueprint, request, abort, redirect, url_for
from flask_stormpath import user, groups_required

from taa import app
from taa.core import TAAFormError
from taa.helpers import get_posted_data
from taa.api import route
from taa.services.cases import CaseService
from taa.services.cases.forms import NewCaseForm, UpdateCaseForm, CensusRecordForm
from taa.services.agents import AgentService

bp = Blueprint("cases", __name__, url_prefix='/cases')

case_service = CaseService()
agent_service = AgentService()

api_groups = ['agents', 'admins']


@route(bp, "/")
@groups_required(api_groups, all=False)
def get_cases():
    
    agent = agent_service.get_logged_in_agent()
    if agent:
        return case_service.get_agent_cases(agent)
    
    if agent_service.is_user_admin(user):
        return case_service.all()
    
    abort(401)

@route(bp, "/<case_id>")
@groups_required(api_groups, all=False)
def get_case(case_id):
    return case_service.get_if_allowed(case_id)
    
@route(bp, "/", methods=["POST"])
@groups_required(api_groups, all=False)
def create_case():
    data = get_posted_data()
    
    # Get agent 
    agent = agent_service.get_logged_in_agent()
    if not agent:
        abort(401)
    
    # Todo: perhaps accept agent_id in form data for admin usage
    
    data['agent_id'] = agent.id
    
    form = NewCaseForm(form_data=data)
    form.agent_id.data = agent.id
    if form.validate_on_submit():
        return case_service.create(**data)
    
    raise TAAFormError(form.errors)

@route(bp, "/<case_id>", methods=["PUT"])
@groups_required(api_groups, all=False)
def update_case(case_id):
    
    case = case_service.get_if_allowed(case_id)
    
    form = UpdateCaseForm()
    if form.validate_on_submit():
        return case_service.update(case, **get_posted_data())
    
    raise TAAFormError(form.errors)

@route(bp, "/<case_id>", methods=["DELETE"])
@groups_required(api_groups, all=False)
def delete_case(case_id):
    case_service.delete(case_service.get_if_allowed(case_id))
    return None, 204
    
@route(bp, "/<case_id>/census_records", methods=["GET"])
@groups_required(api_groups, all=False)
def census_records(case_id):
    return case_service.get_census_records(case_service.get_if_allowed(case_id))

@route(bp, "/<case_id>/census_records", methods=["POST"])
@groups_required(api_groups, all=False)
def create_census_records(case_id):
    case = case_service.get_if_allowed(case_id)
    data = get_posted_data()
    
    file_obj = request.files['csv-file']
    if file_obj and has_csv_extension(file_obj.filename):
        csv_reader = csv.DictReader(file_obj.stream, restkey="extra")
    
        if data['upload_type'] == "merge":
            case_service.merge_census_data(case, csv_reader)
        else:
            case_service.replace_census_data(case, csv_reader)
    
    return dict(errors=[])
  
def has_csv_extension(filename):
    return '.' in filename and filename.lower().rsplit('.', 1)[1] == 'csv'



@route(bp, "/<case_id>/census_records/<census_record_id>", methods=["PUT"])
@groups_required(api_groups, all=False)
def update_census_record(case_id, census_record_id):
    case = case_service.get_if_allowed(case_id)
    census_record = case_service.get_census_record(case, census_record_id)

    form = CensusRecordForm()
    if form.validate_on_submit():
        return case_service.update_census_record(census_record, get_posted_data())
    raise TAAFormError(form.errors)
    

@route(bp, "/<case_id>/census_records/<census_record_id>", methods=["DELETE"])
@groups_required(api_groups, all=False)
def delete_census_record(case_id, census_record_id):
    case = case_service.get_if_allowed(case_id)
    census_record = case_service.get_census_record(case, census_record_id)
    case_service.delete_census_record(census_record)    
    return None, 204