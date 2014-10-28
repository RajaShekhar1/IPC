import os
import csv

from flask import Blueprint, request, abort, redirect, url_for
from flask_stormpath import user, groups_required

from taa import app
from taa.core import TAAFormError
from taa.helpers import get_posted_data
from taa.api import route
from taa.services.cases import CaseService
from taa.services.cases.forms import (
    NewCaseForm, 
    UpdateCaseForm, 
    CensusRecordForm,
    NewCaseEnrollmentPeriodForm,
)
from taa.services.agents import AgentService
from taa.services.products import ProductService

bp = Blueprint("cases", __name__, url_prefix='/cases')

case_service = CaseService()
agent_service = AgentService()
product_service = ProductService()

api_groups = ['agents', 'admins']

# Case management endpoints

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
    data = get_posted_data()
    # Todo: perhaps accept agent_id in form data for admin usage
    agent = agent_service.get_logged_in_agent()
    if not agent:
        abort(401)

    # Remap some naming differences
    data['agent_id'] = agent.id
    if 'case_product' in data:
        data['products'] = [product_service.get_product_by_code_or_400(data['case_product'])]
    
    if 'active' not in data:
        # Deactivate?
        data['active'] = False
    
    form = UpdateCaseForm()
    form.agent_id.data = agent.id
    form.products.data = [p.code for p in data['products']]
    if form.validate_on_submit():
        return case_service.update(case, **data)
    
    raise TAAFormError(form.errors)

@route(bp, "/<case_id>", methods=["DELETE"])
@groups_required(api_groups, all=False)
def delete_case(case_id):
    case_service.delete(case_service.get_if_allowed(case_id))
    return None, 204



# Enrollment Periods
@route(bp, "/<case_id>/enrollment_periods", methods=['GET'])
@groups_required(api_groups, all=False)
def get_case_enrollment_periods(case_id):
    return case_service.get_enrollment_periods(case_service.get_if_allowed(case_id))

@route(bp, "/<case_id>/enrollment_periods", methods=['PUT'])
@groups_required(api_groups, all=False)
def update_case_enrollment_periods(case_id):
    """
    When posting to case_enrollment_periods, we check the type of the added period.
    If it is not the same as the current enrollment period type, we change the type and remove
    all existing enrollment periods to ensure all of a case's enrollment periods are of the same type.
    """
    case = case_service.get_if_allowed(case_id)
    
    form = NewCaseEnrollmentPeriodForm()
    if form.validate_on_submit():
        return case_service.update_enrollment_periods(case, **form.data)
    
    raise TAAFormError(form.errors)
    

# Census Records

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
    
        if data['upload_type'] == "merge-skip":
            errors, records = case_service.merge_census_data(case, csv_reader, replace_matching=False)
        elif data['upload_type'] == "merge-replace":
            errors, records = case_service.merge_census_data(case, csv_reader, replace_matching=True)
        else:
            errors, records = case_service.replace_census_data(case, csv_reader)
    else:
        return dict(errors=[[dict(message='Invalid file format. Filename must end with .csv, and follow the specification exactly. See sample upload file.')]], records=[])
    
    return dict(errors=errors, records=records)
    
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