from datetime import datetime
import StringIO

from flask import Blueprint, request, abort, make_response, jsonify, redirect, url_for
from flask_stormpath import current_user, groups_required

from taa import app
from taa.core import TAAFormError
from taa.helpers import get_posted_data
from taa.api import route
from taa.services.cases import CaseService
from taa.services.cases.forms import (
    NewCaseForm,
    UpdateCaseForm,
    CensusRecordForm,
)
from taa.services.agents import AgentService
from taa.services.products import ProductService

bp = Blueprint('cases', __name__, url_prefix='/cases')

case_service = CaseService()
agent_service = AgentService()
product_service = ProductService()

api_groups = ['agents', 'home_office', 'admins']


# Case management endpoints
@route(bp, '/')
@groups_required(api_groups, all=False)
def get_cases():

    name_filter = request.args.get('by_name')

    agent = agent_service.get_logged_in_agent()
    if agent:
        return case_service.search_cases(by_agent=agent.id, by_name=name_filter)

    # Return all cases for admin
    if agent_service.can_manage_all_cases(current_user):
        return case_service.search_cases(by_name=name_filter)

    abort(401)


@route(bp, '/<case_id>')
@groups_required(api_groups, all=False)
def get_case(case_id):
    return case_service.get_if_allowed(case_id)


@route(bp, '/', methods=['POST'])
@groups_required(api_groups, all=False)
def create_case():
    data = get_posted_data()

    # Determine the owning agent
    if agent_service.can_manage_all_cases(current_user):
        agent = None
    elif agent_service.is_user_agent(current_user):
        # The creating agent is the owner by default
        agent = agent_service.get_logged_in_agent()
        data['agent_id'] = agent.id
    else:
        # We don't have permission to create cases
        abort(401)
        return

    form = NewCaseForm(form_data=data)
    if agent:
        form.agent_id.data = agent.id
    if form.validate_on_submit():
        data['created_date'] = datetime.now()
        return case_service.create(**data)

    raise TAAFormError(form.errors)


@route(bp, '/<case_id>', methods=['PUT'])
@groups_required(api_groups, all=False)
def update_case(case_id):
    case = case_service.get_if_allowed(case_id)
    data = get_posted_data()
    is_admin = agent_service.can_manage_all_cases(current_user)
    if not agent_service.get_logged_in_agent() and not is_admin:
        abort(401)
        return
    form = UpdateCaseForm()
    # Allow the owner agent to be updated only if an admin
    if 'agent_id' in data:
        if not is_admin:
            del data['agent_id']
        else:
            data['agent_id'] = int(data['agent_id']) if (
                data['agent_id'] and data['agent_id'].isdigit()) else None
            form.agent_id.data = data['agent_id']
    form.products.data = [p['id'] for p in data['products']]
    if form.validate_on_submit():
        # Update products
        case_service.update_products(
            case, [p for p in product_service.get_all(*form.products.data)])
        # Update partner agents
        if is_admin:
            case_service.update_partner_agents(
                case, [a for a in agent_service.get_all(
                    *data['partner_agents'])])
        # Update case table (these keys must be removed for the main case
        # update)
        del data['products']
        del data['partner_agents']
        return case_service.update(case, **data)
    raise TAAFormError(form.errors)


@route(bp, '/<case_id>', methods=['DELETE'])
@groups_required(api_groups, all=False)
def delete_case(case_id):
    case_service.delete_case(case_service.get_if_allowed(case_id))
    return None, 204


# Enrollment Periods
@route(bp, '/<case_id>/enrollment_periods', methods=['GET'])
@groups_required(api_groups, all=False)
def get_case_enrollment_periods(case_id):
    return case_service.get_enrollment_periods(case_service.get_if_allowed(
        case_id))


@route(bp, '/<case_id>/enrollment_periods', methods=['PUT'])
@groups_required(api_groups, all=False)
def update_case_enrollment_periods(case_id):
    """
    When posting to case_enrollment_periods, we check the type of the added
    period. If it is not the same as the current enrollment period type, we
    change the type and remove all existing enrollment periods to ensure all
    of a case's enrollment periods are of the same type.
    """
    case = case_service.get_if_allowed(case_id)
    periods = request.json
    errors = case_service.validate_enrollment_periods(case, periods)
    if not errors:
        return case_service.update_enrollment_periods(case, periods)

    raise TAAFormError(errors)


# Enrollment Reports
@route(bp, '/<case_id>/enrollment_report', methods=['GET'])
@groups_required(api_groups, all=False)
def enrollment_report(case_id):
    from taa.services.enrollments import EnrollmentApplicationService
    return EnrollmentApplicationService().get_enrollment_report(
        case_service.get_if_allowed(case_id))


@route(bp, '/<case_id>/enrollment_records', methods=['GET'])
@groups_required(api_groups, all=False)
def enrollment_records(case_id):
    """
    Combines the census and enrollment records for export.

    format=json|csv (json by default)
    """
    from taa.services.enrollments import EnrollmentApplicationService
    enrollment_service = EnrollmentApplicationService()
    data = enrollment_service.get_all_enrollment_records(
        case_service.get_if_allowed(case_id))
    if request.args.get('format') == 'csv':
        body = enrollment_service.export_enrollment_data(data)
        date_str = datetime.now().strftime('%Y-%m-%d')
        headers = {
            'Content-Type': 'text/csv',
            'Content-Disposition':
                'attachment; filename=enrollment_export_{0}.csv'.format(
                    date_str)
        }
        return make_response(body, 200, headers)
    return data


# Census Records
@route(bp, '/<case_id>/census_records', methods=['GET'])
@groups_required(api_groups, all=False)
def census_records(case_id):
    # Extract search parameters
    args = {
        'filter_ssn': request.args.get('filter_ssn'),
        'filter_birthdate': request.args.get('filter_birthdate'),
    }
    data = case_service.get_census_records(case_service.get_if_allowed(case_id),
                                           **args)
    if request.args.get('format') == 'csv':
        body = case_service.export_census_records(data)
        date_str = datetime.now().strftime('%Y-%m-%d')
        headers = {
            'Content-Type': 'text/csv',
            'Content-Disposition':
                'attachment; filename=case_export_{0}.csv'.format(date_str)
        }
        return make_response(body, 200, headers)
    return data


@route(bp, '/<case_id>/census_records', methods=['POST'])
@groups_required(api_groups, all=False)
def post_census_records(case_id):
    case = case_service.get_if_allowed(case_id)
    data = get_posted_data()
    file_obj = request.files.get('csv-file')
    if not file_obj:
        # Attempt to process an ad-hoc post. Currently only SSN is required.
        return case_service.create_ad_hoc_census_record(case, ssn=data['ssn'])
    if not (file_obj and has_csv_extension(file_obj.filename)):
        return dict(
            errors=[dict(
                message='Invalid file format. Filename must end with .csv, '
                        'and follow the specification exactly. See sample '
                        'upload file.',
                records=[]
            )]
        )

    # Process the CSV Data

    # Read data into a buffer
    file_data = StringIO.StringIO()
    file_obj.save(file_data)
    if data['upload_type'] == 'merge-skip':
        errors, records = case_service.merge_census_data(case, file_data,
                                                         replace_matching=False)
    elif data['upload_type'] == 'merge-replace':
        errors, records = case_service.merge_census_data(case, file_data,
                                                         replace_matching=True)
    else:
        errors, records = case_service.replace_census_data(case, file_data)
    # Return at most 20 errors at a time
    # returns all added or changed records
    status = 400 if errors else 200
    return dict(errors=errors[:20], records=records), status


def has_csv_extension(filename):
    return '.' in filename and filename.lower().rsplit('.', 1)[1] == 'csv'


@route(bp, '/<case_id>/census_records/<census_record_id>', methods=['PUT'])
@groups_required(api_groups, all=False)
def update_census_record(case_id, census_record_id):
    case = case_service.get_if_allowed(case_id)
    census_record = case_service.get_census_record(case, census_record_id)
    form = CensusRecordForm()
    if form.validate_on_submit():
        return case_service.update_census_record(census_record, form.data)
    raise TAAFormError(form.errors)


@route(bp, '/<case_id>/census_records/<census_record_id>', methods=['DELETE'])
@groups_required(api_groups, all=False)
def delete_census_record(case_id, census_record_id):
    case = case_service.get_if_allowed(case_id)
    census_record = case_service.get_census_record(case, census_record_id)
    case_service.delete_census_record(census_record)
    return None, 204
