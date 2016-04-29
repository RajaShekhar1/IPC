import json
from datetime import datetime

import re
from flask import Blueprint, request, abort, make_response, Response
from flask_stormpath import current_user, groups_required, login_required
from taa import JSONEncoder

from taa.core import TAAFormError, db
from taa.helpers import get_posted_data
from taa.api import route
from taa.services.cases import CaseService, SelfEnrollmentSetup, AgentSplitsSetup
from taa.services.products.riders import RiderService
from taa.services.cases.forms import (
    CensusRecordForm,
    NewCaseForm,
    SelfEnrollmentSetupForm,
    UpdateCaseForm,
)
from taa.services.enrollments.models import EnrollmentApplication
from taa.services import LookupService

bp = Blueprint('cases', __name__, url_prefix='/cases')

case_service = LookupService('CaseService')
rider_service = RiderService()
agent_service = LookupService('AgentService')
product_service = LookupService('ProductService')
self_enrollment_email_service = LookupService('SelfEnrollmentEmailService')
self_enrollment_link_service = LookupService('SelfEnrollmentLinkService')
enrollment_application_service = LookupService('EnrollmentApplicationService')

api_groups = ['agents', 'home_office', 'admins']


# Case management endpoints
@route(bp, '/')
@login_required
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
@login_required
@groups_required(api_groups, all=False)
def get_case(case_id):
    return case_service.get_if_allowed(case_id)


@route(bp, '/', methods=['POST'])
@login_required
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

    # Make sure this case name isn't used already
    existing_cases = case_service.search_cases(by_name=data['company_name'])
    if existing_cases:
        raise TAAFormError(errors=[{'error': 'Case already exists with the name "%s"' % data['company_name']}])

    form = NewCaseForm(form_data=data)
    if agent:
        form.agent_id.data = agent.id
    if form.validate_on_submit():
        data['created_date'] = datetime.now()
        return case_service.create_new_case(**data)

    raise TAAFormError(form.errors)


@route(bp, '/<case_id>', methods=['PUT'])
@login_required
@groups_required(api_groups, all=False)
def update_case(case_id):
    case = case_service.get_if_allowed(case_id)
    is_admin = agent_service.can_manage_all_cases(current_user)
    if not case_service.can_current_user_edit_case(case):
        abort(401)
        return

    form = UpdateCaseForm()
    data = get_posted_data()
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
        case_service.update_products(case, data['products'])
        # Update partner agents
        if is_admin:
            case_service.update_partner_agents(
                case, [a for a in agent_service.get_all(
                    *data['partner_agents'])])

        # Update the product settings
        case_service.update_product_settings(case, data['product_settings'])

        # Before updating the case table, these keys must be removed for the main case update.
        del data['products']
        del data['partner_agents']
        del data['product_settings']

        # Update case table
        return case_service.update(case, **data)

    raise TAAFormError(form.errors)


@route(bp, '/<case_id>', methods=['DELETE'])
@login_required
@groups_required(api_groups, all=False)
def delete_case(case_id):
    case = case_service.get_if_allowed(case_id)
    if not case_service.can_current_user_edit_case(case):
        abort(401)
        return

    case_service.delete_case(case)
    return None, 204


# Enrollment Periods
@route(bp, '/<case_id>/enrollment_periods', methods=['GET'])
@login_required
@groups_required(api_groups, all=False)
def get_case_enrollment_periods(case_id):
    return case_service.get_enrollment_periods(case_service.get_if_allowed(
        case_id))


@route(bp, '/<case_id>/enrollment_periods', methods=['PUT'])
@login_required
@groups_required(api_groups, all=False)
def update_case_enrollment_periods(case_id):
    """
    When posting to case_enrollment_periods, we check the type of the added
    period. If it is not the same as the current enrollment period type, we
    change the type and remove all existing enrollment periods to ensure all
    of a case's enrollment periods are of the same type.
    """
    case = case_service.get_if_allowed(case_id)
    if not case_service.can_current_user_edit_case(case):
        abort(401)
        return

    periods = request.json
    errors = case_service.validate_enrollment_periods(case, periods)
    if not errors:
        return case_service.update_enrollment_periods(case, periods)

    raise TAAFormError(errors)


# Enrollment Reports
@route(bp, '/<case_id>/enrollment_report', methods=['GET'])
@login_required
@groups_required(api_groups, all=False)
def enrollment_report(case_id):
    case = case_service.get_if_allowed(case_id)

    # TODO: might need to restrict data to certain users who have access to case.
    #  not too important since no sensitive data exposed here.

    return enrollment_application_service.get_enrollment_report(case)


@route(bp, '/<case_id>/enrollment_records', methods=['GET'])
@login_required
@groups_required(api_groups, all=False)
def enrollment_records(case_id):
    """
    Combines the census and enrollment records for export.

    format=json|csv (json by default)
    """
    case = case_service.get_if_allowed(case_id)

    if request.args.get('draw'):
        # Do a datatables response with pagination and search text.
        offset = int(request.args.get('start', 0))
        limit = int(request.args.get('length', None))
        search_text = request.args.get('search[value]', "")
        order_col_num = int(request.args.get('order[0][column]', 1))
        order_dir = request.args.get('order[0][dir]', 'asc')

        col_name_pattern = re.compile('columns\[(\d+)\]\[name\]')
        column_names = {}
        for argname in request.args:
            match = col_name_pattern.match(argname)
            if match:
                col_num = int(match.groups()[0])
                name = request.args[argname]
                column_names[col_num] = name

        order_col_name = column_names.get(order_col_num)

        data = enrollment_application_service.retrieve_enrollment_data_for_table(
            case,
            offset=offset,
            limit=limit,
            search_text=search_text,
            order_column=order_col_name,
            order_dir=order_dir,
        )

        table_data = [dict(
            id=row.id,
            date=row.date,
            case_id=row.case_id,
            census_record_id=row.census_record_id,
            employee_first=row.employee_first,
            employee_last=row.employee_last,
            # employee_email=row.employee_email,
            agent_name=row.agent_name,
            employee_birthdate=row.employee_birthdate,
            enrollment_status=row.enrollment_status,
            total_premium=row.total_premium,
        ) for row in data]

        resp_data = dict(
            data=table_data,
            # DataTables docs recommends casting this to int to prevent XSS
            draw=int(request.args['draw']),
            recordsTotal=enrollment_application_service.retrieve_enrollments_total_visible_count_for_table(case),
            recordsFiltered=enrollment_application_service.retrieve_enrollments_filtered_count_for_table(case,
                                                                                                         search_text),
        )

        return Response(response=json.dumps(resp_data, cls=JSONEncoder), content_type='application/json')

    census_records = case_service.get_current_user_census_records(case)
    data = enrollment_application_service.get_enrollment_records_for_census_records(census_records)

    if request.args.get('format') == 'csv':
        body = enrollment_application_service.export_enrollment_data(data)
        date_str = datetime.now().strftime('%Y-%m-%d')
        headers = {
            'Content-Type': 'text/csv',
            'Content-Disposition': 'attachment; filename=enrollment_export_{0}.csv'.format(date_str)
        }
        return make_response(body, 200, headers)
    return data


@route(bp, '/<case_id>/enrollment_records/<int:census_id>', methods=['GET'])
@login_required
@groups_required(api_groups, all=False)
def enrollment_record(case_id, census_id):
    """
    Combines the census and enrollment records for export, but for a single census record.

    format=json|csv (json by default)
    """
    data = enrollment_application_service.get_enrollment_records_for_census(
        case_service.get_if_allowed(case_id), census_id)
    if request.args.get('format') == 'csv':
        body = enrollment_application_service.export_enrollment_data(data)
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
@login_required
@groups_required(api_groups, all=False)
def census_records(case_id):
    case = case_service.get_if_allowed(case_id)

    # Extract search parameters
    args = {
        'filter_ssn': request.args.get('filter_ssn'),
        'filter_birthdate': request.args.get('filter_birthdate'),
    }
    # Restrict access if needed and if not checking for SSN duplicates
    if case_service.is_current_user_restricted_to_own_enrollments(case) and not args['filter_ssn']:
        args['filter_agent'] = agent_service.get_logged_in_agent()

    # If we are requesting CSV format, get the whole data set.
    if request.args.get('format') == 'csv':
        data = case_service.get_census_records(case, **args)
        body = case_service.export_census_records(data)
        date_str = datetime.now().strftime('%Y-%m-%d')
        headers = {
            'Content-Type': 'text/csv',
            'Content-Disposition':
                'attachment; filename=case_export_{0}.csv'.format(date_str)
        }
        return make_response(body, 200, headers)

    # If we are doing an SSN lookup, use simpler method.
    if args['filter_ssn'] or args['filter_birthdate']:
        return case_service.get_census_records(case, **args)

    # DataTables parameters
    offset = int(request.args.get('start', 0))
    limit = int(request.args.get('length', None))
    search_text = request.args.get('search[value]', "")
    order_col_num = int(request.args.get('order[0][column]', 3))
    order_dir = request.args.get('order[0][dir]', 'asc')

    col_name_pattern = re.compile('columns\[(\d+)\]\[name\]')
    column_names = {}
    for argname in request.args:
        match = col_name_pattern.match(argname)
        if match:
            col_num = int(match.groups()[0])
            name = request.args[argname]
            column_names[col_num] = name

    order_col_name = column_names.get(order_col_num)

    # Search for the matching census rows
    if case_service.is_current_user_restricted_to_own_enrollments(case):
        limit_to_agent = agent_service.get_logged_in_agent().id
    else:
        limit_to_agent = None

    data = case_service.retrieve_census_data_for_table(
        case,
        offset=offset,
        limit=limit,
        search_text=search_text,
        order_column=order_col_name,
        order_dir=order_dir,
        agent_id=limit_to_agent,
    )

    table_data = [dict(
        id=row.id,
        case_id=row.case_id,
        employee_first=row.employee_first,
        employee_last=row.employee_last,
        employee_email=row.employee_email,
        employee_birthdate=row.employee_birthdate,
        enrollment_status=row.enrollment_status,
    ) for row in data]

    resp_data = dict(
        data=table_data,
        # DataTables docs recommends casting this to int to prevent XSS
        draw=int(request.args['draw']),
        recordsTotal=case_service.retrieve_census_total_visible_count_for_table(case, limit_to_agent),
        recordsFiltered=case_service.retrieve_census_filtered_count_for_table(case, limit_to_agent, search_text),
    )

    return Response(response=json.dumps(resp_data, cls=JSONEncoder), content_type='application/json')


# Census Records - lookup self-enroll link debug API for Bill to get SSNs with self-enroll links.
@route(bp, '/<case_id>/census_records/links', methods=['GET'])
@login_required
@groups_required(['admins'], all=False)
def census_record_links(case_id):
    case = case_service.get_if_allowed(case_id)
    census_records = case_service.get_census_records(case, include_enrollment_links=True)

    # Custom serialization
    census_record_service = LookupService('CensusRecordService')
    return census_record_service.serialize_with_tokens(case, census_records, request.url_root)


@route(bp, '/<case_id>/census_records', methods=['POST'])
@login_required
@groups_required(api_groups, all=False)
def post_census_records(case_id):
    case = case_service.get_if_allowed(case_id)

    data = get_posted_data()
    file_obj = request.files.get('csv-file')
    if not file_obj:
        # TODO: create the ad-hoc record after an enrollment, not before.
        # Attempt to process an ad-hoc post. Currently only SSN is required, and anyone who can
        #  view / enroll the case can do this
        return case_service.create_ad_hoc_census_record(case, ssn=data['ssn'])

    # Case upload - must be able to edit case settings.
    # Note: This is after the case above where an ad-hoc record is created since that is one
    #  situation currently that unprivileged users can create records.
    #
    if not case_service.can_current_user_edit_case(case):
        abort(401)
        return

    if not (file_obj and has_csv_extension(file_obj.filename)):
        return dict(
            errors=[dict(
                message='Invalid file format. Filename must end with .csv, '
                        'and follow the specification exactly. See sample '
                        'upload file.',
                records=[]
            )]
        )

    # Process the uploaded file
    errors, records = case_service.process_uploaded_census_data(case, data['upload_type'], file_obj)

    # If no errors, we retrieve the data in bulk so the serializer has everything it needs
    #   (otherwise it will query per record)
    if not errors:
        records = case_service.get_census_records(case)

    # Return at most 20 errors at a time, otherwise returns all added or changed records
    return dict(errors=errors[:20], records=records), (400 if errors else 200)


def has_csv_extension(filename):
    return '.' in filename and filename.lower().rsplit('.', 1)[1] == 'csv'


@route(bp, '/<case_id>/census_records/<census_record_id>', methods=['PUT'])
@login_required
@groups_required(api_groups, all=False)
def update_census_record(case_id, census_record_id):
    case = case_service.get_if_allowed(case_id)
    if not case_service.can_current_user_edit_case(case):
        abort(401)
        return
    census_record = case_service.get_record_if_allowed(census_record_id)
    form = CensusRecordForm(obj=census_record)
    if form.validate_on_submit():
        return case_service.update_census_record(census_record, form.data)
    raise TAAFormError(form.errors)


@route(bp, '/<case_id>/census_records/<census_record_id>', methods=['DELETE'])
@login_required
@groups_required(api_groups, all=False)
def delete_census_record(case_id, census_record_id):
    case = case_service.get_if_allowed(case_id)
    if not case_service.can_current_user_edit_case(case):
        abort(401)
        return
    census_record = case_service.get_record_if_allowed(census_record_id)
    case_service.delete_census_record(census_record)
    return None, 204


@route(bp, '/<case_id>/self_enrollment_setup', methods=['PUT'])
@login_required
@groups_required(api_groups, all=False)
def update_self_enrollment_setup(case_id):
    case = case_service.get_if_allowed(case_id)
    if not case_service.can_current_user_edit_case(case):
        abort(401)
        return
    self_enrollment_setup = case_service.get_self_enrollment_setup(case)
    form = SelfEnrollmentSetupForm(obj=self_enrollment_setup, case=case)

    if ('self_enrollment_type' in request.json
        and request.json['self_enrollment_type'] == SelfEnrollmentSetup.TYPE_CASE_GENERIC):
        # Remove email-specific fields from the form so they are not validated
        del form.email_greeting_type
        del form.email_greeting_salutation
        del form.email_subject
        del form.email_sender_email
        del form.email_sender_name
        del form.email_message

    if form.validate_on_submit():
        # Update enrolling agent
        data = get_posted_data()
        case_service.update_enrolling_agent(case, data['enrolling_agent_id'])

        # Update self enrollment setup
        return case_service.update_self_enrollment_setup(self_enrollment_setup, form.data)

    raise TAAFormError(form.errors)


@route(bp, '/<case_id>/agent_splits_setup', methods=['PUT'])
@login_required
@groups_required(api_groups, all=False)
def update_agent_split_setup(case_id):
    case = case_service.get_if_allowed(case_id)

    case_service.delete_agent_splits_setup_for_case(case)

    if not case_service.can_current_user_edit_case(case):
        abort(401)
        return

    for split in request.json:
        case_service.create_agent_splits_setup(case, split)

    return True


def get_census_records_for_status(case, status=None):
    result = []

    if case is None or case.self_enrollment_setup is None:
        return result
    if status is None:
        return result if case.census_records is None else case.census_records

    case_census_records = case.census_records
    if case_service.requires_occupation(case):
        case_census_records = [cr for cr in case_census_records if cr.occupation_class is not None]

    for record in case_census_records:
        if status == 'not-sent':
            if len(self_enrollment_email_service.get_for_census_record(record)) == 0:
                result.append(record)
        else:
            enroll = enrollment_application_service.get_enrollment_status(record)
            if status == 'not-enrolled' and enroll != EnrollmentApplication.APPLICATION_STATUS_ENROLLED and enroll != EnrollmentApplication.APPLICATION_STATUS_DECLINED:
                result.append(record)
            elif status == 'declined' and enroll == EnrollmentApplication.APPLICATION_STATUS_DECLINED:
                result.append(record)
    return result


@route(bp, '/<case_id>/self_enroll_email_batches', methods=['GET'])
@login_required
@groups_required(api_groups, all=False)
def email_self_enrollment_batch_get_all(case_id):
    case = case_service.get_if_allowed(case_id)
    return self_enrollment_email_service.get_batches_for_case(case)


@route(bp, '/<case_id>/self_enroll_email_batches/<batch_id>', methods=['GET'])
@login_required
@groups_required(api_groups, all=False)
def email_self_enrollment_batch_get(case_id, batch_id):
    case = case_service.get_if_allowed(case_id)
    return self_enrollment_email_service.get_batch_for_case(case, batch_id)


@route(bp, '/<case_id>/self_enroll_email_batches', methods=['POST'])
@login_required
@groups_required(api_groups, all=False)
def email_self_enrollment_batch_post(case_id):
    send_type = request.args["send_type"]
    case = case_service.get_if_allowed(case_id)
    setup = case.self_enrollment_setup
    if setup is None or setup.self_enrollment_type != 'case-targeted':
        abort(400, "Case not configured for targeted self-enrollment")
    agent = agent_service.get_logged_in_agent()
    if not agent:
        # Use the owning agent
        agent = case.owner_agent

    eligible_census = get_census_records_for_status(case, status=send_type)

    results = self_enrollment_email_service.create_batch_for_case(case, eligible_census, request.url_root)

    # Insert the pending emails together for speed.
    db.session.commit()

    # Queue up the results to celery
    for email_log in results:
        self_enrollment_email_service.queue_email(email_log.id)

    return ["Scheduled %s emails for immediate processing." % len(results)]
