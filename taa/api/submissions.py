from flask import Blueprint, request, make_response, abort
from taa.api import route
from flask_stormpath import groups_required, login_required
import datetime
from taa.services import LookupService
from taa.services.enrollments.csv_export import export_hi_acc_enrollments
from taa.services.enrollments.paylogix import create_paylogix_csv

blueprint = Blueprint('submissions', __name__, url_prefix='/submissions')
api_groups = ['admins']


@route(blueprint, '/hi_acc_csv', methods=['GET'])
@login_required
@groups_required(api_groups, all=False)
def get_hi_acc_submissions():
    # noinspection PyBroadException
    try:
        start_date = datetime.datetime.strptime(request.args.get('start_date'),
                                                '%Y-%m-%d') if 'start_date' in request.args else None
        end_date = datetime.datetime.strptime(request.args.get('end_date'),
                                              '%Y-%m-%d') if 'end_date' in request.args else None
    except Exception:
        abort(400, 'Valid start and end dates are required. Dates must be in the form of YYYY-MM-DD.')
        return

    application_service = LookupService('EnrollmentApplicationService')
    """:type: taa.services.enrollments.enrollment_application.EnrollmentApplicationService"""

    applications = application_service.get_applications_by_submission_date(start_date, end_date)

    csv_data = export_hi_acc_enrollments(applications)
    headers = {
        'Content-Type': 'text/csv',
        'Content-Disposition': 'attachment; filename=submission_export_%s.csv' % datetime.date.today().strftime(
            '%Y-%m-%d')
    }

    return make_response(csv_data, 200, headers)


@route(blueprint, '/paylogix_export', methods=['GET'])
@login_required
@groups_required(api_groups, all=False)
def paylogix_export():
    """
    Output a CSV containing all of the Paylogix export information for a given date range

    :param case_id: Id of the Case
    :type case_id: int
    """
    enrollment_application_service = LookupService('EnrollmentApplicationService')
    """:type: taa.services.enrollments.EnrollmentApplicationService"""

    # noinspection PyBroadException
    try:
        start_date = datetime.datetime.strptime(request.args.get('start_date'),
                                                '%Y-%m-%d') if 'start_date' in request.args else None
        end_date = datetime.datetime.strptime(request.args.get('end_date'),
                                              '%Y-%m-%d') if 'end_date' in request.args else None
    except Exception:
        abort(400, 'Valid start and end dates are required. Dates must be in the form of YYYY-MM-DD.')
        return

    if start_date and end_date:
        applications = enrollment_application_service.get_paylogix_applications_between_dates(start_date, end_date)
    else:
        applications = enrollment_application_service.get_paylogix_applications()

    csv_data = create_paylogix_csv(applications)

    date_str = datetime.datetime.now().strftime('%Y-%m-%d')
    headers = {
        'Content-Type': 'text/csv',
        'Content-Disposition': 'attachment; filename=paylogix_export_{0}.csv'.format(date_str)
    }
    return make_response(csv_data, 200, headers)
