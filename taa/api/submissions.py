from flask import Blueprint, request, make_response, abort
from taa.api import route
from flask_stormpath import groups_required, login_required
import datetime
from taa.services import LookupService
from taa.services.enrollments.csv_export import export_hi_acc_enrollments

blueprint = Blueprint('submissions', __name__, url_prefix='/submissions')
api_groups = ['admins']


@route(blueprint, '/csv', methods=['GET'])
@login_required
@groups_required(api_groups, all=False)
def get_submissions():
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
