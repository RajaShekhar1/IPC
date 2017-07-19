import datetime

from flask import Blueprint, request, make_response
from flask_stormpath import groups_required, login_required

from taa.api import route
from taa.api.api_helpers import parse_dates_from_request, parse_dates_as_str_from_request
from taa.config_defaults import PAYLOGIX_PGP_KEY_ID
from taa.services import LookupService
from taa.services.enrollments.csv_export import export_hi_acc_enrollments
from taa.services.enrollments.paylogix import create_paylogix_csv

enrollment_submission_service = LookupService("EnrollmentSubmissionService")

blueprint = Blueprint('submissions', __name__, url_prefix='/submissions')


@route(blueprint, '/', methods=['GET'])
@login_required
@groups_required(['admins'], all=False)
def get_submissions():
    MAX_RESULTS = 500

    end_date, start_date = parse_dates_as_str_from_request()
    submission_type = request.args.get('submission_type')
    submission_status = request.args.get('submission_status')

    all_submissions_query = enrollment_submission_service.search_submissions(start_date=start_date, end_date=end_date, submission_type=submission_type,
                                                            submission_status=submission_status)

    total_submissions = all_submissions_query.count()
    if total_submissions > MAX_RESULTS:
        return {'clipped_length': total_submissions, 'submissions': all_submissions_query.limit(MAX_RESULTS).all()}
    else:
        return {'submissions': all_submissions_query.all()}

@route(blueprint, '/<submission_id>', methods=['GET'])
@login_required
@groups_required(['admins'], all=False)
def get_submission(submission_id):

    return enrollment_submission_service.get_submission(submission_id)


@route(blueprint, '/<submission_id>/applications', methods=['GET'])
@login_required
@groups_required(['admins'], all=False)
def get_submission_apps(submission_id):
    return enrollment_submission_service.get_submission_applications(submission_id)


@route(blueprint, '/<submission_id>/logs', methods=['GET'])
@login_required
@groups_required(['admins'], all=False)
def get_submission_logs(submission_id):
    return enrollment_submission_service.get_submission_logs(submission_id)



@route(blueprint, '/<submission_id>/data', methods=['GET'])
@login_required
@groups_required(['admins'], all=False)
def get_submission_data(submission_id):
    data = enrollment_submission_service.get_submission_data(submission_id)
    if not data:
        return "No data"
    response = make_response(data)
    response.headers['Content-Type'] = 'application/octet-stream'
    response.headers['Content-Disposition'] = 'attachment; filename="submission_data_{}.txt"'.format(submission_id)
    return response




@route(blueprint, '/hi_acc_csv', methods=['GET'])
@login_required
@groups_required(['admins'], all=False)
def get_hi_acc_submissions():
    end_date, start_date = parse_dates_from_request()

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
@groups_required(['admins'], all=False)
def paylogix_export():
    """
    Output a CSV containing all of the Paylogix export information for a given date range
    """

    end_date, start_date = parse_dates_from_request()

    csv_data = create_paylogix_csv(start_date, end_date)

    headers = {
        'Content-Type': 'text/csv',
        'Content-Disposition': 'attachment; filename=paylogix_export_{0}.csv'.format(datetime.datetime.now().strftime('%Y-%m-%d'))
    }

    # Optional encryption for the download for testing purposes
    if 'encrypt' in request.args and bool(request.args.get('encrypt', False)):
        ftp_service = LookupService('FtpService')
        csv_data = ftp_service.encrypt(csv_data, PAYLOGIX_PGP_KEY_ID)
    return make_response(unicode(csv_data), 200, headers)


