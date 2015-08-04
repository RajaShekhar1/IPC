from StringIO import StringIO

from flask import Blueprint, request

from taa.api import route
from taa.services import LookupService


bp = Blueprint('enrollments', __name__, url_prefix='/enrollments')
case_service = LookupService("CaseService")
enrollment_import_service = LookupService("EnrollmentImportService")


@route(bp, '/', methods=["POST"])
def submit_data():
    case_token = request.args.get('case_token') or request.form.get('case_token')
    auth_token = request.args.get('auth_token') or request.form.get('auth_token')
    email_errors = bool(request.args.get('email_errors')) or bool(request.form.get('email_errors'))
    data_format = request.args.get('format') or request.form.get('format', 'flat')
    if request.data:
        data = StringIO(request.data)
    elif request.files['api-upload-file']:
        data = request.files['api-upload-file']

    import_results = enrollment_import_service.process_enrollment_data(
        data,
        data_format,
        case_token=case_token,
        auth_token=auth_token,
        email_errors=email_errors
    )

    return {
        'num_processed': import_results.get_num_processed(),
        'num_errors': len(import_results.get_errors()),
        'errors': [error.to_json() for error in import_results.get_errors()]
    }, 400 if import_results.is_error() else 200
