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
    should_email_status = bool(request.args.get('should_email_status')) or bool(request.form.get('should_email_status'))
    data_format = request.args.get('format') or request.form.get('format', 'flat')
    upload_source = request.args.get('upload_source') or request.form.get('upload_source', 'api')
    if request.data:
        data = StringIO(request.data)
    elif request.files['api-upload-file']:
        data = request.files['api-upload-file']

    import_results = enrollment_import_service.process_enrollment_data(
        data,
        data_format,
        case_token=case_token,
        auth_token=auth_token,
        should_email_status=should_email_status,
        data_source=upload_source
    )

    return {
        'num_processed': import_results.get_num_processed(),
        'num_errors': len(import_results.get_errors()),
        'errors': [error.to_json() for error in import_results.get_errors()]
    }, 400 if import_results.is_error() else 200
