from StringIO import StringIO

from flask import Blueprint, request, abort, make_response
from flask_stormpath import login_required, groups_required

from taa.api import route
from taa.services import LookupService


bp = Blueprint('enrollments', __name__, url_prefix='/enrollments')
case_service = LookupService("CaseService")
enrollment_import_service = LookupService("EnrollmentImportService")
enrollment_import_batch_service = LookupService("EnrollmentImportBatchService")
enrollment_import_batch_item_service = LookupService("EnrollmentImportBatchItemService")
enrollment_submission_service = LookupService("EnrollmentSubmissionService")
enrollment_application_service = LookupService("EnrollmentApplicationService")

@route(bp, '/', methods=["POST"])
def submit_data():    
    case_token = request.args.get('case_token') or request.form.get('case_token')
    auth_token = request.args.get('auth_token') or request.form.get('auth_token')
    user_href = request.args.get('user_href') or request.form.get('user_href')
    data_format = request.args.get('format') or request.form.get('format', 'flat')
    upload_source = request.args.get('upload_source') or request.form.get('upload_source', 'api')
    if request.data:
        data = StringIO(request.data)
    elif request.files['api-upload-file']:
        data = request.files['api-upload-file']
    else:
        raise ValueError("No data provided")

    import_results = enrollment_import_service.process_enrollment_data(
        data,
        data_format,
        case_token=case_token,
        auth_token=auth_token,
        user_href=user_href,
        data_source=upload_source
    )

    return {
        'num_processed': import_results.get_num_processed(),
        'num_errors': len(import_results.get_errors()),
        'errors': [error.to_json() for error in import_results.get_errors()][:20]
    }, 400 if import_results.is_error() else 200


@route(bp, '/import_batches', methods=['GET'])
@login_required
@groups_required(['admins'])
def get_batches():
    return enrollment_import_batch_service.all()


@route(bp, '/import_batches/<batch_id>', methods=['GET'])
@login_required
@groups_required(['admins'])
def get_batch_items(batch_id):
    batch = enrollment_import_batch_service.get(batch_id)
    if not batch:
        abort(404)

    return [item for item in enrollment_import_batch_service.get_batch_items(batch)]


# For convenience, allow lookup of enrollment records without case id for admin only
#  (if this is opened up to other users, add case permission checking)
@route(bp, '/records/<int:enrollment_record_id>')
@login_required
@groups_required(['admins'])
def get_individual_enrollment_record(enrollment_record_id):
    return enrollment_application_service.get_or_404(enrollment_record_id)



@route(bp, '/import_batches/<batch_id>/<item_id>/pdf', methods=['GET'])
@login_required
@groups_required(['admins'])
def render_batch_item_pdf(batch_id, item_id):
    item = enrollment_import_batch_item_service.get(item_id)
    if not item:
        abort(404)

    binary_pdf = enrollment_submission_service.render_enrollment_pdf(item.enrollment_record)

    response = make_response(binary_pdf)
    response.headers['Content-Type'] = 'application/pdf'
    response.headers['Content-Disposition'] = 'inline; filename=%s.pdf' % 'enrollment_{}'.format(item.enrollment_record_id)
    return response
