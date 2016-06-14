import csv
from StringIO import StringIO
from io import BytesIO
from zipfile import ZipFile

from flask import Blueprint, request, abort, make_response, send_file
from flask_stormpath import login_required, groups_required

from taa.api import route
from taa.services import LookupService
from taa.services.enrollments.csv_export import export_hi_acc_enrollments


bp = Blueprint('enrollments', __name__, url_prefix='/enrollments')
case_service = LookupService("CaseService")
enrollment_import_service = LookupService("EnrollmentImportService")
enrollment_import_batch_service = LookupService("EnrollmentImportBatchService")
enrollment_import_batch_item_service = LookupService("EnrollmentImportBatchItemService")
enrollment_submission_service = LookupService("EnrollmentSubmissionService")
enrollment_application_service = LookupService("EnrollmentApplicationService")
product_service = LookupService("ProductService")

@route(bp, '/', methods=["POST"])
def submit_enrollments():
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

@route(bp, '/import_batches/<batch_id>/reprocess', methods=['POST'])
@login_required
@groups_required(['admins'])
def reprocess_batch(batch_id):
    batch = enrollment_import_batch_service.get(batch_id)

    # Enqueue the batch for processing
    enrollment_submission_service.submit_import_enrollments(batch)

@route(bp, '/import_batches/<batch_id>', methods=['DELETE'])
@login_required
@groups_required(['admins'])
def delete_batch(batch_id):
    batch = enrollment_import_batch_service.get(batch_id)

    # Delete all the enrollment records, batch items, and the batch itself.
    enrollment_import_batch_service.delete_batch(batch)



# For convenience, allow lookup of enrollment records without case id for admin only
#  (if this is opened up to other users, add case permission checking)
@route(bp, '/records/<int:enrollment_record_id>', methods=['GET'])
@login_required
@groups_required(['admins', 'home_office'], all=False)
def get_individual_enrollment_record(enrollment_record_id):
    return enrollment_application_service.get_or_404(enrollment_record_id)

# Admin delete enrollment record
@route(bp, '/records/<int:enrollment_record_id>', methods=['DELETE'])
@login_required
@groups_required(['admins', 'home_office'], all=False)
def delete_individual_enrollment_record(enrollment_record_id):
    # Return a 404 if not a valid record
    enrollment_record = enrollment_application_service.get_or_404(enrollment_record_id)

    # Delete the record with associated coverages and batch items
    enrollment_application_service.delete_enrollment_record(enrollment_record)

    # Return proper DELETE response
    return None, 204


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


@route(bp, '/import_batches/<batch_id>/<item_id>/xml', methods=['GET'])
@login_required
@groups_required(['admins'])
def render_batch_item_xml(batch_id, item_id):
    item = enrollment_import_batch_item_service.get(item_id)
    if not item:
        abort(404)

    pdf_bytes = enrollment_submission_service.render_enrollment_pdf(item.enrollment_record)
    zipstream = BytesIO()
    with ZipFile(zipstream, 'w') as zip:
        for form_for in enrollment_submission_service.get_enrollees(item.enrollment_record):
            xml = enrollment_submission_service.render_enrollment_xml(
                item.enrollment_record, form_for, pdf_bytes)
            if xml is not None:
                fn = 'enrollment_{}-{}.xml'.format(item.enrollment_record_id, form_for)
                zip.writestr(fn, xml.encode('latin-1'))
    zipstream.seek(0)
    return send_file(zipstream, attachment_filename='enrollment_{}.zip'.format(
        item.enrollment_record_id), as_attachment=True)


@route(bp, '/records/<int:enrollment_record_id>/pdf', methods=['GET'])
@login_required
@groups_required(['admins', 'home_office', 'agents'], all=False)
def generate_enrollment_pdf(enrollment_record_id):
    enrollment = enrollment_application_service.get_or_404(enrollment_record_id)

    # TODO: Verify agent permission if agent
    # Check that logged-in agent matches enrollment.agent_id or is the case owner. Should be method for this on case.

    binary_pdf = enrollment_submission_service.render_enrollment_pdf(enrollment)

    response = make_response(binary_pdf)
    response.headers['Content-Type'] = 'application/pdf'
    response.headers['Content-Disposition'] = 'inline; filename=%s.pdf' % 'enrollment_{}'.format(enrollment.id)
    return response


@route(bp, '/export/acchi/csv/<from_>/<to_>', methods=['GET'])
@login_required
@groups_required(['admins'])
def render_acc_hi_csv(from_, to_):
    # Get cases for ACC/HI products
    case_ids = []
    for code in ['ACC', 'HI']:
        case_ids.extend([c.id for p in product_service.search(by_code=code)
                         for c in p.cases])
    case_ids = set(case_ids)

    enrollments = enrollment_application_service.get_enrollments_by_date(
            from_, to_).filter(
            enrollment_application_service.__model__.case_id.in_(case_ids)).all()
    data = export_hi_acc_enrollments(enrollments)
    response = make_response(data)
    response.headers['Content-Type'] = 'text/csv'
    response.headers['Content-Disposition'] = 'inline; filename=acc-hi_{}~{}.csv'.format(from_, to_)
    return response

