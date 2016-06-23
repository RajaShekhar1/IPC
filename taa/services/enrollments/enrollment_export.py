from flask import abort
from taa.services.cases.case_service import CaseService

from taa.services.enrollments.enrollment_application import EnrollmentApplicationService

from taa import db, tasks
from taa.core import DBService
from taa.services.data_export import BackgroundExport


class EnrollmentExportService(DBService):

    __model__ = BackgroundExport

    def export_user_case_enrollments(self, user_href, case_id, format):
        """
        Export the enrollments this user is allowed to see, for the given case and format.

        Runs in the background, so this returns only the export_id as a reference.
        """

        # We don't need to store every export ever performed. For now, just store a single export per case/user combo.
        export = db.session.query(BackgroundExport
            ).filter_by(case_id=case_id, user_href=user_href, status=BackgroundExport.STATUS_COMPLETE
            ).first()

        export = BackgroundExport(
            params=dict(
                format=format,
            ),
            case_id=case_id,
            user_href=user_href,
            status=BackgroundExport.STATUS_PENDING,

        )
        db.session.add(export)
        db.session.commit()

        # Queue up the task
        tasks.export_user_case_enrollments.delay(export.id)

        return export

    def is_export_finished(self, export_id, current_user_href):
        """
        Checks to see if an export has finished.
        """
        export = self.get_or_404(export_id)
        if export.user_href != current_user_href:
            abort(403)

        return export.status == BackgroundExport.STATUS_COMPLETE

    def get_export_file(self, export_id, current_user_href):
        """
        If an export has finished, retrieve the file for download.
        """
        export = self.get_or_404(export_id)
        if export.user_href != current_user_href:
            abort(403)

        if export.download_type == BackgroundExport.DOWNLOAD_TYPE_BINARY:
            return export.binary_data
        else:
            return export.unicode_data


    def process_export(self, export_id):
        export = self.get(export_id)

        # Mark as processing
        export.status = BackgroundExport.STATUS_PROCESSING
        db.session.commit()

        # Do the export
        case_service = CaseService()
        case = case_service.get(export.case_id)

        enrollment_application_service = EnrollmentApplicationService()
        census_records = case_service.get_current_user_census_records(case)
        data = enrollment_application_service.get_enrollment_records_for_census_records(census_records)
        export_data = enrollment_application_service.export_enrollment_data(data)

        # Save the results
        export.unicode_data = export_data
        export.download_type = BackgroundExport.DOWNLOAD_TYPE_UNICODE
        export.status = BackgroundExport.STATUS_COMPLETE
        db.session.commit()