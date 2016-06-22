

from taa import tasks
from taa.services.data_export import BackgroundExport

class EnrollmentExportService(object):

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
                user_href=user_href,
                case_id=case_id,
                format=format,
            ),
            status=EnrollmentExport.STATUS_PENDING,

        )
        db.session.add(export)
        db.session.commit()

        # Queue up the task
        tasks.export_user_case_enrollments.delay(export)

        return export.id


    def check_export_status(self, export_id):
        """
        Checks to see if an export has finished.
        """


    def get_export_file(self, export_id):
        """
        If an export has finished, retrieve the file for download.
        """



