import datetime

import dateutil.parser
from flask_script import Command, Option

from ..models import db
from ..services.cases.models import Case
from ..services.enrollments.models import EnrollmentApplication


__all__ = ['FlagPaylogixEnrollmentsCommand']


class FlagPaylogixEnrollmentsCommand(Command):
    """Sets "is Paylogix" flag on enrollment applications for cases that
    require Paylogix export, between specified dates"""
    option_list = (
        Option('--start', '-s',
               dest='start_date',
               help="Start date (signature time) for targeted enrollments (yyyy-mm-dd)",
               required=False,
               default='2016-06-15'),
        Option('--end', '-e',
               dest='end_date',
               help="End date (signature time) for targeted enrollments (yyyy-mm-dd)",
               required=False,
               default='2016-08-31'),
        Option('--dry-run', '-d',
               dest='dry_run',
               action='store_true',
               required=False,
               help="Dry-run flag (when set, don't actually change DB)",
               default=False),
    )

    def run(self, start_date, end_date, dry_run):
        start_date = dateutil.parser.parse(start_date)
        end_date = dateutil.parser.parse(end_date) + datetime.timedelta(days=1)
        if dry_run:
            print('=' * 78)
            print("Dry run -- No database data will be changed!")
            print('=' * 78)

        # Get target cases
        cases = [c.id
                 for c in Case.query.filter_by(requires_paylogix_export=True)]

        # Get enrollments attached to target cases between start/end dates
        enrollments = EnrollmentApplication.query. \
            filter(db.and_(EnrollmentApplication.signature_time>=start_date,
                           EnrollmentApplication.signature_time<end_date,
                           EnrollmentApplication.case_id.in_(cases))).all()
        enrollment_ids = set([e.id for e in enrollments])

        print("Changing {:,} enrollments with signature times starting on {} "
              "and prior to {}".format(len(enrollments),
                                     start_date.date().isoformat(),
                                     end_date.date().isoformat()))

        if not dry_run:
            for enrollment in enrollments:
                enrollment.is_paylogix = True
            db.session.commit()
            print("Changes written to database")

            paylogix_ids = set([e.id for e in
                                EnrollmentApplication.query.filter_by(
                                        is_paylogix=True)])
            if enrollment_ids == paylogix_ids:
                print("Verified {:,} records were changed correctly".format(
                        len(paylogix_ids)))
            else:
                print("WARNING: Couldn't verify that the changes were correct")
                print("-" * 78)
                print("Expected to modify these enrollment_applicaiton IDs:")
                print(enrollment_ids)
                print("=" * 78)
                print("Actually modified these enrollment_applicaiton IDs:")
                print("-" * 78)
                print(paylogix_ids)
        else:
            print("This was a dry-run, so nothing was changed in the database")
