from collections import defaultdict
from csv import writer as csv_writer
from base64 import standard_b64encode

import io
from flask_script import Command, Option

from taa.services import LookupService
from taa.services.mailer import send_email
from taa.services.cases.models import *
from taa.services.enrollments.models import *
from taa.tasks import schedule_case_report

__all__ = ['RunCaseReportCommand']


def generate_case_report(email_address):
    
    name_tags = ['ISD', 'ETX', 'WTX']
    name_filter = db.or_(*[Case.company_name.contains(n) for n in name_tags])
    
    cases = db.session.query(Case
                             ).filter(name_filter
                                      # ).options(db.joinedload('enrollment_applications').subqueryload('coverages')
                                      ).all()
    
    header_row = ['Case ID', 'Case Name', 'Case Token', 'Case URL', '# Apps', '# Coverages', '# STP Submissions']
    
    data = []
    for i, case in enumerate(cases):
        print("Getting data for case #{} {}/{}".format(case.id, i + 1, len(cases)))
        # apps = case.enrollment_applications
        apps = db.session.query(EnrollmentApplication).filter_by(case_id=case.id).all()
        #num_coverages = db.session.query(EnrollmentApplicationCoverage).filter(
        #    EnrollmentApplicationCoverage.enrollment.has(case_id=case.id)).count()
        
        # For coverages, we need to parse the enrollment data and get people who enrolled
        num_apps = len(apps)
        
        num_coverages = 0
        
        for app in apps:
            enrollment_service = LookupService('EnrollmentApplicationService')
            for data_wrap in enrollment_service.get_wrapped_enrollment_data(app):
                if data_wrap.did_employee_select_coverage():
                    num_coverages += 1
                    
                if data_wrap.did_spouse_select_coverage():
                    num_coverages += 1
                
                num_coverages += data_wrap.get_num_covered_children()
        
        num_submissions = db.session.query(EnrollmentSubmission).filter(
            EnrollmentSubmission.enrollment_applications.any(EnrollmentApplication.case_id == case.id)).count()
        
        data.append([
            case.id,
            case.company_name,
            case.case_token,
            "https://5starenroll.com/enrollment-case/{}".format(case.id),
            num_apps,
            num_coverages,
            num_submissions,
        ])
    
    stream = io.BytesIO()
    writer = csv_writer(stream)
    
    writer.writerow(header_row)
    for row in data:
        writer.writerow(row)
    email_data = standard_b64encode(stream.getvalue())
    
    print(stream.getvalue())
    
    send_email([email_address], "5Star Case Report {}".format(datetime.now().strftime('%Y-%m-%d')),
               text="See attached case report.", attachments=[dict(
            data=email_data,
            type='text/csv',
            name='report.csv'
        )])


class RunCaseReportCommand(Command):
    """Runs a custom case report and emails the contents to an email address."""
    option_list = (
        Option('--email', '-e',
               dest='email_address',
               help="Email to send to",
               required=False,
               default='zmason@delmarsd.com'),
        
    )
    
    def run(self, email_address):
    
        # TODO: make celery scheduling a command line switch
        schedule_case_report.delay(email_address)