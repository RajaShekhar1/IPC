from collections import defaultdict
from csv import writer as csv_writer
from base64 import standard_b64encode

import io
from flask_script import Command, Option

from taa.services.mailer import send_email
from taa.services.cases.models import *
from taa.services.enrollments.models import *


__all__ = ['RunCaseReportCommand']

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
    
        name_tags = ['ISD', 'ETX', 'WTX']
        name_filter = db.or_(*[Case.company_name.contains(n) for n in name_tags])
        
        cases = db.session.query(Case
            ).filter(name_filter
            #).options(db.joinedload('enrollment_applications').subqueryload('coverages')
            ).all()
        
        header_row = ['Case ID', 'Case Name', 'Case Token', 'Case URL', '# Apps', '# Coverages', '# STP Submissions']
        
        data = []
        for i, case in enumerate(cases):
            print("Getting data for case #{} {}/{}".format(case.id, i+1, len(cases)))
            #apps = case.enrollment_applications
            num_apps = db.session.query(EnrollmentApplication).filter_by(case_id=case.id).count()
            num_coverages = db.session.query(EnrollmentApplicationCoverage).filter(EnrollmentApplicationCoverage.enrollment.has(case_id=case.id)).count()
            num_submissions = db.session.query(EnrollmentSubmission).filter(EnrollmentSubmission.enrollment_applications.any(EnrollmentApplication.case_id == case.id)).count()
            
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