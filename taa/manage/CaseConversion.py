from flask_script import Command, Option
from taa import db
from taa.models import (Case, CaseEnrollmentPeriod)


class CaseConversionCommand(Command):

    option_list = (
        Option('--only', '-o', dest='task', required=False),
    )

    def run(self, task):
        convert_cases()


def convert_cases():
    s = db.session
    annual_case_ids = []
    cases = s.query(Case).all()

    for case in cases:
        type = case.enrollment_period_type
        case_enrollment_periods = s.query(CaseEnrollmentPeriod).filter(CaseEnrollmentPeriod.case_id == case.id).all()
        for cep in case_enrollment_periods:
            period_type = cep.period_type
            if (period_type == "open_with_start"):
                if (cep.start_date and cep.end_date):
                    cep.period_type = "open_with_start"
                elif (cep.start_date and not cep.end_date):
                    cep.end_date = cep.start_date

                    new_cep = CaseEnrollmentPeriod()
                    new_cep.case_id = cep.case_id
                    new_cep.period_type = "ongoing"
                    #s.add(new_cep)

            if (period_type == "annual"):
                annual_case_ids.append(cep.case_id)
                s.delete(cep)

    for id in annual_case_ids:
        new_cep = CaseEnrollmentPeriod()
        new_cep.case_id = id
        new_cep.period_type = "ongoing"
        # s.add(new_cep)





