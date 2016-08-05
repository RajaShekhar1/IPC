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
        case_enrollment_periods = s.query(CaseEnrollmentPeriod).filter(CaseEnrollmentPeriod.case_id == case.id).all()
        for cep in case_enrollment_periods:
            period_type = cep.period_type
            if (period_type == "open_with_start"):
                if (cep.start_date and cep.end_date):
                    cep.period_type = "open_with_start"
                    case.enrollment_period_type = "open_with_start"
                    if (not case.open_enrollment_type or not case.ongoing_enrollment_type):
                        if (case.requires_paylogix_export):
                            case.open_enrollment_type = "first_friday"
                        else:
                            case.open_enrollment_type = "day_of_month"

                elif (cep.start_date and not cep.end_date):
                    cep.end_date = cep.start_date
                    case.enrollment_period_type = "both"

                    new_cep = CaseEnrollmentPeriod()
                    new_cep.case_id = int(cep.case_id)
                    new_cep.period_type = "ongoing"
                    #s.add(new_cep)

                    if (not case.open_enrollment_type or not case.ongoing_enrollment_type):
                        if (case.requires_paylogix_export):
                            case.open_enrollment_type = "first_friday"
                            case.ongoing_enrollment_type = "first_friday"
                        else:
                            case.open_enrollment_type = "day_of_month"
                            case.ongoing_enrollment_type = "day_of_month"

            if (period_type == "annual_period"):
                case.enrollment_period_type = "ongoing"

                if (not case.open_enrollment_type or not case.ongoing_enrollment_type):
                    if (case.requires_paylogix_export):
                        case.ongoing_enrollment_type = "first_friday"
                    else:
                        case.ongoing_enrollment_type = "day_of_month"

                if (cep.case_id not in annual_case_ids):
                    annual_case_ids.append(cep.case_id)

                s.delete(cep)

    for id in annual_case_ids:
        new_cep = CaseEnrollmentPeriod()
        new_cep.case_id = int(id)
        new_cep.period_type = "ongoing"
        # s.add(new_cep)





