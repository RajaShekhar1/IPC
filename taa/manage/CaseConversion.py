from flask_script import Command, Option
from taa import db
from taa.models import (Case, CaseEnrollmentPeriod, CaseOpenEnrollmentPeriod, CaseOngoingEnrollmentPeriod,
                        CaseBothEnrollmentPeriod, CaseAnnualEnrollmentPeriod)


class CaseConversionCommand(Command):
    option_list = (
        Option('--only', '-o', dest='task', required=False),
    )

    def run(self, task):
        convert_cases()


def convert_cases():
    """
    converts cases to be compatible with effective date capabilities
    """

    s = db.session
    cases = s.query(Case).all()
    case_list = []

    for case in cases:
        has_open = False
        has_ongoing = False
        annual_periods = []
        start_date = None
        end_date = None
        case_product_ids = []
        is_paylogix = case.requires_paylogix_export
        case_enrollment_periods = s.query(CaseEnrollmentPeriod).filter(CaseEnrollmentPeriod.case_id == case.id).all()

        for cep in case_enrollment_periods:
            if cep.period_type == CaseOpenEnrollmentPeriod.PERIOD_TYPE and not cep.start_date and not cep.end_date:
                continue
            elif cep.period_type == CaseOngoingEnrollmentPeriod.PERIOD_TYPE:
                continue
            elif cep.period_type == CaseOpenEnrollmentPeriod.PERIOD_TYPE and cep.start_date and not cep.end_date:
                has_open = True
                has_ongoing = True
                cep.end_date = start_date = end_date = cep.start_date
                ongoing_cep = create_ongoing_cep(case.id)
                s.add(ongoing_cep)
            elif cep.period_type == CaseAnnualEnrollmentPeriod.PERIOD_TYPE:
                has_ongoing = True
                annual_periods.append(cep)
            elif (cep.period_type == "open" or cep.period_type == CaseOpenEnrollmentPeriod.PERIOD_TYPE) and cep.start_date and cep.end_date:
                has_open = True
                start_date = cep.start_date
                end_date = cep.end_date
                cep.period_type = CaseOpenEnrollmentPeriod.PERIOD_TYPE

        if len(annual_periods) > 0:
            ongoing_cep = create_ongoing_cep(case.id)
            s.add(ongoing_cep)
            for annual_period in annual_periods:
                s.delete(annual_period)

        if has_open and not case.open_enrollment_type:
            case.open_enrollment_type = case.DAY_OF_MONTH_ENROLLMENT_TYPE if not is_paylogix else case.FIRST_FRIDAY_ENROLLMENT_TYPE

        if has_ongoing and not case.ongoing_enrollment_type:
            case.ongoing_enrollment_type = case.DAY_OF_MONTH_ENROLLMENT_TYPE if not is_paylogix else case.FIRST_FRIDAY_ENROLLMENT_TYPE

        if has_ongoing and has_open:
            case.enrollment_period_type = CaseBothEnrollmentPeriod.PERIOD_TYPE

        elif has_ongoing and not has_open:
            case.enrollment_period_type = CaseOngoingEnrollmentPeriod.PERIOD_TYPE

        elif has_open and not has_ongoing:
            case.enrollment_period_type = CaseOpenEnrollmentPeriod.PERIOD_TYPE

        for product in case.products:
            case_product_ids.append(product.id)

        if not case.effective_date_settings and (has_open or has_ongoing):
            case.effective_date_settings = format_effective_date_settings(has_open, has_ongoing, is_paylogix,
                                                                          start_date, end_date)
            # case.product_settings = format_product_settings(case.product_settings, case_product_ids)

        case_list.append(case.id)

    print ("Updating Cases: {}".format(case_list))
    s.commit()


def create_ongoing_cep(case_id):
    new_cep = CaseEnrollmentPeriod(case_id=case_id, period_type="ongoing", start_date=None, end_date=None)
    return new_cep


def format_effective_date_settings(has_open, has_ongoing, is_paylogix, start_date, end_date):
    if has_open and not has_ongoing:
        if is_paylogix:
            effective_date_settings = [
                {"type": "open", "method": Case.FIRST_FRIDAY_ENROLLMENT_TYPE, Case.FIRST_FRIDAY_ENROLLMENT_TYPE: 2,
                 "enrollment_period": {"start_date": start_date.strftime("%x"), "end_date": end_date.strftime("%x")}}]
        else:
            effective_date_settings = [{"type": "open", "method": Case.DAY_OF_MONTH_ENROLLMENT_TYPE,
                                        Case.DAY_OF_MONTH_ENROLLMENT_TYPE: 15}]
        return effective_date_settings
    elif has_ongoing and not has_open:
        if is_paylogix:
            effective_date_settings = [{"type": "ongoing", "method": Case.FIRST_FRIDAY_ENROLLMENT_TYPE,
                                        Case.FIRST_FRIDAY_ENROLLMENT_TYPE: 2}]
        else:
            effective_date_settings = [{"type": "ongoing", "method": Case.DAY_OF_MONTH_ENROLLMENT_TYPE,
                                        Case.DAY_OF_MONTH_ENROLLMENT_TYPE: 15}]
        return effective_date_settings
    elif has_open and has_ongoing:
        if is_paylogix:
            effective_date_settings = [
                {"type": "open", "method": Case.FIRST_FRIDAY_ENROLLMENT_TYPE, Case.FIRST_FRIDAY_ENROLLMENT_TYPE: 2,
                 "enrollment_period": {"start_date": start_date.strftime("%x"), "end_date": end_date.strftime("%x")}},
                {"type": "ongoing", "method": Case.FIRST_FRIDAY_ENROLLMENT_TYPE, Case.FIRST_FRIDAY_ENROLLMENT_TYPE: 2}]
        else:
            effective_date_settings = [
                {"type": "open", "method": Case.DAY_OF_MONTH_ENROLLMENT_TYPE, Case.DAY_OF_MONTH_ENROLLMENT_TYPE: 15,
                 "enrollment_period": {"start_date": start_date.strftime("%x"), "end_date": end_date.strftime("%x")}},
                {"type": "ongoing", "method": Case.DAY_OF_MONTH_ENROLLMENT_TYPE, Case.DAY_OF_MONTH_ENROLLMENT_TYPE: 15}]
        return effective_date_settings
    else:
        return None


def format_product_settings(case_product_settings, product_ids):
    case_product_settings['effective_date_settings'] = []
    for product_id in product_ids:
        case_product_settings['effective_date_settings'].append(
            {"effective_date": "", "effective_date_override": False, "product_id": product_id})
    return case_product_settings
