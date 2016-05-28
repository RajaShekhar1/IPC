
import dateutil.parser
from datetime import datetime, date as datetime_date

from taa.core import DBService
from models import (Case, CaseEnrollmentPeriod,
                    CaseOpenEnrollmentPeriod, CaseOngoingEnrollmentPeriod,
                    )


class CaseEnrollmentPeriodsService(DBService):
    __model__ = CaseEnrollmentPeriod

    def validate_for_case(self, case, data):
        errors = {}
        # TODO: Remove
        # if case.enrollment_period_type == Case.OPEN_ENROLLMENT_TYPE:
        #     return self.validate_open_enrollment_period(case, data)
        # else:
        #     return self.validate_ongoing_enrollment_period(case, data)
        return errors

    def validate_open_enrollment_period(self, case, data):
        errors = {}

        return errors

    def validate_ongoing_enrollment_period(self, case, data):
        errors = []
        # for period in data:
        #    if not period.get('start_date'):
        #        errors.append(dict(error='Invalid date'))
        return errors

    def add_for_case(self, case, period_data):
        periods = []
        for period in period_data:
            # TODO: Remove
            # if period['period_type'] == CaseAnnualEnrollmentPeriod.PERIOD_TYPE:
            #     start = self.valid_annual_date(period['start_date'])
            #     end = self.valid_annual_date(period['end_date'])
            #     periods.append(CaseAnnualEnrollmentPeriod(start_date=start,
            #                                               end_date=end,
            #                                               case_id=case.id))
            if period['period_type'] == CaseOngoingEnrollmentPeriod.PERIOD_TYPE:
                periods.append(CaseOngoingEnrollmentPeriod(case_id=case.id))
            elif period['period_type'] == CaseOpenEnrollmentPeriod.PERIOD_TYPE:
                start = self.valid_date(period['start_date'])
                end = self.valid_date(period['end_date'])
                periods.append(CaseOpenEnrollmentPeriod(start_date=start,
                                                        end_date=end,
                                                        case_id=case.id))
        for p in periods:
            self.save(p)
        return periods

    # TODO: Remove
    # def valid_annual_date(self, d):
    #     if not d:
    #         return None
    #     date = dateutil.parser.parse('{}/{}'.format(d, datetime.now().year))
    #     # strip time
    #     return datetime_date(date.year, date.month, date.day)

    def valid_date(self, d):
        if not d:
            return None
        date = dateutil.parser.parse(d)
        # Strip time
        return datetime_date(date.year, date.month, date.day)

    def remove_all_for_case(self, case):
        self.query().filter(CaseEnrollmentPeriod.case == case).delete()
