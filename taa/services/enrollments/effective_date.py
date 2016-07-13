class StaticEffectiveDateRule(object):
    def __init__(self, date):
        self.date = date

    def get_effective_date(self, enrollment_date):
        if enrollment_date > self.date:
            return enrollment_date
        else:
            return self.date


class CutoffEffectiveDateRule(object):
    "Either the 1st of next month, or the 1st of the following month if after the cutoff"
    def __init__(self, cutoff_date_int):
        self.cutoff_date = int(cutoff_date_int)

    def get_effective_date(self, enrollment_date):
        pass


class EnrollerPicksRule(object):
    def __init__(self, enroller_picks_date, default_days, minimum_days):
        self.enroller_picks_date = enroller_picks_date
        self.default_days = default_days
        self.minimum_days = minimum_days

    def get_effective_date(self, enrollment_date):
        pass


class FirstFridayFollowingRule(object):
    "Either the 1st of next month, or the 1st of the following month if after the cutoff"

    def __init__(self, cutoff_date_int):
        self.cutoff_date = int(cutoff_date_int)

    def get_effective_date(self, enrollment_date):
        pass


class EffectiveDateCalculator(object):
    def __init__(self, period_start, period_end, is_ongoing, open_rule, ongoing_rule):
        self.period_start = period_start
        self.period_end = period_end
        self.is_ongoing = is_ongoing
        self.open_rule = open_rule
        self.ongoing_rule = ongoing_rule

    def get_effective_date_for_enroll_date(self, enroll_date):
        if self.is_ongoing and not self.period_start:
            # Just ongoing
            pass
        elif self.period_start and self.period_end and not self.ongoing_rule:
            # Just a date, no ongoing
            pass
        elif self.period_start and self.period_end and self.ongoing_rule:
            # Both
            pass