# -*- coding: utf-8 -*-

MODE_WEEKLY = 52
MODE_BIWEEKLY = 26
MODE_SEMIMONTHLY = 24
MODE_MONTHLY = 12
MODE_CHANGEABLE = -1

# When `adhoc` is false, the option is not permitted during ad-hoc enrollment
payment_modes = map(lambda x: dict(mode=x[0], name=x[1], immutable=x[2]), [
    (MODE_WEEKLY, 'Weekly', True),
    (MODE_BIWEEKLY, 'Bi-Weekly', True),
    (MODE_SEMIMONTHLY, 'Semi-Monthly', True),
    (MODE_MONTHLY, 'Monthly', True),
    (MODE_CHANGEABLE, 'Leave For Applicant To Select', False),
    ])


def get_payment_modes(changeable=False):
    return filter(lambda x: x['immutable'] or x['immutable'] == changeable,
                  payment_modes)


def is_payment_mode_changeable(payment_mode):
    return payment_mode == MODE_CHANGEABLE
