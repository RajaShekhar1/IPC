# -*- coding: utf-8 -*-

MODE_WEEKLY = 52
MODE_BIWEEKLY = 26
MODE_SEMIMONTHLY = 24
MODE_MONTHLY = 12
MODE_CHANGEABLE = -1

MODES_BY_MODE = {
    MODE_WEEKLY: 'weekly',
    MODE_BIWEEKLY: 'biweekly',
    MODE_SEMIMONTHLY: 'semimonthly',
    MODE_MONTHLY: 'monthly',
}

MODES_BY_NAME = dict(zip(MODES_BY_MODE.values(), MODES_BY_MODE.keys()))

# When `adhoc` is false, the option is not permitted during ad-hoc enrollment
payment_modes = map(lambda x: dict(mode=x[0], name=x[1], immutable=x[2]), [
    (MODE_WEEKLY, 'Weekly', True),
    (MODE_BIWEEKLY, 'Biweekly', True),
    (MODE_SEMIMONTHLY, 'Semimonthly', True),
    (MODE_MONTHLY, 'Monthly', True),
    (MODE_CHANGEABLE, 'Leave For Applicant To Select', False),
    ])


def get_payment_modes(changeable=False, single=None):
    result = filter(lambda x: x['immutable'] or x['immutable'] == changeable,
                    payment_modes)
    if single is None:
        return result
    else:
        return filter(lambda x: x['mode'] == single, result)


def is_payment_mode_changeable(payment_mode):
    return payment_mode == MODE_CHANGEABLE
