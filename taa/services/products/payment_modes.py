# -*- coding: utf-8 -*-

MODE_WEEKLY = 52
MODE_BIWEEKLY = 26
MODE_SEMIMONTHLY = 24
MODE_MONTHLY = 12
MODE_CHANGEABLE = -1

MODES_BY_MODE = {
    MODE_WEEKLY: u'weekly',
    MODE_BIWEEKLY: u'biweekly',
    MODE_SEMIMONTHLY: u'semimonthly',
    MODE_MONTHLY: u'monthly',
}

MODES_BY_NAME = dict(zip(MODES_BY_MODE.values(), MODES_BY_MODE.keys()))

# When `adhoc` is false, the option is not permitted during ad-hoc enrollment
payment_modes = map(lambda x: dict(mode=x[0], name=x[1], immutable=x[2]), [
    (MODE_WEEKLY, u'Weekly', True),
    (MODE_BIWEEKLY, u'Biweekly', True),
    (MODE_SEMIMONTHLY, u'Semimonthly', True),
    (MODE_MONTHLY, u'Monthly', True),
    (MODE_CHANGEABLE, u'Leave For Applicant To Select', False),
    ])


def get_full_payment_modes():
    """
    Used for the rewritten multi-product wizard, return all payment modes with more detail.
    """
    return [
      {'frequency': 52, 'label': u'Weekly'},
      {'frequency': 26, 'label': u'Biweekly'},
      {'frequency': 24, 'label': u'Semimonthly'},
      {'frequency': 12, 'label': u'Monthly'}
    ]

def get_payment_modes(changeable=False, single=None):
    result = filter(lambda x: x['immutable'] or x['immutable'] == changeable,
                    payment_modes)
    if single is None:
        return result
    else:
        return filter(lambda x: x['mode'] == single, result)

def is_payment_mode(name=None, payments_per_year=None):
    result = payment_modes
    if name:
        result = filter(lambda mode: mode['name'].lower() == name.lower(), payment_modes)
    if payments_per_year:
        result = filter(lambda mode: mode['mode'] == payments_per_year, payment_modes)
    return bool(result)


def is_payment_mode_changeable(payment_mode):
    return payment_mode == MODE_CHANGEABLE
