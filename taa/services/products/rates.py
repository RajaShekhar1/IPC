import csv
import itertools
import os
from collections import OrderedDict
from io import BytesIO

from payment_modes import MODES_BY_NAME
from taa.core import TAAFormError

__all__ = ['get_rates']

DATA_DIR = 'taa/services/products/data_files'
TYPE_COVERAGE = 'coverage'
TYPE_PREMIUM = 'premium'


def get_rates(product, **demographics):
    product_code = product.get_base_product_code()
    # Check employee eligibility
    limit_errors = []
    if not is_eligible(product_code, demographics['employee_gender'],
                       demographics['employee_height'],
                       demographics['employee_weight']):
        limit_errors += [
            dict(field='employee_height',
                 error='This height/weight combination is outside the range '
                       'for this product.'),
            dict(field='employee_weight',
                 error='This height/weight combination is outside the range '
                       'for this product.')
        ]
    # Check spouse eligibility if required
    if demographics.get('spouse_age') is not None and not is_eligible(
            product_code, demographics['spouse_gender'],
            demographics['spouse_height'], demographics['spouse_weight']):
        limit_errors += [
            dict(field='spouse_height',
                 error='This height/weight combination is outside the range '
                       'for this product.'),
            dict(field='spouse_weight',
                 error='This height/weight combination is outside the range '
                       'for this product.'),
            ]
    # If any height/weight errors, raise an API exception
    if limit_errors:
        raise TAAFormError(errors=limit_errors)
    return {
        'employee': rates.get(product_code, demographics.get('payment_mode'),
                              demographics['employee_age'],
                              demographics.get('employee_smoker')),
        'spouse': rates.get(product_code, demographics.get('payment_mode'),
                            demographics.get('spouse_age'),
                            demographics.get('spouse_smoker')),
        'children': rates.get(product_code, demographics.get('payment_mode'),
                              age=None)
    }


def is_eligible(product_code, sex, height, weight):
    if product_code in ELIGIBILITIES:
        table = ELIGIBILITIES[product_code][sex]
        if height is None or weight is None:
            return False
        height = int(height)
        if height not in table:
            return False
        weight = int(weight)
        return table[height][0] <= weight <= table[height][1]
    # Default to eligible if product has no lookup table
    return True


def build_eligibility(csv_path):
    table = {}
    for line in csv.DictReader(open(csv_path, 'rU')):
        table[int(line['Inches'])] = (int(line['Min Weight']),
                                      int(line['Max Weight']))
    return table


def clean_number_string(s):
    return s.strip().replace(',', '').replace('$', '')


def intify(s, none_on_fail=True):
    try:
        return int(clean_number_string(s))
    except ValueError:
        if none_on_fail:
            return None
        else:
            return s


def floatify(s, digits=2, none_on_fail=True):
    try:
        return round(float(clean_number_string(s)), digits)
    except ValueError:
        if none_on_fail:
            return None
        else:
            return s


class Rates(object):
    def __init__(self):
        self._rates = {}

    def _init_dict(self, product_code, payment_mode, type_):
        if product_code not in self._rates:
            self._rates[product_code] = {}
        if payment_mode not in self._rates[product_code]:
            self._rates[product_code][payment_mode] = {}
        if type_ not in self._rates[product_code][payment_mode]:
            self._rates[product_code][payment_mode][type_] = OrderedDict()

    def from_csv(self, path, product_code, payment_mode, type_, smoker=None):
        self._process_data(open(path, 'rU'), product_code, payment_mode, type_,
                           smoker)

    def from_string(self, s, product_code, payment_mode, type_, smoker=None):
        self._process_data(BytesIO(s), product_code, payment_mode, type_,
                           smoker)

    @staticmethod
    def _get_product_key(product_code, smoker=None):
        if product_code == 'Group CI':
            # Only Group CI has separate non-/smoker rate tables
            return product_code, smoker
        else:
            return product_code

    def _process_data(self, data, product_code, payment_mode, type_, smoker):
        reader = csv.reader(data)
        header = map(floatify, reader.next())
        product_key = Rates._get_product_key(product_code, smoker)
        self._init_dict(product_key, payment_mode, type_)
        for line in reader:
            for index, key in enumerate(
                    itertools.product([floatify(line[0])], header[1:]), start=1):
                
                self._rates[product_key][payment_mode][type_][key] = {
                    TYPE_PREMIUM:
                        floatify(line[index]) if type_ == TYPE_COVERAGE
                        else key[1],
                    TYPE_COVERAGE:
                        floatify(line[index]) if type_ == TYPE_PREMIUM else key[1]
                }

    def get(self, product_code, payment_mode, age, smoker=None):
        result = {}
        if age is None:
            # Children rates/premiums are indexed with age as -1
            age = -1
        product_key = Rates._get_product_key(product_code, smoker)

        for type_ in (TYPE_PREMIUM, TYPE_COVERAGE):
            if type_ not in result:
                result[type_] = []
            if (product_key in self._rates
                    and payment_mode in self._rates[product_key]
                    and type_ in self._rates[product_key][payment_mode]):
                for key, value in iter(
                        self._rates[product_key][payment_mode][type_].items()):
                    if (key[0] == age
                            and value[TYPE_PREMIUM] is not None
                            and value[TYPE_COVERAGE] is not None):
                        result[type_].append(value)
        # Rename keys to fit existing API
        result['byface'] = result.pop(TYPE_COVERAGE)
        result['bypremium'] = result.pop(TYPE_PREMIUM)
        return result


# If a product is not in this dict, there are no limits on eligibility
ELIGIBILITIES = {
    'Group CI': {
        'female': build_eligibility(
            os.path.join(DATA_DIR, 'CIEMP-Female-Height-Weight-ranges.csv')),
        'male': build_eligibility(
            os.path.join(DATA_DIR, 'CIEMP-Male-Height-Weight-ranges.csv'))
    }
}


# Build rate table for adults
rates = Rates()
rates.from_csv(os.path.join(DATA_DIR, 'CIEMP-rates---NonSmoking-Weekly.csv'),
               'Group CI', MODES_BY_NAME['weekly'], TYPE_COVERAGE, smoker=False)
rates.from_csv(os.path.join(DATA_DIR, 'CIEMP-rates---NonSmoking-Biweekly.csv'),
               'Group CI', MODES_BY_NAME['biweekly'], TYPE_COVERAGE, smoker=False)
rates.from_csv(os.path.join(DATA_DIR, 'CIEMP-rates---NonSmoking-Semimonthly.csv'),
               'Group CI', MODES_BY_NAME['semimonthly'], TYPE_COVERAGE, smoker=False)
rates.from_csv(os.path.join(DATA_DIR, 'CIEMP-rates---NonSmoking-Monthly.csv'),
               'Group CI', MODES_BY_NAME['monthly'], TYPE_COVERAGE, smoker=False)

rates.from_csv(os.path.join(DATA_DIR, 'CIEMP-rates---Smoking-Weekly.csv'),
               'Group CI', MODES_BY_NAME['weekly'], TYPE_COVERAGE, smoker=True)
rates.from_csv(os.path.join(DATA_DIR, 'CIEMP-rates---Smoking-Biweekly.csv'),
               'Group CI', MODES_BY_NAME['biweekly'], TYPE_COVERAGE, smoker=True)
rates.from_csv(os.path.join(DATA_DIR, 'CIEMP-rates---Smoking-Semimonthly.csv'),
               'Group CI', MODES_BY_NAME['semimonthly'], TYPE_COVERAGE, smoker=True)
rates.from_csv(os.path.join(DATA_DIR, 'CIEMP-rates---Smoking-Monthly.csv'),
               'Group CI', MODES_BY_NAME['monthly'], TYPE_COVERAGE, smoker=True)

rates.from_csv(os.path.join(DATA_DIR, 'FPPCI-byface.csv'),
               'FPPCI', MODES_BY_NAME['weekly'], TYPE_COVERAGE)
rates.from_csv(os.path.join(DATA_DIR, 'FPPCI-byface-biweekly.csv'),
               'FPPCI', MODES_BY_NAME['biweekly'], TYPE_COVERAGE)
rates.from_csv(os.path.join(DATA_DIR, 'FPPCI-byface-semimonthly.csv'),
               'FPPCI', MODES_BY_NAME['semimonthly'], TYPE_COVERAGE)
rates.from_csv(os.path.join(DATA_DIR, 'FPPCI-byface-monthly.csv'),
               'FPPCI', MODES_BY_NAME['monthly'], TYPE_COVERAGE)

rates.from_csv(os.path.join(DATA_DIR, 'FPPCI-bypremium.csv'),
               'FPPCI', MODES_BY_NAME['weekly'], TYPE_PREMIUM)
rates.from_csv(os.path.join(DATA_DIR, 'FPPCI-bypremium-biweekly.csv'),
               'FPPCI', MODES_BY_NAME['biweekly'], TYPE_PREMIUM)
rates.from_csv(os.path.join(DATA_DIR, 'FPPCI-bypremium-semimonthly.csv'),
               'FPPCI', MODES_BY_NAME['semimonthly'], TYPE_PREMIUM)
rates.from_csv(os.path.join(DATA_DIR, 'FPPCI-bypremium-monthly.csv'),
               'FPPCI', MODES_BY_NAME['monthly'], TYPE_PREMIUM)

rates.from_csv(os.path.join(DATA_DIR, 'FPPGOV-byface.csv'),
               'FPP-Gov', MODES_BY_NAME['weekly'], TYPE_COVERAGE)
rates.from_csv(os.path.join(DATA_DIR, 'FPPGOV-byface-biweekly.csv'),
               'FPP-Gov', MODES_BY_NAME['biweekly'], TYPE_COVERAGE)
rates.from_csv(os.path.join(DATA_DIR, 'FPPGOV-byface-semimonthly.csv'),
               'FPP-Gov', MODES_BY_NAME['semimonthly'], TYPE_COVERAGE)
rates.from_csv(os.path.join(DATA_DIR, 'FPPGOV-byface-monthly.csv'),
               'FPP-Gov', MODES_BY_NAME['monthly'], TYPE_COVERAGE)

rates.from_csv(os.path.join(DATA_DIR, 'FPPTI-byface.csv'),
               'FPPTI', MODES_BY_NAME['weekly'], TYPE_COVERAGE)
rates.from_csv(os.path.join(DATA_DIR, 'FPPTI-byface-biweekly.csv'),
               'FPPTI', MODES_BY_NAME['biweekly'], TYPE_COVERAGE)
rates.from_csv(os.path.join(DATA_DIR, 'FPPTI-byface-semimonthly.csv'),
               'FPPTI', MODES_BY_NAME['semimonthly'], TYPE_COVERAGE)
rates.from_csv(os.path.join(DATA_DIR, 'FPPTI-byface-monthly.csv'),
               'FPPTI', MODES_BY_NAME['monthly'], TYPE_COVERAGE)

rates.from_csv(os.path.join(DATA_DIR, 'FPPTI-bypremium.csv'),
               'FPPTI', MODES_BY_NAME['weekly'], TYPE_PREMIUM)
rates.from_csv(os.path.join(DATA_DIR, 'FPPTI-bypremium-biweekly.csv'),
               'FPPTI', MODES_BY_NAME['biweekly'], TYPE_PREMIUM)
rates.from_csv(os.path.join(DATA_DIR, 'FPPTI-bypremium-semimonthly.csv'),
               'FPPTI', MODES_BY_NAME['semimonthly'], TYPE_PREMIUM)
rates.from_csv(os.path.join(DATA_DIR, 'FPPTI-bypremium-monthly.csv'),
               'FPPTI', MODES_BY_NAME['monthly'], TYPE_PREMIUM)

# Build rate table for children (currently hardcoded)
# Group CI
rates.from_string("age,10000\n-1,0.75", 'Group CI',
                  MODES_BY_NAME['weekly'], TYPE_COVERAGE)
rates.from_string("age,10000\n-1,1.50", 'Group CI',
                  MODES_BY_NAME['biweekly'], TYPE_COVERAGE)
rates.from_string("age,10000\n-1,1.63", 'Group CI',
                  MODES_BY_NAME['semimonthly'], TYPE_COVERAGE)
rates.from_string("age,10000\n-1,3.25", 'Group CI',
                  MODES_BY_NAME['monthly'], TYPE_COVERAGE)
# FPPCI
rates.from_string("age,10000,20000\n-1,1.15,2.30", 'FPPCI',
                  MODES_BY_NAME['weekly'], TYPE_COVERAGE)
rates.from_string("age,10000,20000\n-1,2.30,4.60", 'FPPCI',
                  MODES_BY_NAME['biweekly'], TYPE_COVERAGE)
rates.from_string("age,10000,20000\n-1,2.49,4.98", 'FPPCI',
                  MODES_BY_NAME['semimonthly'], TYPE_COVERAGE)
rates.from_string("age,10000,20000\n-1,4.98,9.97", 'FPPCI',
                  MODES_BY_NAME['monthly'], TYPE_COVERAGE)
# FPP-Gov
rates.from_string("age,10000,20000\n-1,1.15,2.30", 'FPP-Gov',
                  MODES_BY_NAME['weekly'], TYPE_COVERAGE)
rates.from_string("age,10000,20000\n-1,2.30,4.60", 'FPP-Gov',
                  MODES_BY_NAME['biweekly'], TYPE_COVERAGE)
rates.from_string("age,10000,20000\n-1,2.49,4.98", 'FPP-Gov',
                  MODES_BY_NAME['semimonthly'], TYPE_COVERAGE)
rates.from_string("age,10000,20000\n-1,4.98,9.97", 'FPP-Gov',
                  MODES_BY_NAME['monthly'], TYPE_COVERAGE)
# FPPTI
rates.from_string("age,10000,20000\n-1,1.15,2.30", 'FPPTI',
                  MODES_BY_NAME['weekly'], TYPE_COVERAGE)
rates.from_string("age,10000,20000\n-1,2.30,4.60", 'FPPTI',
                  MODES_BY_NAME['biweekly'], TYPE_COVERAGE)
rates.from_string("age,10000,20000\n-1,2.49,4.98", 'FPPTI',
                  MODES_BY_NAME['semimonthly'], TYPE_COVERAGE)
rates.from_string("age,10000,20000\n-1,4.98,9.97", 'FPPTI',
                  MODES_BY_NAME['monthly'], TYPE_COVERAGE)