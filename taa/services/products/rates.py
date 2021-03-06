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

# These must match (case insensitive) the values of applicant type on GI Criteria objects.
APPLICANT_TYPE_EMPLOYEE = 'employee'
APPLICANT_TYPE_SPOUSE = 'spouse'
APPLICANT_TYPE_CHILDREN = 'child'

# Global used to cache the rates after being loaded from files
rates = None


def get_rates(product, **demographics):
    """
    Public Rates interface, but only used for products we can't calculate yet.
    """

    # Initialize rates if None
    global rates
    if rates is None:
        rates = Rates()
        initialize_rates_from_files(rates)

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

    # We use a different method if the product rates are supposed to be limited by GI settings
    if product.are_rates_limited_to_GI():
        rate_calc = GILimitedRatesDecorator(rates, product)
    else:
        rate_calc = rates

    return {
        'employee': rate_calc.get(product_code, demographics.get('payment_mode'),
                                  demographics['employee_age'],
                                  demographics.get('employee_smoker'),
                                  applicant_type=APPLICANT_TYPE_EMPLOYEE,
                                  height=demographics.get('employee_height'),
                                  weight=demographics.get('employee_weight')),
        'spouse': rate_calc.get(product_code, demographics.get('payment_mode'),
                                demographics.get('spouse_age'),
                                demographics.get('spouse_smoker'),
                                applicant_type=APPLICANT_TYPE_SPOUSE,
                                height=demographics.get('spouse_height'),
                                weight=demographics.get('spouse_weight')),
        'children': rate_calc.get(product_code, demographics.get('payment_mode'), age=None,
                                  applicant_type=APPLICANT_TYPE_CHILDREN, smoker=None)
    }


def is_eligible(product_code, sex, height, weight):
    # Skip eligibility check if any criteria is not provided.
    if sex is None or height is None or weight is None or height == '' or weight == '':
        return True

    height = int(height)
    weight = int(weight)

    initialize_eligibilities_from_files()
    if product_code in ELIGIBILITIES:
        table = ELIGIBILITIES[product_code][sex]

        # Is height out of range?
        if height not in table:
            return False

        # Is weight out of range for this height?
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
    def __init__(self, initial_rates=None):
        self._rates = initial_rates or {}

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
            for index, key in enumerate(itertools.product([floatify(line[0])], header[1:]), start=1):
                self._rates[product_key][payment_mode][type_][key] = {
                    TYPE_PREMIUM: floatify(line[index]) if type_ == TYPE_COVERAGE else key[1],
                    TYPE_COVERAGE: floatify(line[index]) if type_ == TYPE_PREMIUM else key[1],
                    "payment_mode": payment_mode,
                }

    def get(self, product_code, payment_mode, age, smoker=None, applicant_type=None, height=None, weight=None):
        result = {}
        if applicant_type == APPLICANT_TYPE_CHILDREN:
            # Children rates/premiums are indexed with age as -1
            age = -1
            smoker = None
        elif age is None:
            # Don't return any rate options for this applicant (Emp/Sp) if age is not provided.
            return {'byface': [], 'bypremium': []}

        product_key = Rates._get_product_key(product_code, smoker)

        for type_ in (TYPE_PREMIUM, TYPE_COVERAGE):
            if type_ not in result:
                result[type_] = []
            if product_key in self._rates and payment_mode in self._rates[product_key] and type_ in \
                    self._rates[product_key][payment_mode]:
                for key, value in iter(self._rates[product_key][payment_mode][type_].items()):
                    if key[0] == age and value[TYPE_PREMIUM] is not None and value[TYPE_COVERAGE] is not None:
                        result[type_].append(value)
        # Rename keys to fit existing API
        result['byface'] = result.pop(TYPE_COVERAGE)
        result['bypremium'] = result.pop(TYPE_PREMIUM)

        return result


class GILimitedRatesDecorator(Rates):
    def __init__(self, rates, product):
        self._wrapped_rates = rates
        self.product = product

    def get(self, product_code, payment_mode, age, smoker=None, applicant_type=None, height=None, weight=None):
        """
        Don't allow any rates that are above GI levels, and
        don't use any rates if no criteria defined for a given applicant.
        """

        rates = self._wrapped_rates.get(
            product_code,
            payment_mode,
            age,
            smoker=smoker,
            applicant_type=applicant_type,
            height=height,
            weight=weight,
        )

        # Get the GI limit for the criteria
        limit = self.get_GI_limit(applicant_type, age, smoker, height, weight)

        def filter_coverage_by_limit(rate):
            return rate['coverage'] <= limit

        if not limit:
            # We don't allow enrollment for this applicant type
            return {'bypremium': [], 'byface': []}
        else:
            rates['bypremium'] = filter(filter_coverage_by_limit, rates['bypremium'])
            rates['byface'] = filter(filter_coverage_by_limit, rates['byface'])
            return rates

    def get_GI_limit(self, applicant_type, age, smoker, height, weight):
        return self.get_gi_limit_for_product(self.product, applicant_type, age, smoker, height, weight)

    @classmethod
    def get_gi_limit_for_product(cls, product, applicant_type, age, smoker, height, weight):
        
        if applicant_type.lower() == 'children':
            # Match what is recording on the criteria rows in the database.
            applicant_type = 'child'
        
        criteria_for_applicant = filter(lambda c: c.applicant_type.lower() == applicant_type,
                                        product.gi_criteria)

        def filter_criteria_min_max(property_min, property_max, value):
            def _f(criteria):
                if not getattr(criteria, property_min) or not getattr(criteria, property_max):
                    # If not specified, we can keep this GI criteria
                    return True
                else:
                    return value >= getattr(criteria, property_min) and value <= getattr(criteria, property_max)

            return _f

        filter_age = filter_criteria_min_max('age_min', 'age_max', age)
        filter_height = filter_criteria_min_max('height_min', 'height_max', height)
        filter_weight = filter_criteria_min_max('weight_min', 'weight_max', weight)

        for func in [filter_age, filter_height, filter_weight]:
            criteria_for_applicant = filter(func, criteria_for_applicant)

        if not criteria_for_applicant:
            return None

        # Use the highest number that is allowed for this applicant
        return max(criteria_for_applicant, key=lambda c: c.guarantee_issue_amount).guarantee_issue_amount


def get_height_weight_table_for_product(product):
    global ELIGIBILITIES
    if ELIGIBILITIES is None:
        initialize_eligibilities_from_files()
    base_product_code = product.get_base_product_code()
    if base_product_code in ELIGIBILITIES:
        tables = dict()
        for applicant_sex in ['female', 'male']:
            eligibility = ELIGIBILITIES[base_product_code][applicant_sex]
            tables[applicant_sex] = [{'height': e, 'min_weight': eligibility[e][0], 'max_weight': eligibility[e][1]} for
                                     e in eligibility]
        return tables

    return None


# If a product is not in this dict, there are no limits on eligibility
ELIGIBILITIES = None


def initialize_eligibilities_from_files():
    global ELIGIBILITIES
    if ELIGIBILITIES is None:
        ELIGIBILITIES = {
            'Group CI': {
                'female': build_eligibility(
                    os.path.join(DATA_DIR, 'CIEMP-Female-Height-Weight-ranges.csv')),
                'male': build_eligibility(
                    os.path.join(DATA_DIR, 'CIEMP-Male-Height-Weight-ranges.csv'))
            }
        }


def initialize_rates_from_files(rates):
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

    # Build rate table for children (currently hardcoded)
    # Group CI
    rates.from_string("age,2500,5000,7500,10000\n-1,0.19,0.37,0.56,0.75", 'Group CI',
                      MODES_BY_NAME['weekly'], TYPE_COVERAGE)
    rates.from_string("age,2500,5000,7500,10000\n-1,0.37,0.75,1.12,1.50", 'Group CI',
                      MODES_BY_NAME['biweekly'], TYPE_COVERAGE)
    rates.from_string("age,2500,5000,7500,10000\n-1,0.41,0.81,1.22,1.62", 'Group CI',
                      MODES_BY_NAME['semimonthly'], TYPE_COVERAGE)
    rates.from_string("age,2500,5000,7500,10000\n-1,0.81,1.62,2.44,3.25", 'Group CI',
                      MODES_BY_NAME['monthly'], TYPE_COVERAGE)


#
#
#
#
# def get_rates_for_applicant(applicant_type, product, product_options, state, demographics, mode):
#
#     rate_query = ApplicantRateQuery(applicant_type, product, product_options, state, demographics, mode)
#
#     if not product.rate_package.is_eligible(rate_query):
#         raise ValueError("Eligibility requirements not met.")
#
#     if not product.rate_package.has_valid_options(rate_query):
#         raise ValueError("Invalid product options")
#
#     rates = product.rate_package.lookup_rates(rate_query)
#


if __name__ == "__main__":
    pass
