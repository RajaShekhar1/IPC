import os
from decimal import Decimal, ROUND_HALF_UP

from yaml import load as yaml_load

try:
    from yaml import CLoader as YamlLoader
except ImportError:
    from yaml import Loader as YamlLoader

from taa.services.products.rates import DATA_DIR

'''

From excel, calculate premium from coverage amount (FACE) given different
 Annual Cost Per Thousand (ACPT) components of the cost.
IF(
    ACPT="", "",
     ROUND(
        (
                                 ROUND(ACPT * FACE/1000, 2)
            + IF(WOP_ACPT <> "", ROUND(WOP_ACPT * FACE/1000, 2), 0)
            + IF(QOL_ACPT <> "", ROUND(QOL_ACPT * FACE/1000, 2), 0)
            + IF(POL_FEE <> "", POL_FEE, 0)
        ) / mode, 2
     )
)

From excel, calculate coverage provided given a premium.

=IF(
    # Ignore these two lines, excel error checking...
    OR(ACPT="", PREMIUM=""),"",
    IF(
      ROUND(((PREMIUM*mode-IF(FIXED_FEE<>"",FIXED_FEE,0))*1000)/ACPT,0)<0,
      "Check Premium",
      # Main computation
      ROUND(
        (
            (PREMIUM * mode - IF(FIXED_FEE<>"", FIXED_FEE,0)) * 1000
        )
        /
        (ACPT + IF(WOP_ACPT<>"", WOP_ACPT, 0) + IF(QOL_ACPT<>"", QOL_ACPT, 0)),
      # Rounding to nearest dollar amount of coverage
      0)
    )
 )
'''

APPLICANT_EMPLOYEE = u'employee'
APPLICANT_SPOUSE = u'spouse'
APPLICANT_CHILD = u'child'

MODE_WEEKLY = 52
MODE_BIWEEKLY = 26
MODE_SEMIMONTHLY = 24
MODE_MONTHLY = 12


class ApplicantQuery(object):
    """
    Used for querying the system for us and
        checking to see if an applicant can apply for a given product configuration (riders, coverage, etc).
    """

    def __init__(self, applicant_type, product_options, state, demographics, mode, rate_options):
        self.applicant_type = applicant_type
        self.product_options = product_options
        self.state = state
        self.demographics = demographics
        self.mode = mode
        self.rate_options = rate_options

    def get_applicant_type(self):
        return self.applicant_type

    def get_age(self):
        return self.demographics.get_age()

    def get_mode(self):
        return self.mode

    def get_riders(self):
        return self.product_options.get('riders', [])

    def get_face_value(self):
        return self.rate_options.get_requested_coverage()

    def get_selected_premium(self):
        return self.rate_options.get_requested_premium()

    def get_coverage_selection_tier(self):
        return self.rate_options.get_requested_coverage_tier()

    def get_rate_level(self):
        return self.rate_options.get_requested_rate_level()


class ApplicantQueryOptions(object):
    def __init__(self, options):
        self.options = options

    def get_requested_coverage(self):
        return self.options.get('by_coverage')

    def get_requested_premium(self):
        return self.options.get('by_premium')

    def get_requested_rate_level(self):
        return self.options.get('rate_level')

    def get_requested_coverage_tier(self):
        return self.options.get('coverage_tier')


class CoverageOption(object):
    def __init__(self, is_by_face, amount):
        self._is_by_face = is_by_face
        self.amount = amount

    def is_by_face(self):
        return self._is_by_face

    def is_by_premium(self):
        return not self._is_by_face

    def get_face_amount(self):
        return self.amount

    def get_premium(self):
        return self.amount


class ApplicantQueryConstraint(object):
    def is_satisfied(self, applicant_query):
        raise NotImplemented()


class AndConstraint(ApplicantQueryConstraint):
    "Compound condition that is satisfied if all sub-conditions are satisfied."

    def __init__(self, subconstraints):
        self.subconstraints = subconstraints

    def is_satisfied(self, applicant_query):
        return all(cond.is_satisfied(applicant_query) for cond in self.subconstraints)


class OrConstraint(ApplicantQueryConstraint):
    "Compound condition that is satisfied if any sub-condition is satisfied."

    def __init__(self, subconstraints):
        self.subconstraints = subconstraints

    def is_satisfied(self, applicant_query):
        return any(cond.is_satisfied(applicant_query) for cond in self.subconstraints)


class NotConstraint(ApplicantQueryConstraint):
    "Invert the logic of a constraint condition."

    def __init__(self, subconstraint):
        self.subconstraint = subconstraint

    def is_satisfied(self, applicant_query):
        return not (self.subconstraint.is_satisfied(applicant_query))


class ProductRiderIncludedConstraint(ApplicantQueryConstraint):
    def __init__(self, rider_code):
        self.rider_code = rider_code

    def is_satisfied(self, applicant_query):
        query_riders = applicant_query.get_riders()
        return self.rider_code in query_riders


class ApplicantTypeMatchesConstraint(ApplicantQueryConstraint):
    def __init__(self, applicant_type):
        assert applicant_type.lower() in [APPLICANT_EMPLOYEE.lower(), APPLICANT_SPOUSE.lower(), APPLICANT_CHILD.lower()]
        self.applicant_type = applicant_type

    def is_satisfied(self, applicant_query):
        return applicant_query.get_applicant_type().lower() == self.applicant_type.lower()


COVERAGE_SELECTION_EE = u'EE'
COVERAGE_SELECTION_ES = u'ES'
COVERAGE_SELECTION_EC = u'EC'
COVERAGE_SELECTION_EF = u'EF'


class CoverageSelectionConstraint(ApplicantQueryConstraint):
    def __init__(self, selection_value):
        assert selection_value in [COVERAGE_SELECTION_EE, COVERAGE_SELECTION_ES, COVERAGE_SELECTION_EC,
                                   COVERAGE_SELECTION_EF]
        self.selection_value = selection_value

    def is_satisfied(self, applicant_query):
        return applicant_query.get_coverage_selection_tier() == self.selection_value


class ApplicantDemographics(object):
    def __init__(self, demographics_object):
        self.age = demographics_object.get('age', None)

    def get_age(self):
        return int(self.age) if self.age is not None else None


class AgeRateLookupTable(object):
    def __init__(self, mapping):
        self._mapping = mapping

    def do_lookup(self, rate_query):
        age = rate_query.get_age()
        return self._mapping.get(int(age))


class RateLevelRateLookupTable(object):
    '''
    Uses the applicant's rating level to lookup premium
    '''

    def __init__(self, mapping):
        self._mapping = mapping

    def do_lookup(self, rate_query):
        rate_level = rate_query.get_rate_level()
        return self._mapping.get(int(rate_level))


class CostComponent(object):
    def __init__(self, is_enabled_constraint=None):
        self.is_enabled_constraint = is_enabled_constraint

    def compute_annual_premium(self, rate_query):
        raise NotImplemented

    def compute_coverage_for_premium(self, rate_query):
        raise NotImplemented

    def get_cost_per_thousand(self, rate_query):
        raise NotImplemented

    def does_vary_with_coverage_selection(self):
        # In general the lookup tables will vary with coverage selection (ACPT)
        raise NotImplemented

    def is_enabled(self, applicant_query):
        if not self.is_enabled_constraint:
            return True

        return self.is_enabled_constraint.is_satisfied(applicant_query)


class LookupTableCostComponent(CostComponent):
    def __init__(self, lookup_table, does_vary_with_coverage=True, is_enabled_constraint=None):
        super(LookupTableCostComponent, self).__init__(is_enabled_constraint)
        self.lookup_table = lookup_table
        self.does_vary_with_coverage = does_vary_with_coverage

    def get_cost_per_thousand(self, rate_query):
        return self.lookup_table.do_lookup(rate_query)

    def compute_annual_premium(self, rate_query):
        acpt = self.lookup_table.do_lookup(rate_query)
        face_value = rate_query.get_face_value()
        if not acpt:
            return Decimal('0.00')
        else:
            return self.rounded_cost_per_thousand(acpt, face_value)

    def rounded_cost_per_thousand(self, cost_per_thousand, face_value):
        return (cost_per_thousand * face_value / Decimal('1000')).quantize(Decimal('0.00'), rounding=ROUND_HALF_UP)

    def does_vary_with_coverage_selection(self):
        # In general the lookup tables will vary with coverage selection (ACPT)
        return self.does_vary_with_coverage


class AnnualPremiumLookupTableCostComponent(LookupTableCostComponent):
    """
    Similar to an ACPT lookup table, but is a fixed annual premium based on a category of coverage selected instead of face amount.
    """

    def get_cost_per_thousand(self, rate_query):
        # Doesn't support this method.
        return None

    def compute_annual_premium(self, rate_query):
        return self.lookup_table.do_lookup(rate_query)


class FlatFeeCostComponent(CostComponent):
    def __init__(self, fixed_fee, is_enabled_constraint=None):
        super(FlatFeeCostComponent, self).__init__(is_enabled_constraint=is_enabled_constraint)
        self.fixed_fee = fixed_fee

    def compute_annual_premium(self, rate_query):
        return self.fixed_fee

    def get_cost_per_thousand(self, rate_query):
        # Should not be called on a fixed fee cost component
        raise NotImplemented("Fixed component has no cost per thousand")

    def compute_coverage_for_premium(self, rate_query):
        pass

    def does_vary_with_coverage_selection(self):
        # Fixed fee
        return False


def build_eligibility_constraint(constraint_def):
    """
    Given a dictionary definition (from YAML or hard-coded), return the constraint objects
    used in figuring out if a part of the rate calculation applies to the given
    applicant query (for example, a rate request for a given applicant type, mode, and set of riders.)
    """
    constraint_keys = {
        'and': AndConstraint,
        'or': OrConstraint,
        'not': NotConstraint,
        'rider_included': build_rider_constraint,
        'applicant_type': ApplicantTypeMatchesConstraint,
        'coverage_selection': CoverageSelectionConstraint,
    }
    if len(constraint_def.keys()) != 1:
        raise ValueError("Invalid constraint, should have exactly 1 constraint: %s" % constraint_def)
    key = constraint_def.keys()[0]
    if key not in constraint_keys:
        raise ValueError("Invalid constraint key '%s'" % key)

    # If a compound constraint, wrap the val in a recursive call
    if key in ['and', 'or']:
        subconstraints = [
            build_eligibility_constraint(subconstraint_def)
            for subconstraint_def in constraint_def[key]
            ]
        initializer = subconstraints
    elif key == 'not':
        # 'not' has a single subconstraint
        initializer = build_eligibility_constraint(constraint_def[key])
    else:
        initializer = constraint_def[key]

    return constraint_keys[key](initializer)


def build_rider_constraint(rider_code):
    return ProductRiderIncludedConstraint(rider_code)


class RatePlan(object):
    def __init__(self, coverage_options, eligibility_constraint=None, rate_levels=None, max_coverage_amount=None):
        self.coverage_options = coverage_options
        self.cost_components = []
        self.eligibility_constraint = eligibility_constraint
        self.rate_levels = rate_levels
        self.max_coverage_amount = max_coverage_amount

    def get_coverage_options_by_face(self, applicant_query):
        return [option.get_face_amount() for option in self.coverage_options if option.is_by_face()]

    def get_premium_options(self, mode):
        mode_options = {
            12: [
                Decimal('10'),
                Decimal('15'),
                Decimal('20'),
                Decimal('25'),
                Decimal('30'),
                Decimal('35'),
                Decimal('40'),
                Decimal('45'),
                Decimal('50'),
            ],
            24: [
                Decimal('5'),
                Decimal('7.50'),
                Decimal('10'),
                Decimal('12.50'),
                Decimal('15'),
                Decimal('17.50'),
                Decimal('20'),
                Decimal('22.50'),
                Decimal('25'),
            ],
            26: [
                Decimal('6'),
                Decimal('8'),
                Decimal('10'),
                Decimal('12'),
                Decimal('14'),
                Decimal('16'),
                Decimal('18'),
                Decimal('20'),
            ],
            52: [
                Decimal('3'),
                Decimal('4'),
                Decimal('5'),
                Decimal('6'),
                Decimal('7'),
                Decimal('8'),
                Decimal('9'),
                Decimal('10'),
            ]
        }
        return mode_options.get(mode, [])

    def get_all_rates_by_premium(self, applicant_query):
        rates = []
        for premium in self.get_premium_options(applicant_query.get_mode()):
            applicant_query.rate_options = ApplicantQueryOptions({'by_premium': premium})
            coverage_amount = int(self.calculate_coverage(applicant_query))

            # Sometimes a premium level results in a coverage that is too big for a given product's rate plan.
            if not self.is_coverage_amount_allowed(coverage_amount):
                continue

            rates.append({
                'coverage': coverage_amount,
                'premium': float(premium),
                'payment_mode': applicant_query.get_mode(),
            })

        return rates

    def get_all_rates_by_coverage(self, applicant_query):
        rates = []

        # Mutate the query slightly each time through by changing the rate options
        #  (to get the coverage for the requested premium).
        for coverage in self.get_coverage_options_by_face(applicant_query):
            applicant_query.rate_options = ApplicantQueryOptions({'by_coverage': coverage})
            rates.append({
                'premium': float(self.calculate_premium(applicant_query)),
                'coverage': int(coverage),
                'payment_mode': applicant_query.get_mode(),
            })

        return rates

    def is_eligible(self, applicant_query):
        return self.eligibility_constraint.is_satisfied(applicant_query)

    def add_cost_component(self, cost_component):
        self.cost_components.append(cost_component)

    def calculate_premium(self, applicant_query):
        """
        Rounding order is important to get to-the-penny matching with Dell / 5Star.
        Each cost component does its own rounding, then we round the modalized total of the cost components.
        """
        component_premiums = [component.compute_annual_premium(applicant_query)
                              for component in self.cost_components
                              if component.is_enabled(applicant_query) and component.compute_annual_premium(
                applicant_query)]
        component_sum = sum(component_premiums, Decimal('0.00'))
        modalized_total = component_sum / applicant_query.get_mode()
        rounded = modalized_total.quantize(Decimal('0.00'), rounding=ROUND_HALF_UP)

        if str(rounded) == '0.00':
            # If we fall outside premium range, return None for now
            return None
        else:
            return rounded

    def calculate_coverage(self, applicant_query):
        """
        Figures out how much coverage is available for a given premium.
        Needs to subtract out fee and rider costs first to determine the premium that is allocated to coverage.
        """
        # import ipdb; ipdb.set_trace()
        mode = applicant_query.get_mode()
        selected_premium = applicant_query.get_selected_premium()
        fixed_annual_fees = sum([component.compute_annual_premium(applicant_query)
                                 for component in self.cost_components
                                 if not component.does_vary_with_coverage_selection() and component.is_enabled(
                applicant_query)],
                                Decimal('0.00'))

        # Remove fees from the annualized modal premium, divide by the combined ACPTs, and convert to coverage
        numerator = (selected_premium * mode - fixed_annual_fees)
        # Divide by the sum of the annualized
        denominator = sum([
                              component.get_cost_per_thousand(applicant_query)
                              for component in self.cost_components
                              if component.does_vary_with_coverage_selection() and component.is_enabled(
                applicant_query) and component.get_cost_per_thousand(applicant_query)
                              ], Decimal('0.00'))

        # Prevent division by zero
        if denominator == Decimal('0.00'):
            return None

        # Convert to coverage by multiplying ACPT by 1000 and rounding to nearest dollar
        return (numerator * 1000 / denominator).quantize(Decimal('0'), rounding=ROUND_HALF_UP)

    @classmethod
    def load_from_yaml(cls, filename):
        with open(os.path.join(DATA_DIR, filename), 'r') as f:
            rate_plan_data = yaml_load(f, Loader=YamlLoader)

            # Load rate tables
            rate_tables = {}
            for rate_table_def in rate_plan_data['rate_tables']:

                table_mapping = cls.extract_lookup_table(rate_table_def)

                # Create the appropriate type of lookup table.
                if rate_table_def['key'] == 'age':
                    rate_table = AgeRateLookupTable(table_mapping)
                    rate_tables[rate_table_def['name']] = rate_table

                elif rate_table_def['key'] == 'rate_level':
                    rate_table = RateLevelRateLookupTable(table_mapping)
                    rate_tables[rate_table_def['name']] = rate_table

                else:
                    raise ValueError("Invalid rate table key type '{}'".format(rate_table_def['key']))

            rate_levels = None
            if 'rate_levels' in rate_plan_data:
                rate_levels = list()
                for rate_level in rate_plan_data['rate_levels']:
                    rate_levels.append({'name': rate_level['name'], 'value': rate_level['value']})
                    
                    
            max_coverage_amount = rate_plan_data.get('max_coverage_amount', None)



            rate_plan = RatePlan(coverage_options=standard_coverage_options, rate_levels=rate_levels, max_coverage_amount=max_coverage_amount)
            for cost_component_def in rate_plan_data['cost_components']:

                if cost_component_def['type'] == "ACPT Lookup":
                    lookup_table = rate_tables[cost_component_def['rate_table']]
                    cost_component = LookupTableCostComponent(
                        lookup_table,
                        does_vary_with_coverage=True,
                        is_enabled_constraint=build_eligibility_constraint(cost_component_def['is_eligible'])
                    )
                    rate_plan.add_cost_component(cost_component)
                elif cost_component_def['type'] == 'Annual Premium Lookup':
                    lookup_table = rate_tables[cost_component_def['rate_table']]
                    cost_component = AnnualPremiumLookupTableCostComponent(
                        lookup_table,
                        does_vary_with_coverage=True,
                        is_enabled_constraint=build_eligibility_constraint(cost_component_def['is_eligible'])
                    )
                    rate_plan.add_cost_component(cost_component)
                elif cost_component_def['type'] == "Fixed Annual Fee":
                    cost_component = FlatFeeCostComponent(
                        Decimal(cost_component_def['fee']),
                        is_enabled_constraint=build_eligibility_constraint(cost_component_def['is_eligible'])
                    )
                    rate_plan.add_cost_component(cost_component)
                else:
                    raise ValueError("Invalid cost component '{}'".format(cost_component_def['type']))

            return rate_plan

    @classmethod
    def extract_lookup_table(cls, rate_table_def):
        # Convert string values to decimal
        table_mapping = {}
        for key, val in rate_table_def['table'].iteritems():
            table_mapping[int(key)] = Decimal(val)
        return table_mapping

    def is_coverage_amount_allowed(self, coverage_amount):

        if self.max_coverage_amount is None:
            return True

        return coverage_amount < self.max_coverage_amount



def load_rate_plan_for_base_product(base_product_code):
    if base_product_code == 'FPPTI':
        return RatePlan.load_from_yaml('fppti.yaml')
    elif base_product_code == 'FPPCI':
        return RatePlan.load_from_yaml('fppci.yaml')
    elif base_product_code == 'FPP-Gov':
        return RatePlan.load_from_yaml('fpptig.yaml')
    elif base_product_code == 'FPPTIW':
        return RatePlan.load_from_yaml('fpptiw.yaml')
    elif base_product_code == 'FPPTIY':
        return RatePlan.load_from_yaml('fpptiy.yaml')
    elif base_product_code == 'FPPTIB':
        return RatePlan.load_from_yaml('fpptib.yaml')
    elif base_product_code == 'ACC':
        return RatePlan.load_from_yaml('acc.yaml')
    elif base_product_code == 'HI':
        return RatePlan.load_from_yaml('hi.yaml')
    else:
        raise ValueError("No rate plan configured for base product '{}'".format(base_product_code))


standard_coverage_options = [
    CoverageOption(is_by_face=True, amount=10000),
    CoverageOption(is_by_face=True, amount=20000),
    CoverageOption(is_by_face=True, amount=25000),
    CoverageOption(is_by_face=True, amount=30000),
    CoverageOption(is_by_face=True, amount=40000),
    CoverageOption(is_by_face=True, amount=50000),
    CoverageOption(is_by_face=True, amount=60000),
    CoverageOption(is_by_face=True, amount=70000),
    CoverageOption(is_by_face=True, amount=75000),
    CoverageOption(is_by_face=True, amount=80000),
    CoverageOption(is_by_face=True, amount=90000),
    CoverageOption(is_by_face=True, amount=100000),
    CoverageOption(is_by_face=True, amount=110000),
    CoverageOption(is_by_face=True, amount=125000),
    CoverageOption(is_by_face=True, amount=130000),
    CoverageOption(is_by_face=True, amount=140000),
    CoverageOption(is_by_face=True, amount=150000),
]
