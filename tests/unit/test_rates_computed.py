from unittest2 import TestCase


class TestRatesCalculator(TestCase):
    def setUp(self):
        pass

    def test_it_can_compute_basic_modal_premium_for_coverage(self):

        TEST_AGE = 27
        TEST_PREMIUM_ACPT = 3.82
        TEST_COVERAGE = 10000

        # Set up a rate plan to reflect these rates
        rate_plan = self._build_rate_plan_for_acpt(TEST_AGE, TEST_PREMIUM_ACPT)

        # Construct a query to ask for the premium for our test applicant
        rate_query = self._build_applicant_query(APPLICANT_EMPLOYEE, TEST_AGE, MODE_WEEKLY, for_coverage=TEST_COVERAGE)

        # Execute the query
        premium = rate_plan.calculate_premium(rate_query)

        # Assert
        # Since there is no fee attached, we just look up the ACPT and multiply by 10 (for 10k of coverage)
        expected_annual = TEST_COVERAGE / 1000 * TEST_PREMIUM_ACPT
        expected_modal = round(round(expected_annual, 2) / MODE_WEEKLY, 2)
        self.assertEqual(premium, expected_modal)

    def test_it_can_compute_premium_with_annual_fee(self):
        TEST_AGE = 27
        TEST_PREMIUM_ACPT = 3.82
        TEST_FEE = 52.00
        TEST_COVERAGE = 10000

        rate_plan = self._build_rate_plan_for_acpt(TEST_AGE, TEST_PREMIUM_ACPT, annual_fee=TEST_FEE)
        rate_query = self._build_applicant_query(APPLICANT_EMPLOYEE, TEST_AGE, MODE_WEEKLY, for_coverage=TEST_COVERAGE)

        premium = rate_plan.calculate_premium(rate_query)

        expected_annual = round(TEST_COVERAGE / 1000 * TEST_PREMIUM_ACPT, 2) + TEST_FEE
        expected_modal = round(expected_annual / MODE_WEEKLY, 2)
        self.assertEqual(premium, expected_modal)

    def test_it_can_compute_premium_with_rider_cost_component(self):
        TEST_AGE = 27
        TEST_PREMIUM_ACPT = 3.82
        TEST_RIDER_ACPT = 0.14
        TEST_FEE = 52.00
        TEST_COVERAGE = 25000
        WP_RIDER = WaiverOfPremiumRider()

        rate_plan = self._build_plan_with_rider(TEST_AGE, TEST_FEE, TEST_PREMIUM_ACPT, TEST_RIDER_ACPT, WP_RIDER)
        rate_query = self._build_applicant_query(APPLICANT_EMPLOYEE, TEST_AGE, MODE_WEEKLY, for_coverage=TEST_COVERAGE, riders=[WP_RIDER])

        premium = rate_plan.calculate_premium(rate_query)

        expected_annual = round(TEST_COVERAGE / 1000 * TEST_PREMIUM_ACPT, 2) + round(TEST_COVERAGE / 1000 * TEST_RIDER_ACPT, 2) + TEST_FEE
        expected_modal = round(expected_annual / MODE_WEEKLY, 2)
        self.assertEqual(premium, expected_modal)

    def test_it_can_compute_premium_when_rider_not_selected(self):
        " Build the plan that accepts the rider, but the query does not have the rider selected."
        TEST_AGE = 27
        TEST_PREMIUM_ACPT = 3.82
        TEST_RIDER_ACPT = 0.14
        TEST_FEE = 52.00
        TEST_COVERAGE = 25000
        WP_RIDER = WaiverOfPremiumRider()

        rate_plan = self._build_plan_with_rider(TEST_AGE, TEST_FEE, TEST_PREMIUM_ACPT, TEST_RIDER_ACPT, WP_RIDER)

        rate_query = self._build_applicant_query(APPLICANT_EMPLOYEE, TEST_AGE, MODE_WEEKLY, for_coverage=TEST_COVERAGE,
                                                 # Note - no riders selected
                                            riders=[])

        premium = rate_plan.calculate_premium(rate_query)

        expected_annual = round(TEST_COVERAGE / 1000 * TEST_PREMIUM_ACPT, 2) + TEST_FEE
        expected_modal = round(expected_annual / MODE_WEEKLY, 2)
        self.assertEqual(premium, expected_modal)

    def test_that_child_rate_does_not_include_rider_premium(self):
        TEST_PREMIUM_ACPT = 2.00
        # Note: Children don't actually have policy fees, but that is not configured in this test.
        TEST_FEE = 52.00
        TEST_COVERAGE = 10000
        rider = WaiverOfPremiumRider()
        rate_plan = self._build_plan_with_rider(age=18, premium_acpt=TEST_PREMIUM_ACPT, annual_fee=TEST_FEE, rider_acpt=1.00, rider=rider)

        # Include the rider in the request
        applicant_query = self._build_applicant_query(APPLICANT_CHILD, 18, MODE_WEEKLY, for_coverage=TEST_COVERAGE, riders=[rider])
        premium = rate_plan.calculate_premium(applicant_query)

        # Note: Children don't actually have policy fees, but that is not configured in this test.
        expected_annual = round(TEST_COVERAGE / 1000 * TEST_PREMIUM_ACPT, 2) + TEST_FEE
        expected_modal = round(expected_annual / MODE_WEEKLY, 2)
        self.assertEqual(premium, expected_modal)

    def _build_applicant_query(self, applicant_type, age, mode, for_coverage=None, for_premium=None, riders=None):

        if not riders:
            riders = []

        if for_coverage:
            rate_options = ApplicantQueryOptions({'by_coverage': for_coverage})
        else:
            rate_options = ApplicantQueryOptions({'by_premium': for_premium})

        rate_query = ApplicantQuery(
            applicant_type=applicant_type,
            product_options={'riders': riders},
            mode=mode,
            state='IN',
            rate_options=rate_options,
            demographics=ApplicantDemographics({
                'age': age,
            })
        )
        return rate_query

    def _build_plan_with_rider(self, age, annual_fee, premium_acpt, rider_acpt, rider):
        rate_plan = self._build_rate_plan_for_acpt(age, premium_acpt, annual_fee=annual_fee)
        # Add the rider cost component
        rider_age_table = AgeRateLookupTable({age: rider_acpt})
        rider_constraint = AndConstraint([
            # Rider must be included in the request
            ProductRiderIncludedConstraint(rider=rider),
            # Applicant must be Emp or Spouse
            OrConstraint([
                ApplicantTypeMatchesConstraint(APPLICANT_EMPLOYEE),
                ApplicantTypeMatchesConstraint(APPLICANT_SPOUSE),
            ])
        ])
        rate_plan.add_cost_component(LookupTableCostComponent(rider_age_table,
                                                              is_enabled_constraint=rider_constraint))
        return rate_plan

    def _build_rate_plan_for_acpt(self, age, premium_acpt, annual_fee=None, eligibility_constraint=None):
        rate_plan = RatePlan(eligibility_constraint=eligibility_constraint)
        acpt_by_age_table = AgeRateLookupTable({age: premium_acpt})
        rate_plan.add_cost_component(LookupTableCostComponent(acpt_by_age_table))

        if annual_fee:
            rate_plan.add_cost_component(FlatFeeCostComponent(annual_fee))

        return rate_plan


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


class ApplicantQueryOptions(object):
    def __init__(self, options):
        self.options = options

    def get_requested_coverage(self):
        return self.options['by_coverage']


class WaiverOfPremiumRider(object):
    pass


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
        return not(self.subconstraint.is_satisfied(applicant_query))


class ProductRiderIncludedConstraint(ApplicantQueryConstraint):
    def __init__(self, rider):
        self.rider = rider

    def is_satisfied(self, applicant_query):
        query_riders = applicant_query.get_riders()
        return self.rider in query_riders


class ApplicantTypeMatchesConstraint(ApplicantQueryConstraint):
    def __init__(self, applicant_type):
        assert applicant_type in [APPLICANT_EMPLOYEE, APPLICANT_SPOUSE, APPLICANT_CHILD]
        self.applicant_type = applicant_type

    def is_satisfied(self, applicant_query):
        return applicant_query.get_applicant_type() == self.applicant_type


class ApplicantDemographics(object):
    def __init__(self, demographics_object):
        self.age = demographics_object.get('age', None)

    def get_age(self):
        return self.age


class AgeRateLookupTable(object):
    def __init__(self, mapping):
        self._mapping = mapping

    def do_lookup(self, rate_query):
        age = rate_query.get_age()
        return self._mapping.get(age)


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
            return 0.0
        else:
            return self.rounded_cost_per_thousand(acpt, face_value)

    def rounded_cost_per_thousand(self, cost_per_thousand, face_value):
        return round(cost_per_thousand * face_value / 1000, 2)

    def does_vary_with_coverage_selection(self):
        # In general the lookup tables will vary with coverage selection (ACPT)
        return self.does_vary_with_coverage


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

class ApplicantQuery(object):
    """
    Used for querying the system for rates and
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
        return self.product_options['riders']

    def get_face_value(self):
        return self.rate_options.get_requested_coverage()


class RatePlan(object):
    def __init__(self, eligibility_constraint=None):
        self.cost_components = []
        self.eligibility_constraint = eligibility_constraint

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
                              if component.is_enabled(applicant_query)]
        component_sum = sum(component_premiums, 0.0)
        modalized_total = component_sum / applicant_query.get_mode()
        return round(modalized_total, 2)

    def calculate_coverage(self, applicant_query):
        """
        Figures out how much coverage is available for a given premium.
        Needs to subtract out fee and rider costs first to determine the premium that is allocated to coverage.
        """
        mode = applicant_query.get_mode()
        selected_premium = applicant_query.get_selected_premium()
        fixed_annual_fees = sum([component.compute_annual_premium(applicant_query)
                             for component in self.cost_components
                             if not component.does_vary_with_coverage_selection() and component.is_enabled(applicant_query)],
                            0.00)

        # Remove fees from the annualized modal premium, divide by the combined ACPTs, and convert to coverage
        numerator = (selected_premium * mode - fixed_annual_fees)
        # Divide by the sum of the annualized
        denominator = sum([
            component.get_cost_per_thousand(applicant_query)
            for component in self.cost_components
            if component.does_vary_with_coverage_selection() and component.is_enabled(applicant_query)
        ], 0.00)
        # Convert to coverage by multiplping ACPT by 1000 and rounding to nearest dollar
        return round((numerator * 1000 / denominator), 0)