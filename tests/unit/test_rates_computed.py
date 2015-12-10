from unittest2 import TestCase

from taa.services.products.RatePlan import (
    RatePlan,

    ApplicantQuery,
    ApplicantQueryOptions,
    ApplicantDemographics,

    ApplicantTypeMatchesConstraint,
    ProductRiderIncludedConstraint,
    AndConstraint,
    NotConstraint,
    OrConstraint,

    MODE_WEEKLY,
    APPLICANT_EMPLOYEE,
    APPLICANT_CHILD,
    APPLICANT_SPOUSE,

    WaiverOfPremiumRider,

    LookupTableCostComponent,
    FlatFeeCostComponent,

    AgeRateLookupTable,
)

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

class TestRatePlan(TestCase):
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

    def test_it_can_compute_coverage_given_a_desired_premium(self):
        TEST_PREMIUM = 10.00
        TEST_MODE = MODE_WEEKLY
        TEST_PREMIUM_ACPT = 2.00
        TEST_AGE = 27
        TEST_FEE = 52.00

        rate_plan = self._build_rate_plan_for_acpt(TEST_AGE, TEST_PREMIUM_ACPT, annual_fee=TEST_FEE)
        rate_query = self._build_applicant_query(APPLICANT_EMPLOYEE, TEST_AGE, TEST_MODE, for_premium=TEST_PREMIUM)

        coverage = rate_plan.calculate_coverage(rate_query)

        # Should be annualized premium, less fee, divided by ACPT * 1000, rounded to nearest dollar
        expected_coverage = round((TEST_PREMIUM * TEST_MODE - TEST_FEE) * 1000 / (TEST_PREMIUM_ACPT), 0)
        self.assertEqual(coverage, expected_coverage)


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


