from collections import defaultdict
from decimal import Decimal

from models import EnrollmentApplication, EnrollmentApplicationCoverage
from taa.services import RequiredFeature
from enrollment_application_coverages import (
    select_most_recent_coverage,
    filter_applicant_coverages,
    group_coverages_by_product,
)


class EnrollmentReportService(object):

    case_service = RequiredFeature('CaseService')

    def get_enrollment_report(self, case):
        report_data = {}
        census_records = self.case_service.get_census_records(case)
        merged_enrollment_applications = [merge_enrollments(census_record)
                                          for census_record in census_records]
        enrollment_applications = []
        for census_record in census_records:
            enrollment_applications += [dict(enrollment=e,
                                             coverages=group_coverages_by_product(e.coverages))
                                        for e in census_record.enrollment_applications]
        report_data['company_name'] = case.company_name
        # Enrollment methods used
        report_data['enrollment_methods'] = self._find_enrollment_methods(enrollment_applications)
        # Enrollment period (most recent)
        if case:
            report_data['enrollment_period'] = self._get_enrollment_period_dates(case)
        else:
            report_data['enrollment_period'] = None
        stats = self.get_product_statistics(enrollment_applications)
        report_data['summary'] = self.build_report_summary(stats, case,
                                                           merged_enrollment_applications,
                                                           enrollment_applications)
        # Product data
        report_data['product_report'] = self.build_report_by_product(stats)
        # Agents on case
        report_data['case_owner'] = self.case_service.get_case_owner(case)
        report_data['case_agents'] = self.case_service.get_case_partner_agents(case)
        return report_data

    def _find_enrollment_methods(self, case_enrollments):
        return list({e['enrollment'].method for e in case_enrollments
                     if e['enrollment'] and
                     self._is_enrollment_finished(e['enrollment'])})

    def _is_enrollment_finished(self, e):
        return e.application_status in [
            EnrollmentApplication.APPLICATION_STATUS_ENROLLED,
            EnrollmentApplication.APPLICATION_STATUS_DECLINED
        ]

    def _get_enrollment_period_dates(self, case):
        most_recent = self.case_service.get_most_recent_enrollment_period(case)
        if not most_recent:
            start = None,
            end = None
        else:
            start = most_recent.get_start_date()
            end = most_recent.get_end_date()
        return dict(start=start, end=end)

    def build_report_summary(self, stats, case, merged_enrollment_applications,
                             unmerged_enrollment_applications):
        """
        Summary data
          - % (#) processed (Enrolled + Declined) (for uploaded census only)
          - % (#) taken (status=Enrolled (not Declined or blank)) (for uploaded census only)
          - total annualized premium
        """
        if self._is_uploaded_census(merged_enrollment_applications):
            return dict(
                processed_enrollments=self.get_num_processed_enrollments(
                    merged_enrollment_applications),
                taken_enrollments=self.get_num_taken_enrollments(
                    merged_enrollment_applications),
                declined_enrollments=self.get_num_declined_enrollments(
                    merged_enrollment_applications),
                total_census=self.get_num_census_records(
                    merged_enrollment_applications),
                total_annualized_premium=self.get_total_annualized_premium(
                    unmerged_enrollment_applications),
                is_census_report=True,
                only_one_product=self.has_only_one_product(case),
                product_names=self.get_product_names(case),
            )
        else:
            return dict(
                processed_enrollments=self.get_num_processed_enrollments(
                    merged_enrollment_applications),
                total_census=self.get_num_census_records(
                    merged_enrollment_applications),
                total_annualized_premium = self.get_total_annualized_premium(
                    unmerged_enrollment_applications),
                is_census_report=False,
                only_one_product=self.has_only_one_product(case),
                product_names=self.get_product_names(case),
            )

    def has_only_one_product(self, case):
        return len(case.products) == 1

    def get_product_names(self, case):
        return [p.name for p in case.products]

    def _is_uploaded_census(self, merged_enrollment_applications):
        """
        If any census record is uploaded, we consider the whole thing
        a census-enrolled case (Zach's guess)
        """
        return any(map(lambda e:
                       (e['enrollment'].census_record.is_uploaded_census
                        if e['enrollment'] else False),
                       merged_enrollment_applications))

    def get_num_processed_enrollments(self, merged_enrollment_applications):
        return sum(1 for enrollment in merged_enrollment_applications
                   if enrollment['enrollment'] and
                   enrollment['enrollment'].did_process())

    def get_num_taken_enrollments(self, merged_enrollment_applications):
        return sum(1 for e in merged_enrollment_applications
                   if e['enrollment'] and e['enrollment'].did_enroll())

    def get_num_declined_enrollments(self, merged_enrollment_applications):
        return sum(1 for e in merged_enrollment_applications
                   if e['enrollment'] and not e['enrollment'].did_enroll())

    def get_total_annualized_premium(self, unmerged_enrollment_applications):
        total = Decimal('0.00')
        for e in unmerged_enrollment_applications:
            all_coverages = []
            for product, coverages in e['coverages'].iteritems():
                all_coverages += coverages
            total += sum(map(lambda c: c.get_annualized_premium(),
                             all_coverages))
        return total

    def get_num_census_records(self, records):
        return sum(1 for e in records)

    def build_report_by_product(self, stats):
        """
        For each product,
        product name, (%) of census, $ of annual premium
        Example:

        product: count (% of census), total annual premium
        FPP-TI: 128 (27%), $32,577
        Critical Illness: 47 (10%), $22,913
        """
        product_report_data = {}
        for product in stats.get_products_used():
            product_report_data[product.id] = dict(
                product_name=product.name,
                enrolled_count=stats.get_taken_count_for_product(product),
                total_annualized_premium=stats.get_annual_premium_for_product(
                    product),
            )
        return product_report_data

    def get_product_statistics(self, merged_enrollment_applications):
        stats = ProductStatsAccumulator()
        for enrollment_application in merged_enrollment_applications:
            stats.add_coverages_for_enrollment_application(
                enrollment_application)
        return stats


def merge_enrollments(census_record):
    all_coverages = []
    for enrollment in census_record.enrollment_applications:
        all_coverages += enrollment.coverages
    if census_record.enrollment_applications:
        most_recent_enrollment = max([
            app for app in census_record.enrollment_applications if app.signature_time
            ],
                                     key=lambda e: e.signature_time)
    else:
        most_recent_enrollment = None
    return {
        'enrollment': most_recent_enrollment,
        'coverages': merge_enrollment_application_coverages(all_coverages),
    }


def merge_enrollment_application_coverages(all_coverages):
    """
    Each time the application is filled out, one or more coverages are added
    for this enrollment_application record.

    We want, for each applicant type (emp, spouse, children), the most recent
    coverage by product.

    So, if an employee signs up twice for the same product, we want only the
    latest coverage.
    """
    merged_coverages_by_product = defaultdict(list)
    for applicant_type in [
            EnrollmentApplicationCoverage.APPLICANT_TYPE_EMPLOYEE,
            EnrollmentApplicationCoverage.APPLICANT_TYPE_SPOUSE,
            EnrollmentApplicationCoverage.APPLICANT_TYPE_CHILD]:
        applicant_coverages = filter_applicant_coverages(all_coverages,
                                                         applicant_type)
        coverages_by_product = group_coverages_by_product(applicant_coverages)
        most_recent_coverage_by_product = {
            p: select_most_recent_coverage(coverages)
            for p, coverages in coverages_by_product.iteritems()
        }
        for product, coverage in most_recent_coverage_by_product.iteritems():
            merged_coverages_by_product[product].append(coverage)
    return merged_coverages_by_product


class ProductStatsAccumulator(object):
    """
    Gathers the stats needed for the coverages on an application for display
    on reports
    """
    def __init__(self):
        self._product_premiums = defaultdict(Decimal)
        self._product_counts = defaultdict(int)
        #self._product_declines = defaultdict(int)

    def add_coverages_for_enrollment_application(self,
                                                 merged_enrollment_application):
        # Count taken products
        for product in merged_enrollment_application['coverages']:
            self._product_counts[product] += 1
        # Count declined products - TODO: when multiproduct is added,
        # individual products may be declined
        # if merged_enrollment_application['enrollment'] and not merged_enrollment_application['enrollment'].did_enroll():
        #     self._product_declines[product] += 1

        # Count annualized premiums by product
        for product, coverages in merged_enrollment_application['coverages'].iteritems():
            self._product_premiums[product] += sum(
                coverage.get_annualized_premium() for coverage in coverages)

    def get_products_used(self):
        return self._product_counts.keys()

    def get_taken_count_for_product(self, product):
        if product not in self._product_counts:
            return None
        return self._product_counts[product]

    def get_annual_premium_for_product(self, product):
        if product not in self._product_premiums:
            return None
        return self._product_premiums[product]

