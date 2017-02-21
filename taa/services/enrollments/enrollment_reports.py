from collections import defaultdict
from decimal import Decimal

from taa.services.products import Product

from taa.services.enrollments import EnrollmentApplicationService

from models import EnrollmentApplication, EnrollmentApplicationCoverage
from taa import db
from taa.services.cases import CaseCensus
from taa.services import RequiredFeature
from enrollment_application_coverages import (
    select_most_recent_coverage,
    filter_applicant_coverages,
    group_coverages_by_product,
)


class EnrollmentReportService(object):

    case_service = RequiredFeature('CaseService')

    def get_enrollment_report(self, case):
        
        app_service = EnrollmentApplicationService()

        
        report_data = {
            # "product_report": {
            #                       "1": {
            #                           "enrolled_count": 15304,
            #                           "product_name": "Family Protection Plan - Terminal Illness",
            #                           "total_annualized_premium": "10005980.88"
            #                       },
            #                       "2": {
            #                           "enrolled_count": 126,
            #                           "product_name": "Family Protection Plan - Critical Illness",
            #                           "total_annualized_premium": "63905.88"
            #                       },
            #                       "23": {
            #                           "enrolled_count": 16,
            #                           "product_name": "Group CI - Health Depot",
            #                           "total_annualized_premium": "5138.16"
            #                       },
            #                       "71": {
            #                           "enrolled_count": 5008,
            #                           "product_name": "HealthDepot Membership",
            #                           "total_annualized_premium": "540864.00"
            #                       }
            #                   },
            "summary": {
                "is_census_report": False,
                "only_one_product": False,
                "processed_enrollments": 14929,
                "product_names": [
                    "HealthDepot Membership",
                    "Family Protection Plan - Terminal Illness",
                    "Family Protection Plan - Critical Illness",
                    "Group CI - Health Depot"
                ],
                "total_annualized_premium": "10504869.12",
                "total_census": 15011
            }
        }

        used_product_ids = [r.product_id for r in
                            db.session.query(EnrollmentApplicationCoverage.product_id.label('product_id')
                                             ).group_by(EnrollmentApplicationCoverage.product_id
                                                        ).filter(EnrollmentApplication.case_id == case.id
                                                                 ).filter(
                                EnrollmentApplicationCoverage.enrollment_application_id == EnrollmentApplication.id
                                ).all()
                            ]
        report_data['enrollment_methods'] = [r.method for r in
                            db.session.query(EnrollmentApplication.method.label('method')
                                             ).group_by(EnrollmentApplication.method
                                             ).filter(EnrollmentApplication.case_id == case.id
                                             ).all()
                            ]

        product_taken_counts = defaultdict(int)
        product_declined_counts = defaultdict(int)

        for product_id in used_product_ids:
            q = db.session.query(EnrollmentApplication
                                 ).filter(EnrollmentApplication.is_preview == False
                                 ).filter(
                EnrollmentApplication.coverages.any(EnrollmentApplicationCoverage.product_id == product_id)
                ).filter(EnrollmentApplication.case_id == case.id
                )
            taken_q = q.filter(
                EnrollmentApplication.application_status == EnrollmentApplication.APPLICATION_STATUS_ENROLLED)
            declined_q = q.filter(
                EnrollmentApplication.application_status == EnrollmentApplication.APPLICATION_STATUS_DECLINED)
            product_taken_counts[product_id] = taken_q.count()
            product_declined_counts[product_id] = declined_q.count()

        # Get product total premiums
        product_annual_premiums = {}
        for product_id in used_product_ids:
            row = db.session.query(app_service._select_total_annual_premium().label('total_premium')
                                   ).filter(
                EnrollmentApplicationCoverage.enrollment_application_id == EnrollmentApplication.id
                ).filter(EnrollmentApplicationCoverage.product_id == product_id
                         ).filter(EnrollmentApplication.case_id == case.id
                                  ).filter(EnrollmentApplication.is_preview == False
                                           ).filter(
                EnrollmentApplication.application_status == EnrollmentApplication.APPLICATION_STATUS_ENROLLED
                ).one()
            product_annual_premiums[product_id] = row.total_premium

        grand_total_premium = sum(product_annual_premiums.values())

        # Add to report
        report_data['product_report'] = {}
        for product_id in used_product_ids:
            product = db.session.query(Product).get(product_id)
            report_data['product_report'][product_id] = {
                "enrolled_count": product_taken_counts[product_id],
                "product_name": product.get_brochure_name(),
                "total_annualized_premium": product_annual_premiums[product_id]
            }
        
        # Summary data
        num_processed = db.session.query(CaseCensus
            ).filter(CaseCensus.case_id == case.id
            ).filter(CaseCensus.enrollments.any(db.and_(
                EnrollmentApplication.is_preview == False,
                db.or_(EnrollmentApplication.application_status.in_(
                    EnrollmentApplication.APPLICATION_STATUS_DECLINED,
                    EnrollmentApplication.APPLICATION_STATUS_ENROLLED,
                ))
        report_data['summary']['processed_enrollments'] = num_processed
        report_data['summary']['total_annualized_premium'] = grand_total_premium
        
        # Enrollment methods used
        #report_data['enrollment_methods'] = self._find_enrollment_methods(enrollment_applications)

        # Enrollment period (most recent)
        if case:
            report_data['enrollment_period'] = self._get_enrollment_period_dates(case)
        else:
            report_data['enrollment_period'] = None

        # Agents on case
        report_data['company_name'] = case.company_name
        report_data['case_owner'] = self.case_service.get_case_owner(case)

        print("Getting partner agents")
        report_data['case_agents'] = self.case_service.get_case_partner_agents(case)
        
        return report_data

    def get_enrollment_report_old(self, case):
        report_data = {}
        print("Retrieving census for case {}...".format(case.id))
        census_records = self.case_service.get_census_records(case)
        print("Merging census records...")
        merged_enrollment_applications = [merge_enrollments(census_record)
                                          for census_record in census_records]

        print("Grouping coverages with enrollments...")
        enrollment_applications = []
        for census_record in census_records:
            enrollment_applications += [dict(enrollment=e,
                                             coverages=group_coverages_by_product(e.coverages))
                                        for e in census_record.enrollment_applications if not e.is_preview]
        report_data['company_name'] = case.company_name

        print("Gathering enrollment methods for {} apps...".format(len(enrollment_applications)))
        # Enrollment methods used
        report_data['enrollment_methods'] = self._find_enrollment_methods(enrollment_applications)

        print("Gathering enrollment period for {} apps...".format(len(enrollment_applications)))
        # Enrollment period (most recent)
        if case:
            report_data['enrollment_period'] = self._get_enrollment_period_dates(case)
        else:
            report_data['enrollment_period'] = None

        print("Gathering product stats for {} apps...".format(len(enrollment_applications)))
        stats = self.get_product_statistics(enrollment_applications)

        print("Building report summary for {} apps...".format(len(enrollment_applications)))
        report_data['summary'] = self.build_report_summary(stats, case,
                                                           merged_enrollment_applications,
                                                           enrollment_applications)

        print("Building report data for stats by product...")
        # Product data
        report_data['product_report'] = self.build_report_by_product(stats)

        # Agents on case
        report_data['case_owner'] = self.case_service.get_case_owner(case)

        print("Getting partner agents")
        report_data['case_agents'] = self.case_service.get_case_partner_agents(case)

        print("Done")
        return report_data

    def get_census_record_tuples(self, case):
        # Returns raw data without extras for reporting speed.

        query = db.session.query(
            CaseCensus.id.label('census_record_id'),
            EnrollmentApplication.application_status.label('application_status'),
            EnrollmentApplication.method.label('method'),
            EnrollmentApplicationCoverage.annual_premium.label('annual_premium'),
            EnrollmentApplicationCoverage.product_id.label('product_id'),

        ).filter(CaseCensus.case_id == case.id
                 )
        query = query.outerjoin('enrollment_applications', 'coverages')

        return db.session.execute(query.statement)

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
                   if e['enrollment'] and e['enrollment'].did_decline())

    def get_total_annualized_premium(self, unmerged_enrollment_applications):
        total = Decimal('0.00')
        for e in unmerged_enrollment_applications:
            if not e['enrollment'].did_enroll():
                continue

            all_coverages = []
            for product, coverages in e['coverages'].iteritems():
                all_coverages += coverages
            total += sum(map(lambda c: c.get_annualized_premium(),
                         filter(lambda c: c.did_enroll(), all_coverages))
                        )
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
        if not enrollment.is_preview:
            all_coverages += enrollment.coverages
    enrollments_with_timestamp = [
        app for app in census_record.enrollment_applications if app.signature_time and not app.is_preview
        ]
    if enrollments_with_timestamp:
        most_recent_enrollment = max(enrollments_with_timestamp,
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

