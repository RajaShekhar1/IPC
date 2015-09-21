import json
from collections import defaultdict

from taa.core import DBService
from models import EnrollmentApplicationCoverage


class EnrollmentApplicationCoverageService(DBService):
    __model__ = EnrollmentApplicationCoverage

    def create_coverage(self, enrollment, product, data, applicant_data,
                        applicant_coverage, applicant_type):
        # Set proper premium
        payment_mode = data.get('payment_mode')
        weekly_premium = None
        biweekly_premium = None
        semimonthly_premium = None
        monthly_premium = None
        if payment_mode == 52:
            weekly_premium = applicant_coverage['premium']
        elif payment_mode == 26:
            biweekly_premium = applicant_coverage['premium']
        elif payment_mode == 24:
            semimonthly_premium = applicant_coverage['premium']
        elif payment_mode == 12:
            monthly_premium = applicant_coverage['premium']
        else:
            raise Exception("Invalid payment mode %s"%payment_mode)

        return self.create(**dict(
            coverage_status=EnrollmentApplicationCoverage.COVERAGE_STATUS_ENROLLED,
            enrollment_application_id=enrollment.id,
            product_id=product.id,
            applicant_type=applicant_type,
            height_inches=(applicant_data['height']
                           if applicant_data['height'] else None),
            weight_pounds=(applicant_data['weight']
                           if applicant_data['weight'] else None),
            is_smoker=applicant_data.get('is_smoker'),
            coverage_face_value=applicant_coverage['face_value'],
            weekly_premium=weekly_premium,
            biweekly_premium=biweekly_premium,
            semimonthly_premium=semimonthly_premium,
            monthly_premium=monthly_premium,
            soh_answers=json.dumps(dict(
                existing_insurance=data['existing_insurance'],
                replacing_insurance=data['replacing_insurance'],
                health_questions=applicant_data['soh_questions'],
            ))
        ))

    def get_coverages_for_employee(self, census_record):
        coverages = self.get_census_record_coverages(census_record)
        return filter_applicant_coverages(coverages,
                                          EnrollmentApplicationCoverage.APPLICANT_TYPE_EMPLOYEE)

    def get_coverages_for_spouse(self, census_record):
        coverages = self.get_census_record_coverages(census_record)
        return filter_applicant_coverages(coverages,
                                          EnrollmentApplicationCoverage.APPLICANT_TYPE_SPOUSE)

    def get_coverages_for_children(self, census_record):
        coverages = self.get_census_record_coverages(census_record)
        return filter_applicant_coverages(coverages,
                                          EnrollmentApplicationCoverage.APPLICANT_TYPE_CHILD)

    def get_census_record_coverages(self, census_record):
        coverages = []
        for app in census_record.enrollment_applications:
            coverages += app.coverages
        return coverages


def select_most_recent_coverage(coverages):
    if not coverages:
        return None
    # Return most recently signed coverage out of the groups of coverages
    return max(coverages, key=lambda c: c.enrollment.signature_time)


def filter_applicant_coverages(coverages, applicant_type):
    return filter(lambda c: c.applicant_type == applicant_type, coverages)

def group_coverages_by_product(coverages):
    """
    Groups coverages by their product type.
    Returns a dictionary with products as keys and a list of coverages as values
    """
    product_coverages = defaultdict(list)
    for coverage in coverages:
        product_coverages[coverage.product].append(coverage)
    return product_coverages

