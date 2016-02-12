"""
A simple module that translates a request for coverage into the Dell / 5Star plan codes.

Reference the FPPTI-MATRIX-from-Dell-2015-12-01.xlsx in the Artifacts directory for more info.

"""


def get_plan_code(base_product_code, applicant_query):

    applicant_type = applicant_query.get_applicant_type()
    riders = applicant_query.get_riders()
    state = applicant_query.state

    plan_code = ''

    return plan_code

