"""
A simple module that translates a request for coverage into the Dell / 5Star plan codes.

Reference the FPPTI-MATRIX-from-Dell-2015-12-01.xlsx in the Artifacts directory for more info.

"""

from taa.services.products.RatePlan import APPLICANT_EMPLOYEE, APPLICANT_SPOUSE, APPLICANT_CHILD


PRODUCT_CODE_FPPTI = u'FPPTI'
PRODUCT_CODE_FPPTIG = u'FPPTIG'
PRODUCT_CODE_FPPTIW = u'FPPTIW'
PRODUCT_CODE_FPPTIB = u'FPPTIB'
PRODUCT_CODE_FPPTIY = u'FPPTIY'
PRODUCT_CODE_FPPCI = u'FPPCI'

PRODUCT_CODE_FPPTIG_INVALID = u'FPPTIG/INVALID'


def get_invalid_plan_code(base_product_code):
    if base_product_code == 'FPP-Gov':
        base_product_code = 'FPPTIG'
    return '{}/INVALID'.format(base_product_code)


def get_plan_code(base_product_code, applicant_query):
    applicant_type = applicant_query.get_applicant_type()
    riders = applicant_query.get_riders()
    state = applicant_query.state

    plan_code = None

    # Convert 5Star representation to Dell representation
    if base_product_code == 'FPP-Gov':
        base_product_code = PRODUCT_CODE_FPPTIG

    # Set rider flags
    has_air = 'AIR' in riders
    has_wp = 'WP' in riders
    has_qol3 = 'QOL3' in riders
    has_qol4 = 'QOL4' in riders

    # Detect and terminate early on invalid cases
    if len(riders) > 2:
        # More than two riders; currently invalid
        return None
    elif len(riders) == 2:
        # Two riders
        if has_air and has_wp:
            # AIR and WP riders are mutually exclusive
            return None
        if has_qol3 and has_qol4:
            # QOL3 and QOL4 riders are mutually exclusive
            return None
    if len(riders) > 0:
        # One or more riders
        if applicant_type == APPLICANT_CHILD:
            # Children can't have any riders
            return None
        if base_product_code == PRODUCT_CODE_FPPCI:
            # FPPCI can't have any riders
            return None
    if base_product_code == PRODUCT_CODE_FPPTIG:
        if has_air:
            # FPPTIG can't have AIR rider
            return None
    if base_product_code not in [PRODUCT_CODE_FPPTI, PRODUCT_CODE_FPPTIG,
                                 PRODUCT_CODE_FPPTIW, PRODUCT_CODE_FPPCI]:
        # Unhandled base product
        return None

    # Determine state suffix
    suffix = ''
    if ((state == 'MD' or state == 'UT') and
            (base_product_code == PRODUCT_CODE_FPPTI or base_product_code == PRODUCT_CODE_FPPCI)):
        # States are appended to FPPTI and FPPCI only (not FPPTIW nor FPPTIG)
        suffix = state
    if len(suffix) != 0:
        suffix = '/{}'.format(suffix)

    if base_product_code == PRODUCT_CODE_FPPCI:
        # FPPCI
        if applicant_type == APPLICANT_CHILD:
            plan_code = 'INDFPD'
        else:
            plan_code = 'INDFPP'
    else:
        # Order of precedence: AIR > QOL3/4 > no rider
        middle = 'A' if has_air else 'Q' if has_qol3 or has_qol4 else 'P'
        if has_air:
            last = '4' if has_qol4 else ''
        else:
            last = '3' if has_qol3 else '4' if has_qol4 else ''

        if base_product_code == PRODUCT_CODE_FPPTI:
            # FPPTI
            if applicant_type == APPLICANT_CHILD:
                plan_code = 'FPPTID'
            else:
                plan_code = 'FP{}TI{}'.format(middle, last)
        elif base_product_code == PRODUCT_CODE_FPPTIG:
            # FPPTIG
            if applicant_type == APPLICANT_CHILD:
                plan_code = 'FPPTDG'
            else:
                if last != '':
                    last = '/{}'.format(last)
                plan_code = 'FP{}TIG{}'.format(middle, last)
        elif base_product_code == PRODUCT_CODE_FPPTIW:
            # FPPTIW
            if applicant_type == APPLICANT_CHILD:
                plan_code = 'FPPTDW'
            else:
                # if has_qol4 and len(riders) == 1:
                #     # Special case from 2016-05-16 Dell Sally Miller email
                #     # > [FPPTIW] WITH [only] QOL 4% -> FPATW/4
                #     middle = 'A'
                # "I" in code disappears when AIR rider is present
                i = '' if has_air or middle == 'A' else 'I'
                if len(last) > 0:
                    last = '/{}'.format(last)
                plan_code = 'FP{}T{}W{}'.format(middle, i, last)

    plan_code += suffix
    return plan_code
