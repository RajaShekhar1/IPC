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



def get_plan_code(base_product_code, applicant_query):
    applicant_type = applicant_query.get_applicant_type()
    riders = applicant_query.get_riders()
    state = applicant_query.state

    #if base_product_code != PRODUCT_CODE_FPPTI and base_product_code != PRODUCT_CODE_FPPTIB and base_product_code != PRODUCT_CODE_FPPTIG and base_product_code != PRODUCT_CODE_FPPTIY and base_product_code != PRODUCT_CODE_FPPTIW:
    #    return ''

    if applicant_type == APPLICANT_CHILD:
        plan_code = get_child_plan_code(base_product_code, state)
    else:
        plan_code = get_adult_plan_code(applicant_type, base_product_code, riders, state)

    plan_code = add_state_modification(base_product_code, plan_code, state)

    return plan_code


def add_state_modification(base_product_code, plan_code, state):
    if (plan_code != None and len(plan_code) > 0 and (state == 'MD' or state == 'UT') and
                base_product_code in [PRODUCT_CODE_FPPTI, PRODUCT_CODE_FPPCI]):
        if plan_code[-1] != '/':
            plan_code += ' '
        plan_code += state

    return plan_code


def get_adult_plan_code(applicant_type, base_product_code, riders, state):

    plan_code = base_product_code

    if base_product_code == PRODUCT_CODE_FPPCI:
        plan_code = 'INDFPP'
        if state == 'MD' or state == 'UT':
            plan_code += '/'
        return plan_code

    if len(riders) == 1:
        if 'AIR' in riders:
            plan_code = get_air_code(base_product_code)
            if state == 'MD' or state == 'UT':
                plan_code += '/'
        elif 'WP' in riders:
            plan_code = base_product_code
        elif 'QOL3' in riders or 'QOL4' in riders:
            plan_code = get_qol_code(base_product_code, riders[0], applicant_type, state)
    elif len(riders) == 2:
        if 'AIR' in riders and 'WP' in riders:
            # AIR and WP rider combination not allowed
            plan_code = ''
        elif 'AIR' in riders and 'QOL3' in riders:
            plan_code = get_air_code(base_product_code)
            if state == 'MD' or state == 'UT':
                plan_code += '/'
        elif 'AIR' in riders and 'QOL4' in riders:
            plan_code = get_air_code(base_product_code)
            if base_product_code != PRODUCT_CODE_FPPTI and base_product_code != PRODUCT_CODE_FPPTIG and state != 'MD' and state != 'UT':
                plan_code += '/'
            if plan_code != '':
                plan_code += '4'
            if state == 'MD' or state == 'UT':
                plan_code += '/'
        elif 'WP' in riders and 'QOL3' in riders:
            plan_code = get_qol_code(base_product_code, 'QOL3', applicant_type, state)
        elif 'WP' in riders and 'QOL4' in riders:
            plan_code = get_qol_code(base_product_code, 'QOL4', applicant_type, state)

    return plan_code


def get_child_plan_code(base_product_code, state_code):
    if base_product_code == PRODUCT_CODE_FPPCI:
        plan_code = 'INDFPD'
        if state_code in ['UT', 'MD']:
            plan_code += '/'
    else:
        plan_code = 'FPPT'

        if base_product_code[-1] != 'I':
            plan_code += 'ID' + base_product_code[-1]
        else:
            plan_code += 'ID'
    return plan_code


def get_air_code(base_product_code):
    plan_code = 'FPAT'
    if base_product_code == PRODUCT_CODE_FPPTIG:
        plan_code = ''
    else:
        plan_code += base_product_code[-1]
    return plan_code


def get_qol_code(base_product_code, rider, applicant_type, state):
    plan_code = 'FPQTI'
    if base_product_code != PRODUCT_CODE_FPPTI:
        plan_code += base_product_code[-1]
    if base_product_code != PRODUCT_CODE_FPPTI or applicant_type == APPLICANT_SPOUSE and state != 'MD' and state != 'UT':
        plan_code += '/'
    plan_code += rider[-1]
    if state == 'MD' or state == 'UT':
        plan_code += '/'
    return plan_code
