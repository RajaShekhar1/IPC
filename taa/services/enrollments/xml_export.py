# -*- coding: utf-8 -*-

import base64
import datetime
import uuid
from decimal import Decimal

from flask import render_template

from .models import db
from taa.services import RequiredFeature
import dell


# TODO: Move this to /services vvv

# Endpoints
ENDPOINT_PROD = 'https://extranetapps.tagtpa.com/TxLifeImport/TxLife.asmx'
ENDPOINT_DEV = 'https://extranetapps-mo.tagtpa.com/TxLifeImport/TxLife.asmx'

# TxLife.TxLifeResponse.TransResult.ResultCode
RESULT_CODES = {
    '1': 'Success',
    '90001': 'TxLife.OLifEExtension.CompanyNumber is missing',
    '90031': 'TxLife.TXLifeRequest.TransRefGUID is missing',
    '90032': 'TxLife.TXLifeRequest.TransRefGUID is duplicate',
    '99001': 'TxLife.OLifEExtension.CompanyNumber is invalid',
    '99003': 'XML does not pass well-formed check',
    '99099': 'Application form is missing',
    '99101': 'File extension missing for one or more forms',
    '99102': 'One or more forms is not correctly encoded',
    '99103': 'Dell internal network error',
    '': 'Other application error',
}
# In the Response:
# /TxLife/TxLifeResponse/TransRefGUID
# should be equal to
# /TxLife/TXLifeRequest/TransRefGUID
# /TxLife/TxLifeResponse/TransResult/ResultCode[@tc = 1]
# Signifies the item was successfully processed.  So the TransRefGUID 	used in this transmission should not be used again.
# Any @tc value should be regarded as an error.  The transmission may be 	sent again at a later time.
class XmlUploadService(object):
    pass

def wsdl_test():
    from pysimplesoap.client import SoapClient
    import logging
    logger = logging.getLogger(__name__)
    logger.addHandler(logging.NullHandler())
    c = SoapClient(wsdl='file:///vagrant/Artifacts/TAA3.0/'
                        '5Star XML Info (rev 052915)/TxLife WSDL.txt')
    cc = SoapClient(location='https://extranetapps-mo.tagtpa.com/TxLifeImport/TxLife.asmx')
    cc.call('TXlifeProcessor')

"""
OLD
# https://extranetapps-mo.perotsystemsbps.net/TxLifeImport/TxLife.asmx
have WSDL for:
https://extranetapps.perotsystemsbps.net/TxLifeImport/TxLife.asmx

NEW
need WSDL for:
https://extranetapps-mo.tagtpa.com/TxLifeImport/TxLife.asmx
https://extranetapps.tagtpa.com/TxLifeImport/TxLife.asmx
"""


# TODO: add support for additional coverage
# TODO: add support for spouse coverage
# TODO: add support for child coverage
# 5Star XML ACORD XML 103 cell C115
# <PlanName>ADD


enrollment_service = RequiredFeature('EnrollmentApplicationService')
agent_service = RequiredFeature('AgentService').result
# pdf_service = RequiredFeature('ImagedFormGeneratorService')
# census_service = RequiredFeature('CensusRecordService')


VENDOR_NAME = '5Star'      # TODO: GET FROM DELL
VENDOR_CODE = '9999'       # TODO: GET FROM DELL
CRYPT_TYPE = 'NONE'        # TODO: GET FROM DELL
COMPANY_NUMBERS = {
    'production': '67',    # 5Star Production
    'test': '87',          # 5Star Model Office
}
TRANS_TYPE_CODE = '103'
PRODUCT_CODES = {
    'Term',
    'Critical Illness',
}
PLAN_NAMES = {
    'VOL01',
    'BAS01',
    'VOL02',
    'BAS02',
    'FPPTI',
    'FPPTI/MD',
    'FPPTIA',
    'FPPTI/UT',
    'INDFPP',
    'INDFPP/MD',
    'INDFPP/UT',
    'GRPFRE',
    'CRTIL',
    'CRTILE'
}
POLICY_STATUS = 'Proposed'
POLICY_STATUS_CODE = '12'
CARRIER_CODE = '5STAR'
ENROLLEE_TYPES = {
    'New Enrollee',
    'Late Enrollee',
}
PAYMENT_MODES = {
    '1': 'Annual',
    '2': 'Semiannual',
    '3': 'Quarterly',
    '4': 'Monthly',
    '12': 'Every 4 weeks',
    '5': 'Semi-Monthly',
    '7': 'Bi-Weekly',
    '6': 'Weekly',
}
PAYMENT_METHODS = {
    '2': 'Regular Billing',
    '5': 'Group Bill',
    '7': 'Electronic Funds Transfer',
}
PAYMENT_ACCOUNT_TYPES = {
    '1': 'Savings Account',
    '2': 'Checking Account',
}
REPLACEMENT_TYPES = {
    '1': 'None',
    '2': 'Internal',
    '3': 'External',
}
TRANS_TYPE = 'New Business Submission'
RELATIONSHIP_ROLES = {
    'As Approved': '77',
    'Aunt': '100001',
    'Brother': '100002',
    'Business': '156',
    'Child': '2',
    'Civil Union': '100003',
    'Corporation': '100004',
    'Custodian': '57',
    'Domestic Partner': '15',
    'Employee': '6',
    'Employer': '7',
    'Estate': '100005',
    'Ex-spouse': '100006',
    'Father': '100007',
    'Father-in-law': '100008',
    'Fiancé': '111',
    'Grandchild': '93',
    'Grandparent': '92',
    'Guardian': '27',
    'Mother': '100009',
    'Mother-in-law': '100010',
    'Nephew': '100011',
    'Niece': '100012',
    'Other': '2147483647',
    'Parent': '3',
    'Self': '168',
    'Sibling': '4',
    'Sister': '100013',
    'Spouse': '1',
    'Trust': '100014',
    'Uncle': '100015',
}
QUALIFYING_PLAN_TYPES = {
    '1': 'Non-Qualified',
}
COVERAGE_INDICATOR_CODES = {
    '1': 'Base',
}
PARTICIPANT_ROLE_CODES = {
    '1': 'Primary Insured',
    '18': 'Owner',
    '12': 'Payor',
    '15': 'Primary Agent',
    '7': 'Beneficiary - Primary',
    '9': 'Beneficiary - Contingent',
    '5': 'Spouse',
    '4': 'Child',
    '31': 'Third Party Recipient',
}
RIDER_CODES = {
    'employee': 'CRTILE',
    'spouse': 'CRTIL/SP',
    'child': 'CRTIL/CH',
}
GENDER_CODES = {
    '1': 'Male',
    '2': 'Female',
}
STATE_CODES = {
    '1': 'AL',
    '38': 'NC',
}
STATE_CODES_REVERSED = dict(zip(STATE_CODES.values(), STATE_CODES.keys()))
SMOKER_CODES = {
    '1': 'Never used tobacco in any form.',
    '3': 'Smoker',
}


def xml_generate(enrollment_id, census_record_id, pdf_bytes):
    # pdf_bytes = pdf_service.generate_form_pdf(template_id, enrollment_tabs)
    enrollment = enrollment_service.get(enrollment_id)
    census = enrollment.census_record
    case = enrollment.case
    agent = agent_service.get(enrollment.agent_id)
    employee = census.standardized_data['employee']
    spouse = census.standardized_data.get('spouse')
    children = census.standardized_data.get('children', [])
    num_children = len(children)
    has_spouse = spouse is not None
    has_children = num_children > 0

    now = datetime.datetime.now()
    now_date = now.date().isoformat()
    now_time = now.time().isoformat().split('.')[0]
    is_replacement = True  # TODO: Set properly

    vars = {
        'agent': agent,
        'vendor': {
            'name': VENDOR_NAME,
            'code': VENDOR_CODE,
            'crypt_type': CRYPT_TYPE,
            'appname': 'iGo Forms',      # TODO: GET FROM DELL
            'extension_code': '6778',    # TODO: GET FROM DELL
            'company_number': COMPANY_NUMBERS['test'],
        },
        'enrollee': {
            'group_life_app_code': 'New Enrollee',
            'coverage_effective_date': '2016-01-01',
            'employee': {
                'govt_id': employee['ssn'],
                'first_name': employee['first'],
                'last_name': employee['last'],
                'gender_code': dell.gender.get(employee['gender']),
                'birth_date': '1',
                'birth_jurisdiction_code': '',
                'smoker_code': '',
                'height': employee['height'],
                'weight': employee['weight'],
                'address1': employee['address1'],
                'address2': employee['address2'],
                'city': employee['city'],
                'state_code': employee['state'],
                'zip': employee['zip'],
                'phone': employee['phone'],
                'email': employee['email'],
                'employer': '',
                'employee_id': '',
                'employment_date': '',
                'salary': '',
                'employer_govt_id': '',
                'employer_id': '',
                'existing_insurance_code': '0',
            },
            'num_children': num_children,
        },
        'life_request': {
            'tx_ref_guid': str(uuid.uuid4()),
            'trans_type_code': TRANS_TYPE_CODE,
            'trans_type': TRANS_TYPE,
            'exe_date': now_date,
            'exe_time': now_time,
            'source_created_date': now_date,
            'source_created_time': now_time,
        },
        'policy': {
            'is_life': True,                    # TODO: Set properly
            'product_code': PRODUCT_CODES[0],   # TODO: Set properly
            'carrier_code': CARRIER_CODE,
            'plan_name': 'FPPTI',
            'status_code': POLICY_STATUS_CODE,
            'status': POLICY_STATUS,
            'payment_method_code': '5',
            'payment_amount': Decimal('1234.56'),        # TODO: Set properly
            # For electronic funds transfer (not currently used)
            'eft_account_number': '1' * 20,
            'eft_routing_number': '1' * 9,
            'eft_account_type_code': '2',
            'eft_draft_day': '01',
            'is_replacement_code': '1' if is_replacement else '0',
            'is_replacement': str(is_replacement),
            'replacement_type_code': '1' if is_replacement else '0',
            'coverage_amount': Decimal('34345'),
            'coverage_units': 100,
            'modal_premium_amount': Decimal('20'),
            'level_premium_period': '10',
            # 'coverage_option': {
            #     'plan_name': None,
            #     'option_amount': None,
            #     'option_units': None,
            #     'modal_premium_amount': None,
            # }
            'participant_codes': [
                '1',
                '18',
            ],
        },
        'life': {
            'qualified_plan_type_code': 1,
            'coverage_indicator': 1,
            # '': '',
        },
        'disability_health': {
        },
        'riders': [
            {
                'code': RIDER_CODES['employee'],
                'total_amount': Decimal('100000.00'),
                'units': 100,
                'payment_amount': Decimal('20.00'),
                'participant_code': '1',
            },
            {
                'code': RIDER_CODES['spouse'],
                'total_amount': Decimal('100000.00'),
                'units': 100,
                'payment_amount': Decimal('20.00'),
                'participant_code': '5',
            },
            {
                'code': RIDER_CODES['child'],
                'total_amount': Decimal('100000.00'),
                'units': 100,
                'payment_amount': Decimal('20.00'),
                'participant_code': '4',
                'child_num': 1,
            },
        ],
        'application_info': {
            'tracking_id': enrollment_tabs['id'].value,
            'signatures': [
                {
                    'role_code': '15',
                    'date': '2016-01-01',
                    'state_code': '38',
                    'ok_code': '1',
                },
            ],
        },

        'attachment': {
            'created_date': now_date,
            'data': base64.b64encode(pdf_bytes),
        }
    }
    # Spouse
    if has_spouse:
        vars['enrollee'].update({
            'spouse': {
                'govt_id': '',
                'first_name': '',
                'last_name': '',
                'gender_code': '1',
                'birth_date': '1',
                'birth_jurisdiction_code': '',
                'smoker_code': '',
                'height': '',
                'weight': '',
                'address1': spouse['address1'],
                'address2': spouse['address2'],
                'city': '',
                'state_code': '',
                'zip': '',
                'phone': '',
                'email': '',
                'employer': '',
                'employee_id': '',
                'employment_date': '',
                'salary': '',
                'employer_govt_id': '',
                'employer_id': '',
                'existing_insurance_code': '0',
            }
        })
    # Children
    if has_children:
        vars['enrollee']['children'] = []
        for child in children:
            vars['enrollee']['children'].append(
                {
                    'govt_id': '',
                    'first_name': '',
                    'last_name': '',
                    'gender_code': '1',
                    'birth_date': '1',
                    'birth_jurisdiction_code': '',
                    'smoker_code': '',
                    'height': '',
                    'weight': '',
                    'address1': employee['address1'],
                    'address2': employee['address2'],
                    'city': '',
                    'state_code': '',
                    'zip': '',
                    'phone': '',
                    'email': '',
                    'employer': '',
                    'employee_id': '',
                    'employment_date': '',
                    'salary': '',
                    'employer_govt_id': '',
                    'employer_id': '',
                    'existing_insurance_code': '0',
                }
            )
    # Update based on internal data
    vars['policy']['payment_method'] = PAYMENT_METHODS[vars['policy']['payment_method_code']]
    vars['policy']['eft_account_type'] = PAYMENT_ACCOUNT_TYPES[vars['policy']['eft_account_type_code']]
    vars['policy']['replacement_type'] = REPLACEMENT_TYPES[vars['policy']['replacement_type_code']]
    # TODO: Set properly  vv
    vars['policy']['payment_mode_code'] = dell.payment_mode.get('12')
    vars['policy']['payment_mode'] = vars['policy']['payment_mode_code']
    vars['policy']['qualified_plan_type'] = QUALIFYING_PLAN_TYPES[vars['life']['qualified_plan_type_code']]
    vars['policy']['coverage_indicator'] = COVERAGE_INDICATOR_CODES[vars['life']['coverage_indicator_code']]
    for sig in vars['application_info']['signatures']:
        sig['role'] = PARTICIPANT_ROLE_CODES[sig['role_code']]
        sig['ok'] = bool(sig['ok_code'])
    for _ in range(vars['enrollee']['num_children']):
        vars['policy']['participants'].append('4')
    vars['enrollee']['employee']['state'] = STATE_CODES[vars['enrollee']['employee']['state_code']]
    vars['enrollee']['employee']['birth_jurisdiction'] = STATE_CODES[vars['enrollee']['employee']['birth_jurisdiction_code']]
    vars['enrollee']['employee']['smoker'] = SMOKER_CODES[vars['enrollee']['employee']['smoker_code']]
    vars['enrollee']['employee']['existing_insurance'] = bool(vars['enrollee']['employee']['existing_insurance'])
    vars['agent']['existing_insurance'] = bool(vars['agent']['existing_insurance'])
    return render_template('enrollment/dell_export.xml', **vars)
