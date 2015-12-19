# -*- coding: utf-8 -*-

from __future__ import unicode_literals

import base64
import datetime
import uuid
from decimal import Decimal

from flask import render_template

from .models import db
from taa.services import RequiredFeature
from dell import *


# TODO: Move this to /services vvv


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
# 5Star XML ACORD XML 103 cell C115
# <PlanName>ADD


enrollment_service = RequiredFeature('EnrollmentApplicationService')
agent_service = RequiredFeature('AgentService').result
# pdf_service = RequiredFeature('ImagedFormGeneratorService')
# census_service = RequiredFeature('CensusRecordService')


def generate_from_enrollment(enrollment_id, census_record_id,
                             template='xml/base.xml'):
    # pdf_bytes = pdf_service.generate_form_pdf(template_id, enrollment_tabs)
    enrollment = enrollment_service.get(enrollment_id)
    census = enrollment.census_record
    case = enrollment.case
    agent = agent_service.get(enrollment.agent_id)
    xmls = {
        'employee': None,
        'spouse': None,
        'children': [],
    }
    return generate_xml(census.standardized_data, template, agent, form_for='employee')


def generate_xml(data, agents, template, form_for='employee', pdf_bytes=None):
    vars = get_variables(data, agents, form_for, pdf_bytes)

    # # Update based on internal data
    # vars['policy']['payment_method'] = PAYMENT_METHODS[vars['policy']['payment_method_code']]
    # vars['policy']['eft_account_type'] = PAYMENT_ACCOUNT_TYPES[vars['policy']['eft_account_type_code']]
    # vars['policy']['replacement_type'] = REPLACEMENT_TYPES[vars['policy']['replacement_type_code']]
    # # TODO: Set properly  vv
    # vars['policy']['payment_mode_code'] = PAYMENT_MODE.get('12')
    # vars['policy']['payment_mode'] = vars['policy']['payment_mode_code']
    # vars['policy']['qualified_plan_type'] = QUALIFYING_PLAN_TYPES[vars['life']['qualified_plan_type_code']]
    # vars['policy']['coverage_indicator'] = COVERAGE_INDICATOR_CODES[vars['life']['coverage_indicator_code']]
    # for sig in vars['application_info']['signatures']:
    #     sig['role'] = PARTICIPANT_ROLE_CODES[sig['role_code']]
    #     sig['ok'] = bool(sig['ok_code'])
    # for _ in range(vars['enrollee']['num_children']):
    #     vars['policy']['participants'].append('4')
    # vars['enrollee']['employee']['state'] = STATE_CODES[vars['enrollee']['employee']['state_code']]
    # vars['enrollee']['employee']['birth_jurisdiction'] = STATE_CODES[vars['enrollee']['employee']['birth_jurisdiction_code']]
    # vars['enrollee']['employee']['smoker'] = SMOKER_CODES[vars['enrollee']['employee']['smoker_code']]
    # vars['enrollee']['employee']['existing_insurance'] = bool(vars['enrollee']['employee']['existing_insurance'])
    # vars['agent']['existing_insurance'] = bool(vars['agent']['existing_insurance'])
    if vars['enrollee']['coverage']['face_value'] is None:
        return None
    return render_template(template, **vars)


def get_variables(data, agents, form_for, pdf_bytes):
    """
    Get the variables needed to populate Dell's ACORD XML template

    :param data: Enrollee data
    :type: dict
    :return: Variables to pass to XML template
    :rtype: dict
    """
    unique_id = uuid.uuid4()
    now = datetime.datetime.now()
    submitted_date = now.date().isoformat()
    submitted_time = now.time().isoformat().split('.', 1)[0]
    if form_for == 'employee':
        enrollee = data['employee']
        enrollee['is_employee'] = True
        enrollee['is_spouse'] = False
        enrollee['is_child'] = False
        enrollee['coverage'] = data['employee_coverage']
    elif form_for == 'spouse':
        try:
            enrollee = data['spouse']
            enrollee['is_employee'] = False
            enrollee['is_spouse'] = True
            enrollee['is_child'] = False
            enrollee['coverage'] = data['spouse_coverage']
        except KeyError:
            raise ValueError(
                "Attempted to enroll for spouse, but employee has no "
                "spouse on record")
    elif form_for.startswith('child'):
        child_index = int(form_for[len('child'):])
        try:
            enrollee = data['children'][child_index]
            enrollee['is_employee'] = False
            enrollee['is_spouse'] = False
            enrollee['is_child'] = True
            enrollee['coverage'] = data['child_coverages'][child_index]
        except (KeyError, IndexError):
            raise ValueError(
                "Attempted to enroll for child #{}, but employee has no such "
                "child on record".format(child_index+1))
    # Ensure enrollee has location set (assume employee info is always valid)
    if form_for != 'employee':
        for key in ('address1', 'address2', 'city', 'state', 'zip'):
            if len(enrollee[key]) == 0:
                enrollee[key] = data['employee'][key]
    # Set Dell-specific "tc" codes
    gender = GENDER_CODES[enrollee['gender'].lower()]
    enrollee['gender_code'] = gender['code']
    enrollee['gender'] = gender['name']
    state = STATE_CODES[enrollee['state'].upper()]
    enrollee['state_code'] = state['code']
    smoker = SMOKER_CODES[enrollee['is_smoker']]
    enrollee['smoker_code'] = smoker['code']
    enrollee['smoker'] = smoker['name']
    for q in enrollee['soh_questions']:
        q['answer'] = YESNO.get(q['answer'].lower() if q['answer'] is not None
                                else None)

    vars = {
        'meta': {
            'submitted_at': now,
            'submitted_date': submitted_date,
            'submitted_time': submitted_time,
            'trans_type_code': TRANS_TYPE_CODE,
            'trans_type': TRANS_TYPE,
            'unique_id': unique_id,
        },
        'auth': {
            'login': AUTH_LOGIN,
            'crypt_type': CRYPT_TYPE,
        },
        'vendor': {
            'code': 'xxx',
            'name': 'VendorName',
            'app_name': 'appname',
        },
        'olife': {
            'vendor_code': 'xxx',
            'extension_code': 'xxxx',
            'company_num': '67',
            'app_code': 'New Enrollee',
        },
        'case': data['case'],
        'enrollee': enrollee,
        'employee': data['employee'],
        'spouse': data.get('spouse'),
        'children': data.get('children'),
        'policy': {
            'enroll_city': data['enrollCity'],
            'enroll_state': data['enrollState'],
            'enroll_state_code': STATE_CODES[data['enrollState']]['code'],
            'product_type': data['product_type'],
            'payment_mode_code': PAYMENT_MODE[str(data['payment_mode'])],
            'payment_mode': data['payment_mode_text'],
            'pin': 12345,
            'payment': {
                'method_code': '5',
                'method': PAYMENT_METHODS['5'],
                # 'account_number': 873456736,
                # 'routing_number': 82347864,
                # 'account_type_code': '2',
                # 'account_type': 'Checking Account',
                # 'draft_day': '01',
            },
        },
    }

    state = STATE_CODES[vars['employee']['state'].upper()]
    vars['employee']['state_code'] = state['code']

    vars['employee']['hire_date'] = data['identityToken']
    vars['agents'] = []
    for agent in agents:
        vars['agents'].append({
            'first': agent.first,
            'last': agent.last,
            'code': agent.agent_code,
            'commission_percent': 100 / len(agents),
        })

    policy = get_policy_info(data, form_for)
    vars['policy'].update(policy)

    vars['primary_agent'] = vars['agents'][0]
    vars['primary_agent']['signature_state'] = vars['policy']['enroll_state']
    vars['primary_agent']['signature_date'] = data['time_stamp']
    vars['enrollee']['signature_date'] = data['time_stamp']

    # Add primary beneficiarie(s)
    if enrollee['is_child']:
        # For children, assume employee is primary beneficiary
        vars['enrollee']['beneficiaries'] = {
            'primary': [
                {
                    'birthdate': data['employee']['birthdate'],
                    'ssn': data['employee']['ssn'],
                    'first': data['employee']['first'],
                    'last': data['employee']['last'],
                    'percentage': '100',
                    'relationship': 'father' if data['employee']['gender'].lower() == 'male' else 'mother',
                }
            ],
            'contingent': []
        }
    else:
        vars['enrollee']['beneficiaries'] = {
            'primary': [
                {
                    'birthdate': data[form_for + '_beneficiary1_dob'],
                    'ssn': data[form_for + '_beneficiary1_ssn'],
                    'first': data[form_for + '_beneficiary1_name'].split(' ', 1)[0],
                    'last': data[form_for + '_beneficiary1_name'].split(' ', 1)[1],
                    'percentage': data[form_for + '_beneficiary1_percentage'],
                    'relationship': data[form_for + '_beneficiary1_relationship'].lower(),
                }
            ],
            'contingent': [
                {
                    'birthdate': data[form_for + '_contingent_beneficiary1_dob'],
                    'ssn': data[form_for + '_contingent_beneficiary1_ssn'],
                    'first': data[form_for + '_contingent_beneficiary1_name'].split(' ', 1)[0],
                    'last': data[form_for + '_contingent_beneficiary1_name'].split(' ', 1)[1],
                    'percentage': data[form_for + '_contingent_beneficiary1_percentage'],
                    'relationship': data[form_for + '_contingent_beneficiary1_relationship'].lower(),
                }
            ],
        }

    vars['relationships'] = {}
    owner = 'self' if enrollee['is_employee'] else \
            'spouse' if enrollee['is_spouse'] else \
            'child' if enrollee['is_child'] else None
    vars['relationships']['owner_to_primary'] = RELATIONSHIP_ROLES.get(owner)
    for type_ in ['primary', 'contingent']:
        for beneficiary in vars['enrollee']['beneficiaries'][type_]:
            key = 'owner_to_{}_beneficiary'.format(type_)
            if key not in vars['relationships']:
                vars['relationships'][key] = []
            vars['relationships'][key].append(RELATIONSHIP_ROLES.get(beneficiary['relationship']))

    vars['encoded_pdf'] = None if pdf_bytes is None else \
        base64.b64encode(pdf_bytes)

    return vars


def get_policy_info(data, form_for):
    policy = {
        'carrier_code': CARRIER_CODE,
        'status_code': POLICY_STATUS_CODE,
        'status': POLICY_STATUS,
        'is_replacement_code': BOOLEAN[data.get('replacing_insurance')],
        'is_replacement': data.get('replacing_insurance', False),
        'existing_insurance_code': BOOLEAN[data.get('existing_insurance')],
        'existing_insurance': data.get('existing_insurance'),
        'product_type': data['product_type'],
        'indicator_code': '1',
        'indicator': 'Base',
        'is_life': True,
    }
    if form_for.startswith('child'):
        policy['product_type'] += 'D'
    if policy['is_replacement']:
        replacement_type = REPLACEMENT_TYPES['internal']
    else:
        replacement_type = REPLACEMENT_TYPES[None]
    policy['replacement_type_code'] = replacement_type['code']
    policy['replacement_type'] = replacement_type['name']
    # TODO: need to determine logic for plan mapping
    if data['product_type'] == 'FPPTI':
        policy.update({'product_code': 'Term'})
    else:
        policy.update({'product_code': 'Family Protection'})
    return policy
