# -*- coding: utf-8 -*-

from __future__ import unicode_literals

import base64
import datetime
import uuid
from decimal import Decimal

from flask import current_app, render_template

from .models import db
from taa import app
from taa.services.agents.models import Agent
from taa.services import RequiredFeature
from taa.services.products.plan_codes import (get_invalid_plan_code,
                                              get_plan_code)
from taa.services.products.RatePlan import (ApplicantQuery,
                                            ApplicantDemographics,
                                            ApplicantQueryOptions)
from dell import *


# TODO: Move this to /services vvv


class XmlUploadService(object):
    pass

# TODO: add support for additional coverage
# 5Star XML ACORD XML 103 cell C115
# <PlanName>ADD


enrollment_service = RequiredFeature('EnrollmentApplicationService')
agent_service = RequiredFeature('AgentService')
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
    return generate_xml(census.standardized_data, case, template,
                        form_for='employee')


def generate_xml(data, enrollment, template, form_for='employee', pdf_bytes=None):
    vars = get_variables(data, enrollment, form_for, pdf_bytes)
    if vars['enrollee']['coverage']['face_value'] is None:
        return None
    with app.test_request_context():
        return render_template(template, **vars)


def get_variables(data, enrollment, form_for, pdf_bytes):
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
        enrollee['height'] = data['emp_height_inches']
        enrollee['weight'] = data['emp_weight_pounds']
        enrollee['spouse_disabled_6_months'] = YESNO[data.get('sp_disabled_6_months', 'N')]
        enrollee['spouse_treated_6_months'] = YESNO[data.get('sp_treated_6_months', 'N')]
    elif form_for == 'spouse':
        try:
            enrollee = data['spouse']
            enrollee['is_employee'] = False
            enrollee['is_spouse'] = True
            enrollee['is_child'] = False
            enrollee['coverage'] = data['spouse_coverage']
            enrollee['height'] = data['sp_height_inches']
            enrollee['weight'] = data['sp_weight_pounds']
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
            enrollee['height'] = None
            enrollee['weight'] = None
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
        q['answer'] = YESNO_SOH.get(q['answer'].lower() if q['answer'] is not None
                                    else None)

    # TODO: move this to 5Star app config
    try:
        is_debug = current_app.config['DEBUG']
    except:
        is_debug = True
    vars = {
        'meta': {
            'submitted_at': now,
            'submitted_date': submitted_date,
            'submitted_time': submitted_time,
            'trans_type_code': TRANS_TYPE_CODE,
            'trans_type': TRANS_TYPE,
            'unique_id': unique_id,
            'case_id': enrollment.case.id,
            'enrollment_id': enrollment.id,
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
            'company_num': COMPANY_NUMBERS['test']
                                if is_debug
                                else COMPANY_NUMBERS['production'],
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
    vars['employee']['actively_at_work'] = data['actively_at_work'] or 'Y'
    vars['employee']['actively_at_work_code'] = YESNO[data['actively_at_work']]

    policy = get_policy_info(data, form_for, enrollee, enrollment.case)
    vars['policy'].update(policy)

    vars['agents'] = get_agents(data, enrollment, form_for)
    vars['primary_agent'] = vars['agents'][0]
    vars['primary_agent']['signature_state'] = vars['policy']['enroll_state']
    vars['primary_agent']['signature_state_code'] = STATE_CODES[vars['policy']['enroll_state']]['code']
    vars['primary_agent']['signature_date'] = data['time_stamp']
    vars['enrollee']['signature_date'] = data['time_stamp']

    # Add primary beneficiarie(s)
    if enrollee['is_child']:
        # For children, assume employee is primary beneficiary
        vars['enrollee']['beneficiaries'] = {
            'beneficiary': [
                {
                    'birthdate': data['employee']['birthdate'],
                    'ssn': data['employee']['ssn'],
                    'first': data['employee']['first'],
                    'last': data['employee']['last'],
                    'percentage': '100',
                    'relationship':
                        'father' if data['employee']['gender'].lower() == 'male'
                        else 'mother' if data['employee']['gender'].lower() == 'female'
                        else 'parent',
                    # 'relationship': 'child',
                }
            ],
            'contingent_beneficiary': []
        }
    else:
        vars['enrollee']['beneficiaries'] = {}
        for source in ['beneficiary', 'contingent_beneficiary']:
            for index in range(1, 11):
                if source == 'beneficiary' and index > MAX_PRIMARY_BENEFICIARIES:
                    break
                elif source == 'contingent_beneficiary' and index > MAX_CONTINGENT_BENEFICIARIES:
                    break
                prefix = '{}_{}{}'.format(form_for, source, index)
                if source not in vars['enrollee']['beneficiaries']:
                    vars['enrollee']['beneficiaries'][source] = []
                if prefix + '_name' not in data:
                    break
                name = data[prefix + '_name']
                if name.strip() == '':
                    break
                first, last = name.split(' ', 1) if ' ' in name else (name, '')
                vars['enrollee']['beneficiaries'][source].append({
                    'first': first,
                    'last': last,
                    'ssn': data[prefix + '_ssn'],
                    'birthdate': data[prefix + '_dob'],
                    'percentage': data[prefix + '_percentage'],
                    'relationship': data[prefix + '_relationship'].lower(),
                })

    vars['relationships'] = {}
    owner = ('self' if enrollee['is_employee'] else
             'spouse' if enrollee['is_spouse'] else
             'parent' if enrollee['is_child'] else None)
    if owner == 'parent':
        if data['employee']['gender'].lower() == 'male':
            owner = 'father'
        elif data['employee']['gender'].lower() == 'female':
            owner = 'mother'
    vars['relationships']['owner_to_primary'] = RELATIONSHIP_ROLES.get(owner)
    for type_ in ['beneficiary', 'contingent_beneficiary']:
        for beneficiary in vars['enrollee']['beneficiaries'][type_]:
            key = 'owner_to_{}'.format(type_)
            if key not in vars['relationships']:
                vars['relationships'][key] = []
            vars['relationships'][key].append(RELATIONSHIP_ROLES.get(beneficiary['relationship']))

    vars['encoded_pdf'] = (None if pdf_bytes is None
                           else base64.b64encode(pdf_bytes))
    return vars


def get_riders(case, data, form_for, case_override=False):
    riders = []
    if case_override:
        # Get riders from case
        rider_settings = case.product_settings.get('riders', [])
        for rider in rider_settings:
            if rider['is_selected']:
                riders.append(rider['rider_code'])
    else:
        # Get riders from enrollment data
        if form_for == 'employee':
            prefix = 'emp'
        elif form_for == 'spouse':
            prefix = 'sp'
        else:
            return []
        for csv_rider in ['air', 'wp', 'qol3', 'qol4']:
            r = data.get('{}_rider_{}'.format(prefix, csv_rider))
            if r is not None and r.upper() == 'Y':
                riders.append(csv_rider.upper())
    return riders


def make_applicant_query(data, form_for, enrollee, case):
    riders = get_riders(case, data, form_for)
    return ApplicantQuery(
            applicant_type='child' if form_for.startswith('ch') else form_for,
            product_options={'riders': riders},
            demographics=ApplicantDemographics(
                    demographics_object={'applicant_type': form_for}),
            state=enrollee['state'],
            mode=data['payment_mode'],
            rate_options=ApplicantQueryOptions({})
    )


def get_policy_info(data, form_for, enrollee, case):
    policy = {
        'carrier_code': CARRIER_CODE,
        'status_code': POLICY_STATUS_CODE,
        'status': POLICY_STATUS,
        'is_replacement_code': BOOLEAN[data.get('replacing_insurance')],
        'is_replacement': data.get('replacing_insurance', False),
        'replacement_number': data.get('replacement_policy1_number', ''),
        'existing_insurance_code': BOOLEAN[data.get('existing_insurance')],
        'existing_insurance': data.get('existing_insurance'),
        'product_type': data['product_type'],
        'base_product_type': data['product_type'],
        'indicator_code': '1',
        'indicator': 'Base',
        'is_life': True,
    }
    if policy['is_replacement']:
        replacement_type = REPLACEMENT_TYPES['external']
    else:
        replacement_type = REPLACEMENT_TYPES[None]
    policy['replacement_type_code'] = replacement_type['code']
    policy['replacement_type'] = replacement_type['name']
    plan_code = get_plan_code(data['product_type'],
                              make_applicant_query(data, form_for,
                                                    enrollee, case))
    if plan_code is None:
        plan_code = get_invalid_plan_code(data['product_type'])
    policy['product_type'] = plan_code
    if policy['base_product_type'].startswith('FPPTI'):
        policy.update({'product_code': 'Term'})
    else:
        policy.update({'product_code': 'Family Protection'})
    policy['riders'] = get_riders(case, data, form_for)
    return policy


def get_agents(data, enrollment, form_for):
    agents = []
    case = enrollment.case
    if case.agent_splits is not None:
        # Case has agent splits
        # for split in case.agent_splits:
        for split in [s for s in case.agent_splits
                      if s.product_id in [c.product_id
                                          for c in enrollment.coverages
                                          if c.applicant_type == 'employee']]:
            if split.agent is None:
                # Writing agent (null agent) is placeholder for enrolling agent
                agent = Agent.query.filter_by(
                        agent_code=data['agent_code']).one()
            else:
                agent = split.agent
            if split.split_percentage > 0:
                agents.append({
                    'first': agent.first,
                    'last': agent.last,
                    'code': agent.agent_code,
                    'subcode': split.commission_subcount_code,
                    'commission_percent': split.split_percentage,
                })
    else:
        agent = Agent.query.filter_by(agent_code=data['agent_code']).one()
        agents.append({
            'first': agent.first,
            'last': agent.last,
            'code': agent.agent_code,
            'commission_percent': 100,
        })
    return agents
