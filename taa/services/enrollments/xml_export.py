# -*- coding: utf-8 -*-

from __future__ import unicode_literals

import base64
import datetime
import uuid

import re
from flask import current_app, render_template
from taa.services.enrollments.enrollment_application import EnrollmentApplicationService

from taa import app
from taa.services import RequiredFeature
from taa.services.products.plan_codes import (get_invalid_plan_code,
                                              get_plan_code)
from taa.services.products.RatePlan import (ApplicantQuery,
                                            ApplicantDemographics,
                                            ApplicantQueryOptions)
from taa.services.enrollments.dell import *


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



def generate_xml(data, enrollment, template, applicant_type='employee', pdf_bytes=None):
    vars = get_variables(data, enrollment, applicant_type, pdf_bytes)
    if vars['enrollee']['coverage']['face_value'] is None:
        return None
    with app.test_request_context():
        return render_template(template, **vars)


def normalize_phone(phone_val):
    if isinstance(phone_val, dict):
        # Already processed this phone

        return phone_val
    if not phone_val:
        area_code = ''
        dial_num = ''
    else:

        phone_val = phone_val.strip()

        # Replace any non alpha-numeric characters (if they type a message, we'll let it go through)
        phone_val = re.sub('[^0-9a-zA-Z]+', '', phone_val)

        if len(phone_val) > 3:
            area_code = phone_val[:3]
            dial_num = phone_val[3:]
        else:
            area_code = ''
            dial_num = phone_val

    return {'area_code': area_code, 'dial_num': dial_num}



def get_variables(data, enrollment, applicant_type, pdf_bytes):
    """
    Get the variables needed to populate Dell's ACORD XML template
    """
    unique_id = uuid.uuid4()
    now = datetime.datetime.now()
    submitted_date = now.date().isoformat()
    submitted_time = now.time().isoformat().split('.', 1)[0]
    
    effective_date = data.get_effective_date().strftime('%Y-%m-%d')

    if applicant_type == 'employee':
        enrollee = data['employee']
        enrollee['is_employee'] = True
        enrollee['is_spouse'] = False
        enrollee['is_child'] = False
        enrollee['coverage'] = data['employee_coverage']
        # enrollee['height'] = data['emp_height_inches']
        # enrollee['weight'] = data['emp_weight_pounds']
        
    elif applicant_type == 'spouse':
        try:
            enrollee = data['spouse']
            enrollee['is_employee'] = False
            enrollee['is_spouse'] = True
            enrollee['is_child'] = False
            enrollee['coverage'] = data['spouse_coverage']
            # enrollee['height'] = data['sp_height_inches']
            # enrollee['weight'] = data['sp_weight_pounds']
            enrollee['spouse_disabled_6_months'] = YESNO[data.get('sp_disabled_6_months', 'N')]
            enrollee['spouse_treated_6_months'] = YESNO[data.get('sp_treated_6_months', 'N')]
        except KeyError:
            raise ValueError(
                "Attempted to enroll for spouse, but employee has no "
                "spouse on record")
    elif applicant_type.startswith('child'):
        child_index = int(applicant_type[len('child'):])
        try:
            enrollee = data['children'][child_index]
            enrollee['is_employee'] = False
            enrollee['is_spouse'] = False
            enrollee['is_child'] = True
            enrollee['coverage'] = data['child_coverages'][child_index]
            # enrollee['height'] = None
            # enrollee['weight'] = None
        except (KeyError, IndexError):
            raise ValueError(
                "Attempted to enroll for child #{}, but employee has no such "
                "child on record".format(child_index+1))
    # Ensure enrollee has location set (assume employee info is always valid)
    if applicant_type != 'employee':
        for key in ('address1', 'address2', 'city', 'state', 'zip'):
            if not enrollee.get(key, '') or len(enrollee.get(key, '')) == 0:
                enrollee[key] = data['employee'][key]



    # Set Dell-specific "tc" codes
    gender = GENDER_CODES[enrollee['gender'].lower()] if enrollee['gender'] else dict(code='', name='')
    enrollee['gender_code'] = gender['code']
    enrollee['gender'] = gender['name']
    state = STATE_CODES[enrollee['state'].upper()]
    enrollee['state_code'] = state['code']
    smoker = SMOKER_CODES[enrollee['is_smoker']]
    enrollee['smoker_code'] = smoker['code']
    enrollee['smoker'] = smoker['name']
    if applicant_type == 'employee':
        #key = 'employee_soh_questions'
        questions = data.get_employee_soh_questions()
    elif applicant_type == 'spouse':
        #key = 'spouse_soh_questions'
        questions = data.get_spouse_soh_questions()
    elif applicant_type.startswith('child'):
        child_index = int(applicant_type[len('child'):])
        #key = 'children_soh_questions'[child_index]
        questions = data.get_child_soh_questions(child_index)
    else:
        questions = []
        
    # Filter out any questions that are not part of the main QOH section.
    questions = [q for q in questions if not q.get('is_spouse_only') and not q.get('is_employee_only')]
    
    # for q in enrollee['soh_questions']:
    for q in questions:
        q['answer'] = YESNO_SOH.get(q['answer'].lower() if q['answer'] is not None else None)
    
    
    # Bank info
    if data.has_bank_draft_info() and not data.requires_paylogix_export():
        # We have the bank info and we aren't sending it to Paylogix
        from taa.services.docusign.templates.fpp_bank_draft import FPPBankDraftFormTemplate
        bank_draft_form = FPPBankDraftFormTemplate([], data, False)
        payment = {
            'method_code': '7',
            'method': PAYMENT_METHODS['7'],
            'account_number': bank_draft_form.get_bank_account_number(),
            'routing_number': bank_draft_form.get_bank_routing_number(),
            'draft_day': bank_draft_form.get_draft_day(),
        }
        if bank_draft_form.get_bank_account_type().lower() == "checking":
            payment['account_type_code'] = '2'
            payment['account_type'] = PAYMENT_ACCOUNT_TYPES['2']
        else:
            payment['account_type_code'] = '1'
            payment['account_type'] = PAYMENT_ACCOUNT_TYPES['1']
    else:
        payment = {
            'method_code': '5',
            'method': PAYMENT_METHODS['5'],
        }
        
    try:
        is_debug = current_app.config['IS_STP_DEBUG']
    except:
        is_debug = True
    vars = {
        'applicant_type': applicant_type,
        'meta': {
            'submitted_at': now,
            'effective_date': effective_date,
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
            # This is in the spec, looks weird though.
            'code': 'xxx',
            'name': 'VendorName',
            'app_name': 'appname',
        },
        'olife': {
            # This is in the spec too.
            'vendor_code': 'xxx',
            'extension_code': 'xxxx',
            'company_num': COMPANY_NUMBERS['test']
                                if is_debug
                                else COMPANY_NUMBERS['production'],
            'app_code': 'New Enrollee',
        },
        'case': data['case'],
        'enrollee': enrollee,
        'soh_questions': questions,
        'employee': data['employee'],
        'spouse': data.get('spouse'),
        'children': data.get('children'),
        'policy': {
            # FIXME: These values are updated below - see if we can just call get_policy_info here?
            'enroll_city': data['enrollCity'],
            'enroll_state': data['enrollState'],
            'enroll_state_code': STATE_CODES[data['enrollState']]['code'],
            'payment_mode_code': PAYMENT_MODE[str(data['payment_mode'])],
            'payment_mode': data['payment_mode_text'],
            # TODO:  - SIGNATURE PIN - add in once signing ceremony?
            'pin': 12345,
            'payment': payment,
        },
    }

    state = STATE_CODES[vars['employee']['state'].upper()]
    vars['employee']['state_code'] = state['code']

    vars['employee']['hire_date'] = data['identityToken']
    vars['employee']['actively_at_work'] = data.get_actively_at_work()# or 'Y'
    vars['employee']['actively_at_work_code'] = YESNO[data.get_actively_at_work()]

    policy = get_policy_info(data, applicant_type, enrollee, enrollment.case)
    vars['policy'].update(policy)

    vars['agents'] = get_agents(data, enrollment)
    vars['primary_agent'] = vars['agents'][0]
    vars['primary_agent']['signature_state'] = vars['policy']['enroll_state']
    vars['primary_agent']['signature_state_code'] = STATE_CODES[vars['policy']['enroll_state']]['code']
    vars['primary_agent']['signature_date'] = enrollment.signature_time
    vars['enrollee']['signature_date'] = enrollment.signature_time

    # Normalize phone numbers
    if 'phone' in vars['enrollee']:
        vars['enrollee']['phone'] = normalize_phone(vars['enrollee']['phone'])

    if 'phone' in vars['employee']:
        vars['employee']['phone'] = normalize_phone(vars['employee']['phone'])

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
                }
            ],
            'contingent_beneficiary': []
        }
    else:
        vars['enrollee']['beneficiaries'] = {}
        all_benes = data.get_beneficiary_data()
        for k, benes in iter(all_benes.items()):
            source = ('beneficiary' if k.endswith('_primary')
                      else 'contingent_beneficiary')
            if source not in vars['enrollee']['beneficiaries']:
                vars['enrollee']['beneficiaries'][source] = []
            for bene in benes:
                if applicant_type not in k:
                    # Skip non-enrollees beneficiaries
                    continue
                name = bene.get('name', '').strip()
                if len(name) == 0:
                    # Name not set -- skip
                    continue
                first, last = name.split(' ', 1) if ' ' in name else (name, '')
                vars['enrollee']['beneficiaries'][source].append({
                    'first': first,
                    'last': last,
                    'ssn': bene['ssn'],
                    'birthdate': bene['birthdate'],
                    'percentage': bene['percentage'],
                    'relationship': bene['relationship'].lower(),
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
            beneficiary['relationship'] = 'eggert'
            relationship = RELATIONSHIP_ROLES.get(beneficiary['relationship'])
            if relationship is None:
                # Special case -- when specified relationship can't be
                # identified, use the code for "Other" but retain specified
                # relationship value
                relationship = RELATIONSHIP_ROLES.get(None)
                relationship['name'] = beneficiary['relationship']
            vars['relationships'][key].append(relationship)

    vars['encoded_pdf'] = (None if pdf_bytes is None
                           else base64.b64encode(pdf_bytes))
    
    
    return vars


def get_riders(data, applicant_type):
    if applicant_type not in ['employee', 'spouse']:
        return []

    riders = []

    if not data.is_import():
        if applicant_type == 'employee':
            return data.get_selected_employee_riders()
        elif applicant_type == 'spouse':
            return data.get_selected_spouse_riders()

    else:
        # Get riders from enrollment data
        if applicant_type == 'employee':
            prefix = 'emp'
        elif applicant_type == 'spouse':
            prefix = 'sp'

        for csv_rider in ['air', 'wp', 'qol3', 'qol4']:
            r = data.get('{}_rider_{}'.format(prefix, csv_rider))
            if r is not None and r.upper() == 'Y':
                riders.append(csv_rider.upper())
    return riders


def make_applicant_query(data, applicant_type, enrollee, case):
    riders = get_riders(data, applicant_type)
    return ApplicantQuery(
            applicant_type='child' if applicant_type.startswith('ch') else applicant_type,
            product_options={'riders': riders},
            demographics=ApplicantDemographics(
                    demographics_object={'applicant_type': applicant_type}),
            state=enrollee['state'],
            mode=data['payment_mode'],
            rate_options=ApplicantQueryOptions({})
    )


def get_policy_info(data, applicant_type, enrollee, case):

    plan_code = get_plan_code(data.get_product_code(),
                              make_applicant_query(data, applicant_type,
                                                   enrollee, case))
    if plan_code is None:
        plan_code = get_invalid_plan_code(data.get_product_code())

    # This is fairly FPP-specific, will need to be changed if other products add STP perhaps.
    if data.get_product_code().startswith('FPPTI'):
        # FPPTI, FPPTIW, FPPTIY, etc
        product_code = "Term"
    else:
        # Not sure ?
        product_code = "Family Protection"

    policy = {
        'carrier_code': CARRIER_CODE,
        'status_code': POLICY_STATUS_CODE,
        'status': POLICY_STATUS,
        'is_replacement_code': BOOLEAN[data.get('replacing_insurance')],
        'is_replacement': data.get('replacing_insurance', False),
        'replacement_number': data.get('replacement_policy1_number', ''),
        'existing_insurance_code': BOOLEAN[data.get('existing_insurance')],
        'existing_insurance': data.get('existing_insurance'),
        'product_type': plan_code,
        'product_code': product_code,
        'base_product_type': data.get_product_code(),
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
    policy['riders'] = get_riders(data, applicant_type)
    return policy


def get_agents(data, enrollment):
    agents = []
    case = enrollment.case
    if case.agent_splits is not None:
        # Case has agent splits
        # for split in case.agent_splits:
        for split in [s for s in case.agent_splits if s.product_id == data.get_product_id()]:
            if split.agent is None:
                # Writing agent (null agent) is placeholder for enrolling agent
                agent = data.get_signing_agent()
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

    if not agents or not case.agent_splits:
        agent = data.get_signing_agent()
        agents.append({
            'first': agent.first,
            'last': agent.last,
            'code': agent.agent_code,
            'commission_percent': '',
        })

    return agents


def test_wizard_xml():
    from zipfile import ZipFile
    from io import BytesIO
    from taa import db
    from taa.services.enrollments.models import EnrollmentApplication
    from taa.services.submissions import EnrollmentSubmissionService
    apps = db.session.query(EnrollmentApplication).filter(EnrollmentApplication.signature_time >= '2016-07-07'
          ).options(db.subqueryload('coverages').joinedload('enrollment').joinedload('case').joinedload('owner_agent')
          ).all()
    
    coverages = []
    for app in apps:

        for coverage in app.coverages:
            enrollment = coverage.enrollment
            case = enrollment.case
            splits = case.agent_splits
            for split in splits:
                agent = split.agent
            
            if coverage.product.can_submit_stp():
                coverages.append(coverage)

    print("Processing {} coverages: ".format(len(coverages)))

    zipstream = BytesIO()
    app_pdfs = set()
    with ZipFile(zipstream, 'w') as zip:
        for coverage in coverages:
            print("Processing app #{}, applicant {}, product '{}'".format(coverage.enrollment.id, coverage.applicant_type,
                                                                          coverage.product.name))
            
            if coverage.enrollment.id not in app_pdfs:
                # Generate and write out the PDF
                pdf_bytes = EnrollmentSubmissionService().render_enrollment_pdf(coverage.enrollment, is_stp=True,
                                                                                 product_id=coverage.product_id)
                zip.writestr('enrollment_{}.pdf'.format(coverage.enrollment.id), pdf_bytes)
                app_pdfs.add(coverage.enrollment.id)
            

            xmls = []
            if coverage.applicant_type == 'children':
                # Generate one for each child
                data = EnrollmentApplicationService().get_wrapped_data_for_coverage(coverage)
                for i, child in enumerate(data['children']):
                    print("Generating child {}".format(i + 1))
                    applicant_type = "child{}".format(i)
                    xml = EnrollmentSubmissionService().render_enrollment_xml(coverage, applicant_type, pdf_bytes=None)
                    if xml:
                        xmls.append((xml, applicant_type))
            else:
                
                applicant_type = coverage.applicant_type
                xml = EnrollmentSubmissionService().render_enrollment_xml(coverage, applicant_type, pdf_bytes=None)
                if xml:
                    xmls.append((xml, applicant_type))
            
            for xml, applicant_type in xmls:
    
                fn = 'enrollment_{}-{}.xml'.format(coverage.enrollment.id, applicant_type)
                zip.writestr(fn, xml.encode('latin-1'))
            

    f = open('out.zip', 'w+')
    f.write(zipstream.getvalue())
    f.close()

if __name__ == "__main__":
    test_wizard_xml()

