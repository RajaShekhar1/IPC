# DocuSign API Walkthrough 08 (PYTHON) - Embedded Signing
import decimal
import random
from datetime import datetime

import flask
import re
from dateutil.parser import parse as dateutil_parse

from taa import app
from taa.services.agents import AgentService
from taa.services.products import ProductService
from taa.services.products.riders import RiderService
from taa.services.enrollments import EnrollmentApplication

product_service = ProductService()
agent_service = AgentService()
rider_service = RiderService()


class EnrollmentDataWrap(object):
    def __init__(self, wizard_data, case):
        self.data = wizard_data
        self.case = case

    def __getitem__(self, item):
        """Allow dict access to raw data"""
        return self.data[item]

    def __setitem__(self, item, val):
        self.data[item] = val

    def __contains__(self, item):
        return item in self.data

    def get(self, key, default=None):
        return self.data.get(key, default)

    def is_self_enroll(self):
        if self.data.get("is_third_party"):
            return False

        # If wizard and not inperson, it is self-enroll.
        if self.data.get('method') == EnrollmentApplication.METHOD_SELF_EMAIL:
            return True

        return False

    def is_enrollment_type_agent_assisted(self):
        """
        For checkbox on the top of the FPP form. For imports, pass data through, otherwise use self_enroll.
        """
        if self.data.get('enrollment_type'):
            return self.data['enrollment_type'].lower() == 'a'

        return not self.is_self_enroll()

    def is_import(self):
        return bool(self.data.get("is_third_party"))

    def get_session_type(self):
        if self.data.get("is_third_party"):
            return 'thirdparty'
        elif self.data['method'] == EnrollmentApplication.METHOD_INPERSON:
            return 'inperson'
        else:
            return 'email'

    def get_id_type(self):
        if self.data['identityType']:
            return self.data['identityType']
        else:
            return 'email'

    def get_id_token(self):
        if self.data['identityToken']:
            return self.data['identityToken']
        else:
            return self.get_employee_email()

    def get_agent_signing_name(self):
        if self.data.get('is_third_party'):
            return self.data['agent_name']

        if not flask.has_request_context():
            return '(TEST)'

        agent = self.get_signing_agent()
        if not agent:
            return ''

        # Get stormpath user for agent
        agent_user = agent_service.get_agent_stormpath_account(agent)
        if not agent_user:
            return ''

        if 'signing_name' not in agent_user.custom_data:
            return agent_user.full_name

        return agent_user.custom_data['signing_name']

    def get_agent_code(self):
        if self.data.get('is_third_party'):
            return self.data['agent_code']

        if not flask.has_request_context():
            return '(TEST)'

        agent = self.get_signing_agent()
        if not agent:
            return ''

        return agent.agent_code

    def get_signing_agent(self):
        if self.is_self_enroll():
            if self.case.self_enrollment_setup.enrolling_agent:
                return self.case.self_enrollment_setup.enrolling_agent
            elif self.case.owner_agent:
                return self.case.owner_agent
            else:
                # This is not good - the case should not be enrollable without
                # an owner agent.
                raise Exception('Tried to enroll a case without an '
                                'owner agent.')
        elif self.is_import():
            return self.case.owner_agent
        elif agent_service.get_logged_in_agent():
            return agent_service.get_logged_in_agent()
        else:
            # If the logged-in user is not an agent, default to case owner.
            return self.case.owner_agent

    def get_employer_name(self):
        return self.case.company_name

    def get_product_code(self):
        return self.get_product().get_base_product_code()

    def get_product(self):
        return product_service.get(self.get_product_id())

    def get_product_id(self):
        if 'product_data' in self.data:
            # For backwards compatibility with old data format.
            return self.data['product_data']['id']
        else:
            return self.data['product_id']

    def get_employee_name(self):
        return '{} {}'.format(self.data['employee']['first'],
                              self.data['employee']['last'])

    def get_employee_first(self):
        return self.data['employee']['first']

    def get_employee_last(self):
        return self.data['employee']['last']

    def get_employee_birthdate(self):
        return self.data['employee']['birthdate']

    def get_employee_ssn(self):
        return self.data['employee']['ssn']

    def get_spouse_name(self):
        return '{} {}'.format(self.data['spouse']['first'],
                              self.data['spouse']['last'])

    def get_spouse_ssn(self):
        return self.data['spouse']['ssn']

    def get_employee_email(self):
        email_to = self.data['employee']['email']
        if not email_to:
            # fallback email if none was entered - just need a unique address
            email_to = '{}@5StarEnroll.com'.format(self.random_email_id())

        return email_to

    invalid_email_chars = re.compile(r'[^a-zA-Z0-9!#$%&\'*+\/=?^_`{|}~\.-]')

    def _sanitize_email_str(self, val):
        # Replace invalid characters with empty string
        return self.invalid_email_chars.sub('', val)

    def get_employee_email_parts(self):
        if '@' not in self.get_employee_email():
            return '', self.get_employee_email()
        else:
            return self.get_employee_email().split('@', 1)

    def random_email_id(self, token_length=10):
        chars = 'ABCDEF0123456789'
        return ''.join([random.choice(chars)
                               for _ in range(token_length)])

    def get_employee_date_of_hire(self):
        try:
            # The identityToken is the date_of_hire on FPP products.
            return dateutil_parse(self.data['identityToken'])
        except Exception:
            return None

    def did_employee_select_coverage(self):
        return (self.data['employee_coverage'] and
                self.data['employee_coverage']['face_value'])

    def get_employee_coverage(self):
        return format(self.data['employee_coverage']['face_value'], ',.0f')

    def get_formatted_employee_premium(self):
        return self.format_money(self.get_employee_premium())

    def get_employee_premium(self):
        return decimal.Decimal(self.data['employee_coverage']['premium'])

    def did_spouse_select_coverage(self):
        return (self.data['spouse_coverage'] and
                self.data['spouse_coverage']['face_value'])

    def get_spouse_coverage(self):
        return format(decimal.Decimal(self.data['spouse_coverage']['face_value']), ',.0f')

    def get_formatted_spouse_premium(self):
        return self.format_money(self.get_spouse_premium())

    def get_spouse_premium(self):
        return decimal.Decimal(self.data['spouse_coverage']['premium'])

    def format_money(self, amount):
        return '%.2f' % amount

    def get_total_children_premium(self):
        if self.get_product().is_fpp():
            # Add up the child premium for each child if this is FPP
            return sum(decimal.Decimal(unicode(child_coverage.get('premium', '0.00')))
                       for child_coverage in self.data["child_coverages"])
        else:
            # Just use the first child premium, if any.
            if len(self.data["child_coverages"]) > 0:
                child_coverage = self.data["child_coverages"][0]
                return decimal.Decimal(unicode(child_coverage.get('premium', '0.00')))

            return decimal.Decimal('0.00')

    def get_total_modal_premium(self):
        total = decimal.Decimal('0.00')
        if self.did_employee_select_coverage():
            total += self.get_employee_premium()
        if self.did_spouse_select_coverage():
            total += self.get_spouse_premium()
        if self.get_total_children_premium() > 0.0:
            total += self.get_total_children_premium()

        return total

    def get_num_covered_children(self):
        return len(self.get_covered_children())

    def get_covered_children(self):
        covered_children = []
        for i, child in enumerate(self.data['children']):
            coverage = self.data['child_coverages'][i]
            if coverage and coverage['face_value']:
                covered_children.append(child)
        return covered_children

    def get_employee_soh_questions(self):
        if 'soh_questions' in self.data['employee']:
            # Legacy format
            return self.data['employee']['soh_questions']
        else:
            return self.data['employee_soh_questions']

    def get_spouse_soh_questions(self):
        if 'soh_questions' in self.data['spouse']:
            # Legacy format
            return self.data['spouse']['soh_questions']
        else:
            return self.data['spouse_soh_questions']

    def get_child_soh_questions(self, child_index):
        child = self.data['children'][child_index]
        if 'soh_questions' in child:
            # Backwards compat for legacy data format:
            return child['soh_questions']
        else:
            return self.data['children_soh_questions'][child_index]

    def get_employee_esignature(self):
        # Replace employee signature with "John Doe voice auth on file 02:45pm"
        esig = "{} voice auth on file {}".format(self.get_employee_name(), datetime.now().strftime("%l:%M%p"))
        return self.data.get('emp_sig_txt', esig)

    def get_employee_initials(self):
        return self.data.get('emp_initials_txt', '')

    def has_employee_esigned(self):
        return bool(self.get_employee_esignature())

    def get_agent_esignature(self):
        return self.data.get('agent_sig_txt', '')

    def has_agent_esigned(self):
        return bool(self.get_agent_esignature())

    def get_agent_initials(self):
        return self.data.get('agent_initials_txt', '')

    def get_beneficiary_data(self):
        bene_data = {
            'employee_primary':[],
            'employee_contingent':[],
            'spouse_primary':[],
            'spouse_contingent':[],
        }

        from taa.services.enrollments import EnrollmentRecordParser
        for num in range(1, EnrollmentRecordParser.MAX_BENEFICIARY_COUNT+1):
            if self.data.get("emp_bene{}_name".format(num)):
                bene_data['employee_primary'] += [
                    self.get_beneficiary_dict("emp_bene{}".format(num))
                ]
            if self.data.get("emp_cont_bene{}_name".format(num)):
                bene_data['employee_contingent'] += [
                    self.get_beneficiary_dict("emp_cont_bene{}".format(num))
                ]
            if self.data.get("sp_bene{}_name".format(num)):
                bene_data['spouse_primary'] += [
                    self.get_beneficiary_dict("sp_bene{}".format(num))
                ]
            if self.data.get("sp_cont_bene{}_name".format(num)):
                bene_data['spouse_contingent'] += [
                    self.get_beneficiary_dict("sp_cont_bene{}".format(num))
                ]

        return bene_data

    def get_beneficiary_dict(self, prefix):
        bd = self.data["%s_birthdate" % prefix]
        #try:
        #    bd = dateutil.parser.parse(bd).strftime('%F')
        #except Exception:
        #    pass

        bene_dict = dict(
            name=self.data["%s_name" % prefix],
            ssn=self.data["%s_ssn" % prefix],
            relationship=self.data["%s_relationship" % prefix],
            birthdate=bd,
            percentage=self.data["%s_percentage" % prefix],
        )

        return bene_dict

    def has_multiple_beneficiaries(self):
        """returns True if any of the beneficiaries are not at 100%"""
        bene_pattern = re.compile('_bene\d+_percentage$')

        for key, value in self.data.iteritems():
            if bene_pattern.search(key) and value and value.isdigit() and int(value) < 100:
                return True

        return False

    def should_include_bank_draft(self):
        return self.case.include_bank_draft_form

    def should_use_call_center_workflow(self):
        return self.case.should_use_call_center_workflow

# For employee signing sessions
def build_callback_url(wizard_data, session_type):
    is_ssl = app.config.get('IS_SSL', True)
    hostname = app.config.get('HOSTNAME', '5starenroll.com')
    scheme = 'https://' if is_ssl else 'http://'
    # note: DS supplies the last parm of 'event' in the callback
    return ('{scheme}{hostname}/application_completed'
            '?name={name}&type={session_type}'.format(
                scheme=scheme,
                hostname=hostname,
                name=wizard_data['employee']['first'],
                session_type=session_type,
    ))

def build_callcenter_callback_url(case):
    is_ssl = app.config.get('IS_SSL', True)
    hostname = app.config.get('HOSTNAME', '5starenroll.com')
    scheme = 'https://' if is_ssl else 'http://'
    # note: DS supplies the last parm of 'event' in the callback
    return ('{scheme}{hostname}/enrollment-case/{case_id}#enrollment'.format(
                scheme=scheme,
                hostname=hostname,
                case_id=case.id,
    ))