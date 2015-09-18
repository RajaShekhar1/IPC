# DocuSign API Walkthrough 08 (PYTHON) - Embedded Signing
import random
import json
from string import ascii_letters
import decimal

import httplib2
import flask

from taa import app
from taa.services.docusign.DocuSign_config import (
    dsAPIAuthenticateString,
    baseUrl,
    apiAccountID,
    templateClientID,
    get_template_id
)
from taa.services.products import ProductService
from taa.services.agents import AgentService

product_service = ProductService()
agent_service = AgentService()


def generate_SOHRadios(prefix, soh_questions):
    radioList = []
    for i, soh_question in enumerate(soh_questions):
        if soh_question['answer'] and soh_question['answer'].upper() == 'GI':
            # GI - skip for now
            selected = 'False'
            answer = 'GI'
        else:
            selected = 'True'
            answer = 'no'
        radioList.append({
            'groupName': prefix + 'SOH' + str(i+1),
            'radios': [{'selected': selected, 'value': answer}],
        })
    return radioList


def generate_SOH_GI_tabs(prefix, soh_questions):
    tabs = []
    for i, soh_question in enumerate(soh_questions):
        if soh_question['answer'] and soh_question['answer'].upper() == 'GI':
            # GI - skip for now
            answer = 'GI'
        else:
            answer = ''
        tabs.append(make_tab('{prefix}SOH{i}gi'.format(prefix=prefix, i=i+1), answer))
    return tabs


def generate_ChildGenderRadio(child_index, wizard_data):
    return {
        'groupName': 'child' + str(child_index + 1) + 'Gender',
        'radios': [
            {
                'selected': 'True' if wizard_data['children'][child_index]['gender'] == 'male' else 'False',
                'value': 'male'
            },
            {
                'selected': 'True' if wizard_data['children'][child_index]['gender'] == 'female' else 'False',
                'value': 'female'
            }
        ]}


def generate_ChildGenderAbbrevTab(child_index, wizard_data):
    if wizard_data['children'][child_index]['gender'] == 'male':
        genderAbbrev = 'M'
    elif wizard_data['children'][child_index]['gender'] == 'female':
        genderAbbrev = 'F'
    else:
        genderAbbrev = ''
    return {'tabLabel': 'child' + str(child_index + 1) + 'GenderAbbrev',
            'value': genderAbbrev}


def generate_ChildTabsEntry (child_index, wizard_data):
    childStr = 'child' + str(child_index +1)
    child_coverage = wizard_data['child_coverages'][child_index]
    tabsList = [
        # FullName is only used for child >2, otherwise FName and LName on
        # child <=2 assuming for now the Docusign API will ignore those tabs
        # not used in the template
        {
            'tabLabel': childStr + 'FullName',
            'value': '{} {}'.format(
                wizard_data['children'][child_index]['first'],
                wizard_data['children'][child_index]['last'])
        },
        {
            'tabLabel': childStr + 'FName',
            'value': wizard_data['children'][child_index]['first']
        },
        {
            'tabLabel': childStr + 'LName',
            'value': wizard_data['children'][child_index]['last']
        },
        {
            'tabLabel': childStr + 'DOB',
            'value': wizard_data['children'][child_index]['birthdate']
        },
        {
            'tabLabel': childStr + 'SSN',
            'value': wizard_data['children'][child_index]['ssn']
        },
        {
            'tabLabel': childStr + 'Coverage',
            'value': format(child_coverage['face_value'], ',.0f') if child_coverage else ''
        },
        {
            'tabLabel': childStr + 'Premium',
            'value':
                format(child_coverage['premium']*52/12,
                       ',.2f') if child_coverage else ''
        },
    ]
    return tabsList


class EnrollmentDataWrap(object):
    def __init__(self, wizard_data, census_record, case):
        self.data = wizard_data
        self.census_record = census_record
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
        return not self.data['agent_data']['is_in_person']

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
        elif self.data['agent_data']['is_in_person']:
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
        else:
            return agent_service.get_logged_in_agent()

    def get_employer_name(self):
        return self.case.company_name

    def get_product_code(self):
        return self.data['product_type']

    def get_employee_name(self):
        return '{} {}'.format(self.data['employee']['first'],
                              self.data['employee']['last'])

    def get_spouse_name(self):
        return '{} {}'.format(self.data['spouse']['first'],
                              self.data['spouse']['last'])

    def get_employee_email(self):
        email_to = self.data['employee']['email']
        if not email_to:
            # fallback email if none was entered - just need a unique address
            name = self.data['employee']['first']+ '.' + self.data['employee']['last']
            email_to = '{}.{}@5StarEnroll.com'.format(name, self.random_email_id(name))
        return email_to

    def get_employee_email_parts(self):
        if '@' not in self.get_employee_email():
            return '', self.get_employee_email()
        else:
            return self.get_employee_email().split('@', 1)

    def random_email_id(self, name='', token_length=8):
        chars = 'ABCDEF0123456789'
        name = filter(lambda x: x in ascii_letters + '.', name)
        if name != '':
            name = name + '_'
        return name + ''.join([random.choice(chars)
                               for _ in range(token_length)])

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

    def get_num_covered_children(self):
        return len(self.get_covered_children())

    def get_covered_children(self):
        covered_children = []
        for i, child in enumerate(self.data['children']):
            coverage = self.data['child_coverages'][i]
            if coverage and coverage['face_value']:
                covered_children.append(child)
        return covered_children

    def get_employee_esignature(self):
        return self.data.get('emp_sig_txt', '')

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

def old_create_envelope_and_get_signing_url(enrollment_data):
    # return is_error(bool), error_message, and redirectURL
    product = product_service.get(enrollment_data['product_data']['id'])

    # Product code
    productType = enrollment_data['product_type']
    enrollmentState = enrollment_data['agent_data']['state']

    recipName = enrollment_data.get_employee_name()
    employer = enrollment_data.get_employer_name()
    emailTo = enrollment_data.get_employee_email()
    sessionType = enrollment_data.get_session_type()
    idType = enrollment_data.get_id_type()
    idToken = enrollment_data.get_id_token()
    idTokenStr = 'Authentication via ' + idType + ': ' + idToken

    SOH_RadiosList = []
    SOH_GI_Tabs = []

    if recipName != '' and recipName != None:
        recipientName = recipName
    else:
        recipientName = 'Applicant'

    eeCoverageNullToken = 'NONE'
    if enrollment_data['employee_coverage']:
        if enrollment_data['employee_coverage']['face_value']:
            employeeCoverage = format(
                enrollment_data['employee_coverage']['face_value'], ',.0f')
            eePremium = format(
                round((enrollment_data['employee_coverage']['premium']*100 * 52)/12)/100.0, ',.2f')
            SOH_RadiosList += generate_SOHRadios(
                'ee', enrollment_data['employee']['soh_questions'])
            SOH_GI_Tabs += generate_SOH_GI_tabs(
                'ee', enrollment_data['employee']['soh_questions'])
        else:
            employeeCoverage = eeCoverageNullToken
            eePremium = ' '
    else:
        employeeCoverage = eeCoverageNullToken
        eePremium = ' '

    if enrollment_data['spouse_coverage']:
        if enrollment_data['spouse_coverage']['face_value']:
            spouseCoverage = format(
                enrollment_data['spouse_coverage']['face_value'], ',.0f')
            spPremium = format(
                round((enrollment_data['spouse_coverage']['premium']*100 * 52)/12)/100.0, ',.2f')
            SOH_RadiosList += generate_SOHRadios(
                'sp', enrollment_data['spouse']['soh_questions'])
            SOH_GI_Tabs += generate_SOH_GI_tabs(
                'sp', enrollment_data['spouse']['soh_questions'])
        else:
            spouseCoverage = ' '
            spPremium = ' '
    else:
        spouseCoverage = ' '
        spPremium = ' '

    childTabsList = []
    childRadiosList = []
    for i, child in enumerate(enrollment_data['children']):
        if (not enrollment_data['children'][i] or
                not enrollment_data['child_coverages'][i]):
            continue
        childTabsList += generate_ChildTabsEntry(i, enrollment_data)
        childRadiosList.append(generate_ChildGenderRadio(i, enrollment_data))
        childRadiosList += generate_SOHRadios('c%s' % (i+1), enrollment_data['children'][i]['soh_questions'])
        SOH_GI_Tabs += generate_SOH_GI_tabs('c%s' % (i+1), enrollment_data['children'][i]['soh_questions'])

    agent_code = enrollment_data.get_agent_code()
    agent_signing_name = enrollment_data.get_agent_signing_name()

    eeTabsList = make_applicant_tabs('ee', enrollment_data['employee'])
    eeTabsList += [
        make_tab('eeEnrollCityState', '{}, {}'.format(
            enrollment_data['enrollCity'], enrollment_data['enrollState'])),
        make_tab('identityToken', idTokenStr),
        make_tab('agentCode', agent_code),
        make_tab('agentSignName', agent_signing_name),
        make_tab('eeCoverage', employeeCoverage),
        make_tab('eePremium',
                 eePremium if employeeCoverage != eeCoverageNullToken else ''),
        make_tab('Employer', enrollment_data['agent_data']['company_name']),
        make_tab('eeOtherOwnerName',
                 enrollment_data['employee_other_owner_name'] if (
                     enrollment_data['employee_owner'] == 'other') else ''),
        make_tab('eeOtherOwnerName2',
                 enrollment_data['employee_other_owner_name'] if (
                     enrollment_data['employee_owner'] == 'other') else ''),
        make_tab('eeOtherOwnerSSN',
                 enrollment_data['employee_other_owner_ssn'] if (
                     enrollment_data['employee_owner'] == 'other') else ''),
        make_tab('eeEmail', enrollment_data.get_employee_email())
        #make_tab('eeEmailPart1', ee_email_part_1),
        #make_tab('eeEmailPart2', ee_email_part_2),
    ]

    eeTabsList += make_contact_tabs('ee', enrollment_data['employee'])

    def make_beneficiary_tabs(prefix, name, relationship, dob, ssn):
        return [
            make_tab(prefix+'BeneFullName', name),
            make_tab(prefix+'BeneRelationship', relationship),
            make_tab(prefix+'BeneDOB', dob),
            make_tab(prefix+'BeneSSN', ssn),
        ]

    # add in beneficiaries if appropriate
    if employeeCoverage != eeCoverageNullToken:
        if enrollment_data['employee_beneficiary'] == 'spouse':
            eeTabsList += make_beneficiary_tabs(
                prefix='ee',
                name='{} {}'.format(enrollment_data['spouse']['first'],
                                    enrollment_data['spouse']['last']),
                relationship='spouse',
                dob=enrollment_data['spouse']['birthdate'],
                ssn=enrollment_data['spouse']['ssn'],
            )
        else:
            eeTabsList += make_beneficiary_tabs(
                prefix = 'ee',
                name = enrollment_data.get('employee_beneficiary_name', ''),
                relationship = enrollment_data.get(
                    'employee_beneficiary_relationship', ''),
                dob = enrollment_data.get('employee_beneficiary_dob', ''),
                ssn = enrollment_data.get('employee_beneficiary_ssn', ''),
            )
        # TODO: Add in ee contingent beneficiary once the tab names are known

    if enrollment_data['spouse_owner'] == 'other':
        spouseOtherOwnerName = enrollment_data['spouse_other_owner_name']
        spouseOtherOwnerSSN = enrollment_data['spouse_other_owner_ssn']
    elif enrollment_data['spouse_owner'] == 'employee':
        spouseOtherOwnerName = '{} {}'.format(
            enrollment_data['employee']['first'],
            enrollment_data['employee']['last'])
        spouseOtherOwnerSSN = enrollment_data['employee']['ssn']
    else:
        spouseOtherOwnerName = ''
        spouseOtherOwnerSSN = ''

    spouseTabsList = []
    if spouseCoverage != ' ':
        spouseTabsList += make_applicant_tabs('sp', enrollment_data['spouse'])
        spouseTabsList += [
            {
                'tabLabel': 'spOtherOwnerName',
                'value': spouseOtherOwnerName
            },
            {
                'tabLabel': 'spOtherOwnerSSN',
                'value': spouseOtherOwnerSSN
            },
            {
                'tabLabel': 'spCoverage',
                'value': spouseCoverage
            },
            {
                'tabLabel': 'spPremium',
                'value': spPremium
            }
        ]
        if enrollment_data['spouse_beneficiary'] == 'spouse':
            spouseTabsList += make_beneficiary_tabs(
                prefix='sp',
                name='{} {}'.format(enrollment_data['employee']['first'],
                                    enrollment_data['employee']['last']),
                relationship='spouse',
                dob=enrollment_data['employee']['birthdate'],
                ssn=enrollment_data['employee']['ssn'],
            )
        else:
            spouseTabsList += make_beneficiary_tabs(
                prefix='sp',
                name=enrollment_data['spouse_beneficiary_name'],
                relationship=enrollment_data['spouse_beneficiary_relationship'],
                dob=enrollment_data['spouse_beneficiary_dob'],
                ssn=enrollment_data['spouse_beneficiary_ssn'],
            )
        # TODO: Add in spouse contingent beneficiary once the tab names
        # are known

    generalRadiosList = []
    # Note: UI screens out any 'yes' replacement - so all applications are
    # 'no' to replacement
    generalRadiosList.append(
        {
            'groupName': 'productType',
            'radios': [
                {
                    'selected': 'True',
                    # FPPTI or FPPCI
                    'value':
                        'FPPTI' if productType == 'FPP-Gov' else productType
                }
            ]
        }
    )
    generalRadiosList.append(
        {
            'groupName': 'existingIns',
            'radios': [
                {
                    'selected': 'True',
                    'value': enrollment_data['existing_insurance']
                }
            ]
        }
    )
    generalRadiosList.append(
        {
            'groupName': 'replace',
            'radios': [
                {
                    'selected': 'True',
                    'value': 'no'
                }
            ]
        }
    )
    for (prefix_short, prefix_long) in {('ee', 'employee'), ('sp', 'spouse')}:
        generalRadiosList.append(
            {
                'groupName': prefix_short + 'Gender',
                'radios': [
                    {
                        'selected':
                            'True' if (enrollment_data[prefix_long] and
                                       enrollment_data[prefix_long]['gender'] == 'male') else 'False',
                        'value': 'male'
                    },
                    {
                        'selected':
                            'True' if (enrollment_data[prefix_long] and
                                       enrollment_data[prefix_long]['gender'] == 'female') else 'False',
                        'value': 'female'
                    }
                ]
            }
        )
        if enrollment_data[prefix_long] and 'is_smoker' in enrollment_data[prefix_long]:
            generalRadiosList.append(
                {
                    'groupName': prefix_short + 'Smoking',
                    'radios': [
                        {
                            'selected': 'True' if enrollment_data[prefix_long]['is_smoker'] else 'False',
                            'value': 'smoker'
                        },
                        {
                            'selected': 'True' if not enrollment_data[prefix_long]['is_smoker'] else 'False',
                            'value': 'nonsmoker'
                        }
                    ]
                }
            )
        # only include Owner checkbox if coverage was selected
        if ((prefix_short == 'ee' and
             employeeCoverage != eeCoverageNullToken) or
                (prefix_short == 'sp' and spouseCoverage != ' ')):
            generalRadiosList.append(
                {
                    'groupName': prefix_short + 'Owner',
                    'radios': [
                        {
                            'selected':
                                'True' if enrollment_data[prefix_long + '_owner'] == 'self' else 'False',
                            'value': 'self'
                        },
                        {
                            'selected':
                                'True' if ((enrollment_data[prefix_long + '_owner'] == 'other') or
                                           (enrollment_data[prefix_long + '_owner'] == 'employee')) else 'False',
                            'value': 'other'
                        }
                    ]
                }
            )
    agentRadiosList = []
    # identical to whatever EE said
    agentRadiosList.append(
        {
            'groupName': 'existingInsAgent',
            'radios': [
                {
                    'selected': 'True',
                    'value': enrollment_data['existing_insurance']
                }
            ]
        }
    )
    agentRadiosList.append(
        {
            'groupName': 'replaceAgent',
            'radios': [
                {
                    'selected': 'True',
                    'value': 'no'
                }
            ]
        }
    )

    # *******************************
    # Create envelope with an embedded recipient
    # NOTE: this could be done via SOBO, in which case we'd need to follow
    # the general procedure below. However, this seems to add more complexity
    # and the benefit isn't clear what the Agent can do as a 'sender' that he
    # can't already (since we merely want him to be a signer). Having him be
    # able to access 'sent' items before signed seems useless since the form is
    # blank until after the employee signs and commits, so there's no data in
    # there to resend. Further, by the admin account being the sender, we have
    # faster access into what envelopes are out/pending, etc. So, for now we'll
    # still avoiding SOBO on creating the envelopes (still need, elsewhere,
    # for access to console for signing).
    # Nonetheless, here's the general notes on the process:
    """
    1. turn on 'Sending' permission

    POST to:
    https://{{environment}}.docusign.net/restapi/v2/accounts/{{AccountID}}/users/
    {
    'newUsers':[{
        'email': 'exactemail@address.com',
        'userName':'Exact Name Match',
        'userSettings': [
            {
                'name': 'canSendEnvelope',
                'value': 'false'
            },
            {
                'name': 'enableSequentialSigningAPI',
                'value': 'false'
            }
          ]
    }]


    2. use SOBO authentication
       authenticateStr = dsAgentAuthenticateString() if sessionUserApprovedForDocusign() else dsAPIAuthenticateString()

    3. turn off 'Sending' permissions (reverse the above)
    """
    # *******************************
    authenticateStr = dsAPIAuthenticateString()

    accountId = apiAccountID

    # same role name that exists on the template in the console
    templateRoleName = 'Employee'
    templateAgentRoleName = 'Agent'

    # TODO: Enable GI tabs for other products once the forms are ready
    if productType == 'Group CI':
        giTabsList = SOH_GI_Tabs
    else:
        giTabsList = []

    # construct the body of the request in JSON format
    requestBody = {
        'accountID': accountId,
        'status': 'sent',
        'emailSubject':
            "signature needed: {} for {} ({})".format(productType,
                                                      recipientName, employer),
        'templateId': get_template_id(productType, enrollmentState),
        'templateRoles': [
            {
                'email': emailTo,
                'name': recipientName,
                'tabs': {
                    'textTabs':
                        eeTabsList + spouseTabsList + childTabsList + giTabsList,
                    'radioGroupTabs':
                        generalRadiosList + SOH_RadiosList + childRadiosList
                },
                'roleName':  templateRoleName,
                'clientUserId': templateClientID
            },
            {
                'email': enrollment_data.get_signing_agent().email, #user.email,
                'name': enrollment_data.get_agent_signing_name(),
                'tabs': {
                    'radioGroupTabs': agentRadiosList
                },
                'roleName':  templateAgentRoleName
                #'clientUserId': templateClientID
            }
        ]
    }

    # Debug what we send to DocuSign
    import pprint
    pprint.pprint(requestBody)

    # Convert to JSON string
    requestBodyStr = json.dumps(requestBody)

    # append '/envelopes' to baseURL and use in the request
    url = baseUrl + '/envelopes'
    headers = {
        'X-DocuSign-Authentication': authenticateStr,
        'Accept': 'application/json',
        'Content-Length': str(len(requestBodyStr))
    }
    http = httplib2.Http()
    response, content = http.request(url, 'POST', headers=headers,
                                     body=requestBodyStr)

    # When troubleshooting, send instead to requestb.in (or similar listener)
    # to capture/examing the JSON trace.  Past that trace into SOAPUI to
    # explore the response if needed.
    # response, content = http.request('http://requestb.in/1mars8q1', 'POST',
    #                                  headers=headers, body=requestBodyStr);
    status = response.get('status')
    if (status != '201'):
        print('url=', url)
        print('headers=', headers)
        print(requestBodyStr)
        print("Error generating Docusign envelope, status is: {}".format(
            status))
        return True, "Error generating Docusign envelope", None
    data = json.loads(content)

    # store the uri for next request
    uri = data.get('uri')
    # write to log in case we need for short-term retrieval
    print("Envelope for {} ({}) by {}: {}\n".format(
        recipientName, emailTo, enrollment_data.get_agent_signing_name(), uri))

    #
    # Get the Embedded Send View
    #

    # construct the body of the request in JSON format
    requestBody = {
        'authenticationMethod': 'email',
        'email': emailTo,
        'returnUrl':  build_callback_url(enrollment_data, sessionType),
        'clientUserId': templateClientID,
        'userName': recipientName
    }
    requestBodyStr = json.dumps(requestBody)

    # append uri + '/views/recipient' to baseUrl and use in the request,
    # don't need OnBehalfOf for this so just use API auth
    url = baseUrl + uri + '/views/recipient'
    headers = {
        'X-DocuSign-Authentication': dsAPIAuthenticateString(),
        'Accept': 'application/json',
        'Content-Length': str(len(requestBodyStr))
    }
    http = httplib2.Http()
    response, content = http.request(url, 'POST', headers=headers,
                                     body=requestBodyStr)
    status = response.get('status')
    # print('response: {}\ncontent: {}'.format(response, content))
    if status != '201':
        print('url=', url)
        print('headers=', headers)
        print(requestBodyStr)
        print("Error retrieving signature URL, status is: {}".format(status))
        return True, "Error retrieving signature URL", None
    data = json.loads(content)
    viewUrl = data.get('url')
    return False, None, viewUrl


def make_tab(name, val):
    return dict(tabLabel=name, value=val)


def make_radio_tab(group_name, selected, val):
    return {
        'groupName': group_name,
        'radios': [
            {
                'selected': 'True' if selected else 'False',
                'value': val,
            }
        ]
    }


def make_applicant_tabs(prefix, data):
    tabs = [
        make_tab(prefix + 'FName', data['first']),
        make_tab(prefix + 'LName', data['last']),
        make_tab(prefix + 'DOB', data['birthdate']),
        make_tab(prefix + 'SSN', data['ssn']),
    ]
    if data.get('height'):
        height_ft = '%s' % int(data['height']/12.0)
        height_in = '%s' % int(data['height']%12.0)
        tabs += [
            make_tab(prefix + 'HeightFt', height_ft),
            make_tab(prefix + 'HeightIn', height_in),
        ]
    if data.get('weight'):
        tabs += [make_tab(prefix + 'Weight', data['weight'])]
    return tabs


def make_contact_tabs(prefix, data):
    return [
        make_tab(prefix + 'Street1', data['address1']),
        make_tab(prefix + 'Street2',
                 data['address2'] if 'address2' in data else ''),
        make_tab(prefix + 'City', data['city']),
        make_tab(prefix + 'State', data['state']),
        make_tab(prefix + 'Zip', data['zip']),
        make_tab(prefix + 'Phone', data['phone']),
        make_tab(prefix + 'Email', data['email']),
    ]


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
