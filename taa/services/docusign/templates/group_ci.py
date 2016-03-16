from taa.services.docusign.service import DocuSignServerTemplate, DocuSignRadioTab, DocuSignTextTab
from taa.services.docusign.DocuSign_config import get_template_id
from taa.services.products import ProductService

product_service = ProductService()


class GroupCITemplate(DocuSignServerTemplate):
    def __init__(self, recipients, enrollment_data, use_docusign_renderer):

        product_type = enrollment_data.get_product_code()
        state = enrollment_data["enrollState"]
        template_id = get_template_id(product_type, state)

        DocuSignServerTemplate.__init__(self, template_id, recipients, use_docusign_renderer)

        self.data = enrollment_data

    def num_children_on_form(self):
        return 4

    def is_child_attachment_form_needed(self):
        return self.data.get_num_covered_children() > self.num_children_on_form()

    def get_attachment_children(self):
        return self.data.get_covered_children()[self.num_children_on_form():] if len(self.data.get_covered_children()) > self.num_children_on_form() else []

    def should_include_bank_draft(self):
        return self.data.should_include_bank_draft()

    def generate_tabs(self, recipient, purpose):

        tabs = super(GroupCITemplate, self).generate_tabs(recipient, purpose)

        if recipient.is_agent():
            tabs += self.convert_to_tab_objects(self.make_agent_tabs())

        if recipient.is_employee() or self.data.should_use_call_center_workflow():
            tabs += self.convert_to_tab_objects(self.make_employee_tabs())

        return tabs

    def make_agent_tabs(self):
        agent_radios = []

        if not self.data.should_use_call_center_workflow():
            # identical to whatever EE said
            agent_radios.append(
                {
                    'groupName': 'existingInsAgent',
                    'radios': [
                        {
                            'selected': 'True',
                            'value': self.data['existing_insurance']
                        }
                    ]
                }
            )
            agent_radios.append(
                {
                    'groupName': 'replaceAgent',
                    'radios': [
                        {
                            'selected': 'True',
                            'value': self.data['replacing_insurance']
                        }
                    ]
                }
            )

        return {'radioGroupTabs': agent_radios}

    def convert_to_tab_objects(self, docusign_tabs):
        "Takes docusign-formatted tab dicts and converts them to our internal, intermediate representation that our PDF renderer understands."
        tabs = []

        if 'radioGroupTabs' in docusign_tabs:
            for tab in docusign_tabs['radioGroupTabs']:
                for radio in tab['radios']:
                    if radio.get('selected') == "True":
                        tabs.append(DocuSignRadioTab(group_name=tab['groupName'], value=radio['value'], is_selected="True"))

        if 'textTabs' in docusign_tabs:
            for tab in docusign_tabs['textTabs']:
                tabs.append(DocuSignTextTab(name=tab['tabLabel'], value=tab['value']))

        return tabs

    def make_employee_tabs(self):
        # To get the legacy code below to work, make this a local variable.
        enrollment_data = self.data

        idType = enrollment_data.get_id_type()
        idToken = enrollment_data.get_id_token()
        idTokenStr = 'Authentication via ' + idType + ': ' + idToken

        SOH_RadiosList = []
        SOH_GI_Tabs = []

        eeCoverageNullToken = 'NONE'
        if enrollment_data['employee_coverage']:
            if enrollment_data['employee_coverage']['face_value']:
                employeeCoverage = format(
                    float(enrollment_data['employee_coverage']['face_value']), ',.0f')
                eePremium = format(float(enrollment_data['employee_coverage']['premium']), ',.2f')
                SOH_RadiosList += generate_SOHRadios(
                    'ee', enrollment_data.get_employee_soh_questions())
                SOH_GI_Tabs += generate_SOH_GI_tabs(
                    'ee', enrollment_data.get_employee_soh_questions())
            else:
                employeeCoverage = eeCoverageNullToken
                eePremium = ' '
        else:
            employeeCoverage = eeCoverageNullToken
            eePremium = ' '

        if enrollment_data['spouse_coverage']:
            if enrollment_data['spouse_coverage']['face_value']:
                spouseCoverage = format(
                    float(enrollment_data['spouse_coverage']['face_value']), ',.0f')
                spPremium = format(float(enrollment_data['spouse_coverage']['premium']), ',.2f')
                SOH_RadiosList += generate_SOHRadios(
                    'sp', enrollment_data.get_spouse_soh_questions())
                SOH_GI_Tabs += generate_SOH_GI_tabs(
                    'sp', enrollment_data.get_spouse_soh_questions())
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
            childRadiosList += generate_SOHRadios('c%s' % (i+1), enrollment_data.get_child_soh_questions(i))
            SOH_GI_Tabs += generate_SOH_GI_tabs('c%s' % (i+1), enrollment_data.get_child_soh_questions(i))

        agent_code = enrollment_data.get_agent_code()
        agent_signing_name = enrollment_data.get_agent_signing_name()

        eeTabsList = make_applicant_tabs('ee', enrollment_data['employee'])
        eeTabsList += [
            make_tab('eeEnrollCityState', u'{}, {}'.format(
                enrollment_data['enrollCity'], enrollment_data['enrollState'])),
            make_tab('identityToken', idTokenStr),
            make_tab('agentCode', agent_code),
            make_tab('agentSignName', agent_signing_name),
            make_tab('eeCoverage', employeeCoverage),
            make_tab('eePremium',
                     eePremium if employeeCoverage != eeCoverageNullToken else ''),
            make_tab('Employer', enrollment_data.case.company_name),
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
        ]

        eeTabsList += make_contact_tabs('ee', enrollment_data['employee'])

        if enrollment_data['spouse_owner'] == 'other':
            spouseOtherOwnerName = enrollment_data['spouse_other_owner_name']
            spouseOtherOwnerSSN = enrollment_data['spouse_other_owner_ssn']
        elif enrollment_data['spouse_owner'] == 'employee':
            spouseOtherOwnerName = u'{} {}'.format(
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

        generalRadiosList = []
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
                        'value': enrollment_data['replacing_insurance']
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

        return {
            'textTabs':
                eeTabsList + spouseTabsList + childTabsList + SOH_GI_Tabs,
            'radioGroupTabs':
                generalRadiosList + SOH_RadiosList + childRadiosList
        }


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
            'value': u'{} {}'.format(
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
            'value': format(float(child_coverage['face_value']), ',.0f') if child_coverage else ''
        },
        {
            'tabLabel': childStr + 'Premium',
            'value':
                format(float(child_coverage['premium']), ',.2f') if child_coverage else ''
        },
    ]
    return tabsList


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