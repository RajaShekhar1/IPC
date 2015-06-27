# DocuSign API Walkthrough 08 (PYTHON) - Embedded Signing
import random
import json
import httplib2
from string import ascii_letters

import flask
from flask.ext.stormpath import user

from taa import app
from taa.services.docusign.DocuSign_config import (
    dsAPIAuthenticateString,
    baseUrl, 
    apiAccountID, 
    templateClientID,
    get_template_id
)
from service import (
    AgentDocuSignRecipient,
    EmployeeDocuSignRecipient,
    get_docusign_transport,
    create_envelope
)
from templates.fpp import FPPTemplate
from templates.fpp_replacement import FPPReplacementFormTemplate
from documents.extra_children import ChildAttachmentForm
from documents.additional_replacement_policies import AdditionalReplacementPoliciesForm
from taa.services.products import ProductService
from taa.services.agents import AgentService

product_service = ProductService()
agent_service = AgentService()



def create_envelope_and_get_signing_url(wizard_data, census_record, case):

    enrollment_data = EnrollmentDataWrap(wizard_data, census_record, case)

    # Product code
    #product = product_service.get(wizard_data['product_data']['id'])
    productType = wizard_data["product_type"]
    is_fpp = ('fpp' in productType.lower())

    # If FPP Product, use the new docusign code, otherwise use old path
    if is_fpp:
        return create_fpp_envelope_and_fetch_signing_url(enrollment_data, case)
    else:
        return old_create_envelope_and_get_signing_url(enrollment_data)



def create_fpp_envelope_and_fetch_signing_url(enrollment_data, case):

    is_error = False
    error_message = None

    #owner_agent = enrollment_data.census_record.case.owner_agent if enrollment_data.census_record else agent_service.get_logged_in_agent()

    logged_in_agent = agent_service.get_logged_in_agent()

    if not logged_in_agent:
        logged_in_agent = case.owner_agent

    agent = AgentDocuSignRecipient(name=logged_in_agent.name(),
                                   email=logged_in_agent.email)
    employee = EmployeeDocuSignRecipient(name=enrollment_data.get_employee_name(),
                                         email=enrollment_data.get_employee_email())
    recipients = [
        agent,
        employee,
        # TODO Check if BCC's needed here
    ]

    fpp_form = FPPTemplate(recipients, enrollment_data)

    # Build the components (sections) needed for signing
    # Main form
    components = [fpp_form]

    # Additional Children
    if fpp_form.is_child_attachment_form_needed():
        child_attachment_form = ChildAttachmentForm(recipients, enrollment_data)

        for i, child in enumerate(fpp_form.get_attachment_children()):
            child.update(dict(
                coverage=format(enrollment_data['child_coverages'][i+2]['face_value'], ",.0f"),
                premium=format(enrollment_data['child_coverages'][i+2]['premium'], ".2f")
            ))
            child_attachment_form.add_child(child)

        components.append(child_attachment_form)

    # Replacement Form
    if fpp_form.is_replacement_form_needed():
        replacement_form = FPPReplacementFormTemplate(recipients, enrollment_data)
        components.append(replacement_form)

    if fpp_form.is_additional_replacment_policy_attachment_needed():
        components.append(AdditionalReplacementPoliciesForm(recipients, enrollment_data))


    transport = get_docusign_transport()
    envelope_result = create_envelope(email_subject="Signature needed: " + enrollment_data.get_product_code() + " for " +  enrollment_data.get_employee_name() + " (" + enrollment_data.get_employer_name() + ")",
                                      components=components,
                                      docusign_transport=transport,
                                     )

    redirect_url = envelope_result.get_signing_url(
        employee,
        callback_url=build_callback_url(enrollment_data, enrollment_data.get_session_type()),
        docusign_transport=transport
    )

    return is_error, error_message, redirect_url


def generate_SOHRadios(prefix, soh_questions):
    
    radioList = []
    for i, soh_question in enumerate(soh_questions):
        if soh_question['answer'] and soh_question['answer'].upper() == "GI":
            # GI - skip for now
            selected = "False"
            answer = "GI"
        else:
            selected = "True"
            answer = "no"
        
        radioList.append(
            {"groupName": prefix + "SOH" + str(i+1),
             "radios": [{"selected" : selected,
                         "value" : answer}]
            }
        )
        
    return radioList

def generate_SOH_GI_tabs(prefix, soh_questions):
    tabs = []
    for i, soh_question in enumerate(soh_questions):
        if soh_question['answer'] and soh_question['answer'].upper() == "GI":
            # GI - skip for now
            answer = "GI"
        else:
            answer = ""

        tabs.append(make_tab("{prefix}SOH{i}gi".format(prefix=prefix, i=i+1), answer))

    return tabs

def generate_ChildGenderRadio(child_index, wizard_data):
    return {"groupName": "child" + str(child_index + 1) + "Gender",
            "radios": [
                {"selected" : "True" if wizard_data["children"][child_index]["gender"] == "male" else "False",
                 "value" : "male"},
                {"selected" : "True" if wizard_data["children"][child_index]["gender"] == "female" else "False",
                 "value" : "female"}
            ]}

def generate_ChildGenderAbbrevTab(child_index, wizard_data):
    if wizard_data["children"][child_index]["gender"] == "male": 
        genderAbbrev = "M"
    elif wizard_data["children"][child_index]["gender"] == "female":
        genderAbbrev = "F"
    else:
        genderAbbrev = ""

    return {"tabLabel" : "child" + str(child_index + 1) + "GenderAbbrev",
            "value" : genderAbbrev}


def generate_ChildTabsEntry (child_index, wizard_data):
    childStr = "child" + str(child_index +1)
    child_coverage = wizard_data["child_coverages"][child_index]
    tabsList = [
        # FullName is only used for child >2, otherwise FName and LName on child <=2
        #  assuming for now the Docusign API will ignore those tabs not used in the template
        {"tabLabel" : childStr + "FullName",
         "value" : wizard_data["children"][child_index]["first"] + " " + wizard_data["children"][child_index]["last"]},
        {"tabLabel" : childStr + "FName",
         "value" : wizard_data["children"][child_index]["first"]},
        {"tabLabel" : childStr + "LName",
         "value" : wizard_data["children"][child_index]["last"]},
        {"tabLabel" : childStr + "DOB",
         "value" : wizard_data["children"][child_index]["birthdate"]},
        {"tabLabel" : childStr + "SSN",
         "value" : wizard_data["children"][child_index]["ssn"]},
        {"tabLabel" : childStr + "Coverage",
         "value" : format(child_coverage["face_value"], ",.0f") if child_coverage else "" },
        {"tabLabel" : childStr + "Premium",
         "value" : 
             format(child_coverage["premium"]*52/12, ",.2f") if child_coverage else ""
        
        },
    ]
    return tabsList
    



class EnrollmentDataWrap(object):
    def __init__(self, wizard_data, census_record, case):
        self.data = wizard_data
        self.census_record = census_record
        self.case = case

    def __getitem__(self, item):
        "Allow dict access to raw data"
        return self.data[item]

    def __setitem__(self, item, val):
        self.data[item] = val
    
    def __contains__(self, item):
        return item in self.data

    def get(self, key, default=None):
        return self.data.get(key, default)

    def is_self_enroll(self):
        return not self.data["agent_data"]["is_in_person"]

    def get_session_type(self):
        if self.data["agent_data"]["is_in_person"]:
            return "inperson"
        else:
            return "email"

    def get_id_type(self):
        if self.data["identityType"]:
            return self.data["identityType"]
        else:
            return "email"

    def get_id_token(self):
        if self.data["identityToken"]:
            return self.data["identityToken"]
        else:
            return self.get_employee_email()

    def get_agent_signing_name(self):
        # TODO: need to get proper agent for self-enroll.
        if not flask.has_request_context():
            return "(TEST)"

        # TODO: Need to store all agent custom_data in database so we can get to
        #   it when not logged in (ie, self enroll)
        if not user or user.is_anonymous():
            return ""

        if 'signing_name' not in user.custom_data:
            return user.full_name
        return user.custom_data["signing_name"]

    def get_agent_code(self):
        # TODO: need to get proper agent for self-enroll.
        if not flask.has_request_context():
            return "(TEST)"

        # TODO: Need to store all agent custom_data in database so we can get to
        #   it when not logged in (ie, self enroll)
        if not user or user.is_anonymous():
            return ""
            #return self.case.owner_agent.

        if 'agent_code' not in user.custom_data:
            return "HOME OFFICE"
        return user.custom_data["agent_code"]

    def get_employer_name(self):
        if self.census_record and self.census_record.case:
            return self.census_record.case.company_name
        else:
            return self.data['agent_data']["company_name"]

    def get_product_code(self):
        return self.data['product_type']

    def get_employee_name(self):
        return self.data["employee"]["first"] + " " + self.data["employee"]["last"]

    def get_employee_email(self):
        emailTo = self.data["employee"]["email"]

        if not emailTo:
            # fallback email if none was entered - just need a unique address
            emailTo = self.random_email_id(self.data["employee"]["first"] + "." + self.data["employee"]["last"]) + "@5StarEnroll.com"

        return emailTo

    def get_employee_email_parts(self):
        if '@' not in self.get_employee_email():
            return '', self.get_employee_email()
        else:
            return self.get_employee_email().split('@', 1)

    def random_email_id(self, name='', token_length=8):
        chars = "ABCDEF0123456789"
        name = filter(lambda x: x in ascii_letters + ".",name)
        if name != '':
            name = name + "_"
        return name + ''.join([random.choice(chars) for i in range(token_length)])


    def did_employee_select_coverage(self):
        return self.data["employee_coverage"] and self.data["employee_coverage"]["face_value"]

    def get_employee_coverage(self):
        return format(self.data["employee_coverage"]["face_value"], ",.0f")

    def get_formatted_employee_premium(self):
        return self.format_money(self.get_employee_premium())

    def get_employee_premium(self):
        return self.data["employee_coverage"]["premium"]

    def did_spouse_select_coverage(self):
        return self.data["spouse_coverage"] and self.data["spouse_coverage"]["face_value"]

    def get_spouse_coverage(self):
        return format(self.data["spouse_coverage"]["face_value"], ",.0f")

    def get_formatted_spouse_premium(self):
        return self.format_money(self.get_spouse_premium())

    def get_spouse_premium(self):
        return self.data["spouse_coverage"]["premium"]

    def format_money(self, amount):
        return "%.2f"%amount

    def get_num_covered_children(self):
        return len(self.get_covered_children())

    def get_covered_children(self):

        covered_children = []
        for i, child in enumerate(self.data['children']):
            coverage = self.data['child_coverages'][i]
            if coverage and coverage["face_value"]:
                covered_children.append(child)

        return covered_children


def old_create_envelope_and_get_signing_url(enrollment_data):
    # return is_error(bool), error_message, and redirectURL

    product = product_service.get(enrollment_data['product_data']['id'])

    # Product code
    productType = enrollment_data["product_type"]
    enrollmentState = enrollment_data["agent_data"]["state"]

    recipName = enrollment_data.get_employee_name()
    employer = enrollment_data.get_employer_name()
    emailTo = enrollment_data.get_employee_email()
    sessionType = enrollment_data.get_session_type()
    idType = enrollment_data.get_id_type()
    idToken = enrollment_data.get_id_token()
    idTokenStr = "Authentication via " + idType + ": " + idToken
    
    SOH_RadiosList = []
    SOH_GI_Tabs = []
    
    if recipName != "" and recipName != None:
        recipientName = recipName
    else:
        recipientName = "Applicant"
    
    eeCoverageNullToken = "NONE"
    if enrollment_data["employee_coverage"]:
        if enrollment_data["employee_coverage"]["face_value"]:
            employeeCoverage = format(enrollment_data["employee_coverage"]["face_value"], ",.0f")
            eePremium = format(round((enrollment_data["employee_coverage"]["premium"]*100 * 52) / 12)/100.0, ",.2f")
            SOH_RadiosList += generate_SOHRadios("ee", enrollment_data["employee"]["soh_questions"])
            SOH_GI_Tabs += generate_SOH_GI_tabs("ee", enrollment_data["employee"]["soh_questions"])
        else:
            employeeCoverage = eeCoverageNullToken
            eePremium = " "
    else:
        employeeCoverage = eeCoverageNullToken
        eePremium = " "

    
    if enrollment_data["spouse_coverage"]:
        if enrollment_data["spouse_coverage"]["face_value"]:
            spouseCoverage = format(enrollment_data["spouse_coverage"]["face_value"], ",.0f")
            spPremium = format(round((enrollment_data["spouse_coverage"]["premium"]*100 * 52) / 12)/100.0, ",.2f")
            SOH_RadiosList += generate_SOHRadios("sp", enrollment_data["spouse"]['soh_questions'])
            SOH_GI_Tabs += generate_SOH_GI_tabs("sp", enrollment_data["spouse"]["soh_questions"])
        else:
            spouseCoverage = " "
            spPremium = " "
    else:
        spouseCoverage = " "
        spPremium = " "
      
    
    childTabsList = []
    childRadiosList = []
    for i, child in enumerate(enrollment_data['children']):
        if not enrollment_data['children'][i] or not enrollment_data['child_coverages'][i]:
            continue
        childTabsList += generate_ChildTabsEntry(i, enrollment_data)
        childRadiosList.append(generate_ChildGenderRadio(i, enrollment_data))
        childRadiosList += generate_SOHRadios("c%s"%(i+1), enrollment_data["children"][i]['soh_questions'])
        SOH_GI_Tabs += generate_SOH_GI_tabs("c%s"%(i+1), enrollment_data["children"][i]["soh_questions"])


    agent_code = enrollment_data.get_agent_code()
    agent_signing_name = enrollment_data.get_agent_signing_name()

    eeTabsList = make_applicant_tabs("ee", enrollment_data['employee'])
    eeTabsList += [
        make_tab('eeEnrollCityState', enrollment_data["enrollCity"] + ", " + enrollment_data["enrollState"]),
        make_tab('identityToken', idTokenStr),
        make_tab('agentCode', agent_code),
        make_tab('agentSignName', agent_signing_name),
        make_tab('eeCoverage', employeeCoverage),
        make_tab('eePremium', eePremium if employeeCoverage != eeCoverageNullToken else ""),
        make_tab('Employer', enrollment_data["agent_data"]["company_name"]),
        make_tab('eeOtherOwnerName', enrollment_data["employee_other_owner_name"] if enrollment_data["employee_owner"] == "other" else  ""),
        make_tab('eeOtherOwnerName2', enrollment_data["employee_other_owner_name"] if enrollment_data["employee_owner"] == "other" else  ""),
        make_tab('eeOtherOwnerSSN', enrollment_data["employee_other_owner_ssn"] if enrollment_data["employee_owner"] == "other" else  ""),
        make_tab('eeEmail', enrollment_data.get_employee_email())
        #make_tab('eeEmailPart1', ee_email_part_1),
        #make_tab('eeEmailPart2', ee_email_part_2),
    ]
    
    eeTabsList += make_contact_tabs('ee', enrollment_data["employee"])
    
    def make_beneficiary_tabs(prefix, name, relationship, dob, ssn):
        return [
            make_tab(prefix+"BeneFullName", name),
            make_tab(prefix+"BeneRelationship", relationship),
            make_tab(prefix+"BeneDOB", dob),
            make_tab(prefix+"BeneSSN", ssn),
        ]
    
    # add in beneficiaries if appropriate
    if employeeCoverage != eeCoverageNullToken:
        if enrollment_data["employee_beneficiary"] == "spouse":
            eeTabsList += make_beneficiary_tabs(
                prefix="ee",
                name=enrollment_data["spouse"]["first"] + " " + enrollment_data["spouse"]["last"],
                relationship="spouse",
                dob=enrollment_data["spouse"]["birthdate"],
                ssn=enrollment_data["spouse"]["ssn"],
            )
        else:
            eeTabsList += make_beneficiary_tabs(
                prefix = "ee",
                name = enrollment_data["employee_beneficiary_name"],
                relationship = enrollment_data["employee_beneficiary_relationship"],
                dob = enrollment_data["employee_beneficiary_dob"],
                ssn = enrollment_data["employee_beneficiary_ssn"],
            )

        # TODO: Add in ee contingent beneficiary once the tab names are known
    
    if enrollment_data["spouse_owner"] == "other":
        spouseOtherOwnerName = enrollment_data["spouse_other_owner_name"]
        spouseOtherOwnerSSN = enrollment_data["spouse_other_owner_ssn"]
    elif enrollment_data["spouse_owner"] == "employee":
        spouseOtherOwnerName = enrollment_data["employee"]["first"] + " " + enrollment_data["employee"]["last"]
        spouseOtherOwnerSSN = enrollment_data["employee"]["ssn"]
    else:
        spouseOtherOwnerName = ""
        spouseOtherOwnerSSN = ""
        
    
    spouseTabsList = []
    if spouseCoverage != " ":
        spouseTabsList += make_applicant_tabs("sp", enrollment_data["spouse"])
        spouseTabsList += [
            {"tabLabel" : "spOtherOwnerName",
             "value" : spouseOtherOwnerName},
            {"tabLabel" : "spOtherOwnerSSN",
             "value" : spouseOtherOwnerSSN},
            {"tabLabel" : "spCoverage",
             "value" : spouseCoverage},
            {"tabLabel" : "spPremium",
             "value" : spPremium}
        ]
        if enrollment_data["spouse_beneficiary"] == "spouse":
            spouseTabsList += make_beneficiary_tabs(
                prefix="sp",
                name=enrollment_data["employee"]["first"] + " " + enrollment_data["employee"]["last"],
                relationship="spouse",
                dob=enrollment_data["employee"]["birthdate"],
                ssn=enrollment_data["employee"]["ssn"],
            )
        else:
            spouseTabsList += make_beneficiary_tabs(
                prefix="sp",
                name=enrollment_data["spouse_beneficiary_name"],
                relationship=enrollment_data["spouse_beneficiary_relationship"],
                dob=enrollment_data["spouse_beneficiary_dob"],
                ssn=enrollment_data["spouse_beneficiary_ssn"],
            )

        # TODO: Add in spouse contingent beneficiary once the tab names are known

    generalRadiosList = []
    # Note: UI screens out any "yes" replacement - so all applications are "no" to replacement
    
    generalRadiosList.append({"groupName": "productType",
                              "radios": [
                                  {"selected" : "True",
                                   # FPPTI or FPPCI
                                   "value" : "FPPTI" if productType == "FPP-Gov" else productType
                                  }
                              ]})
    generalRadiosList.append({"groupName": "existingIns",
                              "radios": [
                                  {"selected" : "True",
                                   "value" : enrollment_data["existing_insurance"]}
                              ]})
    generalRadiosList.append({"groupName": "replace",
                              "radios": [
                                  {"selected" : "True",
                                   "value" : "no"}
                              ]})
    
    for (prefix_short, prefix_long) in {("ee", "employee"), ("sp", "spouse")}:
        generalRadiosList.append({"groupName": prefix_short + "Gender",
                              "radios": [
                                  {"selected" : "True" if enrollment_data[prefix_long] and enrollment_data[prefix_long]["gender"] == "male" else "False",
                                   "value" : "male"},
                                  {"selected" : "True" if enrollment_data[prefix_long] and enrollment_data[prefix_long]["gender"] == "female" else "False",
                                   "value" : "female"}
                              ]})
        if enrollment_data[prefix_long] and "is_smoker" in enrollment_data[prefix_long]:
            generalRadiosList.append(
                {"groupName": prefix_short + "Smoking",
                 "radios": [
                     {"selected": "True" if enrollment_data[prefix_long]["is_smoker"] else "False",
                      "value": "smoker"},
                     {"selected": "True" if not enrollment_data[prefix_long]["is_smoker"] else "False",
                      "value": "nonsmoker"}
                 ]}
            )
        
        # only include Owner checkbox if coverage was selected
        if ((prefix_short == "ee" and employeeCoverage != eeCoverageNullToken) or 
            (prefix_short == "sp" and spouseCoverage != " ")):
            generalRadiosList.append({"groupName": prefix_short + "Owner",
                                      "radios": [
                                          {"selected" : "True" if enrollment_data[prefix_long + "_owner"] == "self" else "False",
                                           "value" : "self"},
                                          {"selected" : "True" if ((enrollment_data[prefix_long + "_owner"] == "other") or (enrollment_data[prefix_long + "_owner"] == "employee")) else "False",
                                           "value" : "other"}
                                      ]})
         
    agentRadiosList = []
    # identical to whatever EE said
    agentRadiosList.append({"groupName": "existingInsAgent",
                              "radios": [
                                  {"selected" : "True",
                                   "value" : enrollment_data["existing_insurance"]}
                              ]})
    agentRadiosList.append({"groupName": "replaceAgent",
                              "radios": [
                                  {"selected" : "True",
                                   "value" : "no"}
                              ]})
    


    # *******************************
    # Create envelope with an embedded recipient
    # NOTE:  this could be done via SOBO , in which case we'd need to follow the general procedure below.  However, 
    #        this seems to add more complexity and the benefit isn't clear what the Agent can do as a "sender" that he can't already (since we merely want him to be a signer).  Having him be able to access "sent" items before signed seems useless since the form is blank until after the employee signs and commits, so there's no data in there to resend.  
    #        Further, by the admin account being the sender, we have faster access into what envelopes are out/pending, etc.  So, for now we'll still avoiding SOBO on creating the envelopes (still need, elsewhere, for access to console for signing).
    # Nonetheless, here's the general notes on the process:
    """
    1. turn on "Sending" permission 
    
    POST to:
    https://{{environment}}.docusign.net/restapi/v2/accounts/{{AccountID}}/users/
    {
    "newUsers":[{
        "email": "exactemail@address.com",
        "userName":"Exact Name Match",
        "userSettings": [
            {
                "name": "canSendEnvelope",
                "value": "false"
            },
            {
                "name": "enableSequentialSigningAPI",
                "value": "false"
            }
          ]
    }]


    2. use SOBO authentication
       authenticateStr = dsAgentAuthenticateString() if sessionUserApprovedForDocusign() else dsAPIAuthenticateString()

    3. turn off "Sending" permissions (reverse the above)
    """
    # *******************************
    authenticateStr = dsAPIAuthenticateString()
 
    accountId = apiAccountID
        
    templateRoleName = "Employee"  # same role name that exists on the template in the console
    templateAgentRoleName = "Agent"

    # TODO: Enable GI tabs for other products once the forms are ready 
    if productType == "Group CI":
        giTabsList = SOH_GI_Tabs
    else:
        giTabsList = []
        
    # construct the body of the request in JSON format  
    requestBody = {
        "accountID" : accountId,
        "status" : "sent",
        "emailSubject": "signature needed: " + productType + " for " +  recipientName + " (" + employer + ")",
        "templateId": get_template_id(productType, enrollmentState),
        "templateRoles": [
            {"email" : emailTo,
             "name" :recipientName,
             "tabs" : {
                 "textTabs": eeTabsList + spouseTabsList + childTabsList + giTabsList,
                 "radioGroupTabs": generalRadiosList + SOH_RadiosList + childRadiosList 
             },
             "roleName" :  templateRoleName,
             "clientUserId": templateClientID 
             },
            {"email" : user.email,
             "name" : user.custom_data["signing_name"],
             "tabs" : {
                 "radioGroupTabs": agentRadiosList
             },
             "roleName" :  templateAgentRoleName
             #"clientUserId": templateClientID 
             } 
        ]
    }
    
    # Debug what we send to DocuSign
    import pprint
    pprint.pprint(requestBody)
    
    # Convert to JSON string
    requestBodyStr = json.dumps(requestBody)
    
    # append "/envelopes" to baseURL and use in the request
    url = baseUrl + "/envelopes"
    headers = {'X-DocuSign-Authentication': authenticateStr, 'Accept': 'application/json', 'Content-Length': str(len(requestBodyStr))}
    http = httplib2.Http()
    response, content = http.request(url, 'POST', headers=headers, body=requestBodyStr)
    
    # When troubleshooting, send instead to requestb.in (or similar listener) to capture/examing the JSON trace.  Past that trace into SOAPUI to explore the response if needed.
    #response, content = http.request("http://requestb.in/1mars8q1", 'POST', headers=headers, body=requestBodyStr);
    status = response.get('status')
    if (status != '201'): 
        print "url=",url
        print "headers=",headers
        print requestBodyStr
        print("Error generating Docusign envelope, status is: %s" % status); return True, "Error generating Docusign envelope", None
    
    data = json.loads(content)
    
    # store the uri for next request
    uri = data.get('uri')
    
    # write to log in case we need for short-term retrieval
    print ("Envelope for %s (%s) by %s: %s\n" % (recipientName, emailTo, user.custom_data["signing_name"], uri))
    
    
    #
    # Get the Embedded Send View
    #
    
    # construct the body of the request in JSON format  
    requestBody =   {
        "authenticationMethod" : "email",
        "email" : emailTo,
        "returnUrl" :  build_callback_url(enrollment_data, sessionType),
        "clientUserId" : templateClientID,
        "userName" : recipientName
    }
    
    requestBodyStr = json.dumps(requestBody)
    
    # append uri + "/views/recipient" to baseUrl and use in the request, don't need OnBehalfOf for this so just use API auth
    url = baseUrl + uri + "/views/recipient"
    headers = {'X-DocuSign-Authentication': dsAPIAuthenticateString(), 'Accept': 'application/json', 'Content-Length': str(len(requestBodyStr))}
    http = httplib2.Http()
    response, content = http.request(url, 'POST', headers=headers, body=requestBodyStr)
    status = response.get('status')
    
    # print ("response: %s\ncontent: %s" % (response, content))

    if (status != '201'):
        print "url=",url
        print "headers=",headers
        print requestBodyStr
        print("Error retrieving signature URL, status is: %s" % status); return True, "Error retrieving signature URL", None

    data = json.loads(content)
    viewUrl = data.get('url')


    return False, None, viewUrl


def construct_composite_envelope():
    """
    Builds a DocuSign composite envelope using a server template and one or more plain PDFs that we generate
    """

    import requests

    # DocuSign
    docusign_integrator_key = 'DELM-0d0ee159-7e61-499f-81ec-5c03bec86ec3'
    docusign_api_password = '12121212'
    docusign_api_username = 'cb64545b-0bb7-4e77-bb0c-492b02c3dd5b'
    docusign_api_account_id = '5988eb5b-bee1-4825-a078-dcac445a22ce'

    auth_string = "<DocuSignCredentials>" \
        "<Username>" + docusign_api_username + "</Username>" \
        "<Password>" + docusign_api_password + "</Password>" \
        "<IntegratorKey>" + docusign_integrator_key + "</IntegratorKey>" \
        "</DocuSignCredentials>"

    headers = {'X-DocuSign-Authentication': auth_string,
               'Accept': 'application/json',
    }

    from reportlab.pdfgen.canvas import Canvas
    from reportlab.lib.pagesizes import letter
    from reportlab.lib.units import inch
    page_width, page_height = letter
    import StringIO

    pdf_data = StringIO.StringIO()
    canvas = Canvas(pdf_data, pagesize=letter)
    canvas.drawString(inch, page_height - inch, "Testing Extra Page")
    canvas.save()

    import base64
    pdf_b64_data = base64.standard_b64encode(pdf_data.getvalue())

    tabs = {
         "textTabs": [make_tab('Employer', "DELMAR SD INC")],
         "radioGroupTabs": [],
    }
    agent_tabs = {
        "radioGroupTabs": [
            {"groupName": "existingInsAgent",
            "radios": [
                {"selected" : "True", "value" : "yes"}
            ]},
            {"groupName": "replaceAgent",
            "radios": [
                {"selected" : "True", "value" : "no"}
            ]}
        ]
    }

    recipients = {
        "signers": [
            {
                "name": "Zach Mason",
                "email": "zach@zachmason.com",
                "recipientId": "1",
                "routingOrder": "1",
                "roleName": "Employee",
                "templateRequired": True,
                "tabs": tabs,
                "clientUserId":"123456",
            },
            {
                "name": "Agent Mason",
                "email": "agent@zachmason.com",
                "recipientId": "2",
                "routingOrder": "2",
                "roleName": "Agent",
                "templateRequired": False,
                "tabs": agent_tabs,
            }
        ],
    }

    data = {
        "accountID" : docusign_api_account_id,
        "status" : "sent",
        "emailSubject": "testing: signature needed",
        "compositeTemplates": [
            {
                "serverTemplates":[
                    {
                        "templateId": '666F1F5B-77C6-47CC-AC85-1784B8569C3D',
                        "sequence": "1",
                    },
                ],
                "inlineTemplates":[
                    {
                        "sequence": "2",
                        "recipients": recipients,
                    }
                ]
            },
            {
                "document":{
                    "name": 'ExtraChildrenForm',
                    "sequence": "1",
                    "documentId": "1",
                    "pages": "1",
                    "fileExtension":"pdf",
                    "documentBase64":pdf_b64_data,
                },
                "inlineTemplates":[
                    {
                        "sequence": "1",
                        "recipients": recipients,
                    }
                ]
            },

        ],

    }

    import json
    docusign_base_url = "https://demo.docusign.net/restapi/v2/accounts/" + docusign_api_account_id
    docusign_envelope_url = docusign_base_url + "/envelopes"
    result = requests.post(docusign_envelope_url, data=json.dumps(data), headers=headers)

    print(result.json())

    envelope_uri = result.json()['uri']


    # Get the View URL
    data2 = dict(
        authenticationMethod="email",
        email="zach@zachmason.com",
        returnUrl="https://5starenroll.com",
        clientUserId="123456",
        userName="Zach Mason",
    )
    view_url = docusign_base_url + envelope_uri + "/views/recipient"
    result2 = requests.post(view_url, json=data2, headers=headers)

    print(result2.json())


def make_tab(name, val):
    return dict(tabLabel=name, value=val)


def make_radio_tab(group_name, selected, val):
    return {
        "groupName": group_name,
        "radios": [
            {
                "selected": "True" if selected else "False",
                "value": val,
            }
        ]
    }


def make_applicant_tabs(prefix, data):
    tabs = [
        make_tab(prefix + 'FName', data["first"]),
        make_tab(prefix + 'LName', data["last"]),
        make_tab(prefix + 'DOB', data["birthdate"]),
        make_tab(prefix + 'SSN', data["ssn"]),
    ]
    if data.get('height'):
        height_ft = "%s" % int(data['height'] / 12.0)
        height_in = "%s" % int(data['height'] % 12.0)

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
        make_tab(prefix + 'Street2', data['address2'] if 'address2' in data else ''),
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
    return "{scheme}{hostname}/application_completed?name={name}&type={session_type}".format(
        scheme=scheme,
        hostname=hostname,
        name=wizard_data["employee"]["first"], 
        session_type=session_type,
    )


if __name__ == "__main__":
    print("Constructing composite envelope...")
    #construct_composite_envelope()

    #data =

    #good_data = '''{"status": "sent", "compositeTemplates": [{"serverTemplates": [{"sequence": "1", "templateId": "E26A7761-1ACF-4993-A2A1-2D021B79E68C"}], "inlineTemplates": [{"recipients": {"signers": [{"templateRequired": false, "name": "Agent Mason", "routingOrder": "0", "tabs": {}, "roleName": "Agent", "recipientId": "0", "email": "agent@zachmason.com"}, {"templateRequired": true, "name": "Katherine Adams", "routingOrder": "1", "clientUserId": "123456", "tabs": {"radioGroupTabs": [{"groupName": "payment_mode", "radios": [{"selected": "True", "value": "monthly"}]}, {"groupName": "actively_at_work", "radios": [{"selected": "True", "value": "no"}]}, {"groupName": "eeSOH1", "radios": [{"selected": "True", "value": "no"}]}, {"groupName": "eeSOH2", "radios": [{"selected": "True", "value": "no"}]}, {"groupName": "eeSOH3", "radios": [{"selected": "True", "value": "no"}]}, {"groupName": "eeSOH4", "radios": [{"selected": "True", "value": "no"}]}, {"groupName": "eeSOH5", "radios": [{"selected": "True", "value": "no"}]}, {"groupName": "eeSOH6", "radios": [{"selected": "True", "value": "no"}]}, {"groupName": "eeSOH7", "radios": [{"selected": "True", "value": "no"}]}, {"groupName": "spSOH1", "radios": [{"selected": "True", "value": "no"}]}, {"groupName": "spSOH2", "radios": [{"selected": "True", "value": "no"}]}, {"groupName": "spSOH3", "radios": [{"selected": "True", "value": "no"}]}, {"groupName": "spSOH4", "radios": [{"selected": "True", "value": "no"}]}, {"groupName": "spSOH5", "radios": [{"selected": "True", "value": "no"}]}, {"groupName": "spSOH6", "radios": [{"selected": "True", "value": "no"}]}, {"groupName": "spSOH7", "radios": [{"selected": "True", "value": "no"}]}, {"groupName": "spouse_hospital_six_months", "radios": [{"selected": "True", "value": "no"}]}, {"groupName": "spouse_disability_six_months", "radios": [{"selected": "True", "value": "no"}]}, {"groupName": "child1Gender", "radios": [{"selected": "True", "value": "female"}]}, {"groupName": "c1SOH1", "radios": [{"selected": "True", "value": "no"}]}, {"groupName": "c1SOH2", "radios": [{"selected": "True", "value": "no"}]}, {"groupName": "c1SOH3", "radios": [{"selected": "True", "value": "no"}]}, {"groupName": "c1SOH4", "radios": [{"selected": "True", "value": "no"}]}, {"groupName": "c1SOH5", "radios": [{"selected": "True", "value": "no"}]}, {"groupName": "c1SOH6", "radios": [{"selected": "True", "value": "no"}]}, {"groupName": "c1SOH7", "radios": [{"selected": "True", "value": "no"}]}, {"groupName": "child2Gender", "radios": [{"selected": "True", "value": "male"}]}, {"groupName": "c2SOH1", "radios": [{"selected": "True", "value": "no"}]}, {"groupName": "c2SOH2", "radios": [{"selected": "True", "value": "no"}]}, {"groupName": "c2SOH3", "radios": [{"selected": "True", "value": "no"}]}, {"groupName": "c2SOH4", "radios": [{"selected": "True", "value": "no"}]}, {"groupName": "c2SOH5", "radios": [{"selected": "True", "value": "no"}]}, {"groupName": "c2SOH6", "radios": [{"selected": "True", "value": "no"}]}, {"groupName": "c2SOH7", "radios": [{"selected": "True", "value": "no"}]}, {"groupName": "child3Gender", "radios": [{"selected": "True", "value": "male"}]}, {"groupName": "c3SOH1", "radios": [{"selected": "True", "value": "no"}]}, {"groupName": "c3SOH2", "radios": [{"selected": "True", "value": "no"}]}, {"groupName": "c3SOH3", "radios": [{"selected": "True", "value": "no"}]}, {"groupName": "c3SOH4", "radios": [{"selected": "True", "value": "no"}]}, {"groupName": "c3SOH5", "radios": [{"selected": "True", "value": "no"}]}, {"groupName": "c3SOH6", "radios": [{"selected": "True", "value": "no"}]}, {"groupName": "c3SOH7", "radios": [{"selected": "True", "value": "no"}]}, {"groupName": "productType", "radios": [{"selected": "True", "value": "FPPCI"}]}, {"groupName": "existingIns", "radios": [{"selected": "True", "value": "no"}]}, {"groupName": "replace", "radios": [{"selected": "True", "value": "yes"}]}, {"groupName": "eeGender", "radios": [{"selected": "True", "value": "female"}]}, {"groupName": "eeSmoking", "radios": [{"selected": "True", "value": "nonsmoker"}]}, {"groupName": "eeOwner", "radios": [{"selected": "True", "value": "self"}]}, {"groupName": "spGender", "radios": [{"selected": "True", "value": "female"}]}, {"groupName": "spSmoking", "radios": [{"selected": "True", "value": "nonsmoker"}]}, {"groupName": "spOwner", "radios": [{"selected": "True", "value": "employee"}]}], "textTabs": [{"tabLabel": "employer", "value": "ABC"}, {"tabLabel": "group_number", "value": "ABC-XYZ-12334"}, {"tabLabel": "eeName", "value": "Katherine Adams"}, {"tabLabel": "eeFName", "value": "Katherine"}, {"tabLabel": "eeLName", "value": "Adams"}, {"tabLabel": "eeDOB", "value": "04/05/1955"}, {"tabLabel": "eeSSN", "value": "287-28-8500"}, {"tabLabel": "eeAddress", "value": ""}, {"tabLabel": "eeCity", "value": "Naples"}, {"tabLabel": "eeState", "value": "FL"}, {"tabLabel": "eeZip", "value": "34114"}, {"tabLabel": "eePhone", "value": "4-(566)054-7761"}, {"tabLabel": "eeEmail", "value": "kwoods25w@chron.com"}, {"tabLabel": "eeEnrollCityState", "value": "Indianapolis, IN"}, {"tabLabel": "eeEnrollCity", "value": "Indianapolis"}, {"tabLabel": "eeEnrollState", "value": "IN"}, {"tabLabel": "date_of_hire", "value": "12/01/1988"}, {"tabLabel": "agentCode", "value": "ABCDE"}, {"tabLabel": "agentSignName", "value": "SIGNING NAME"}, {"tabLabel": "Employer", "value": "ABC"}, {"tabLabel": "eeOtherOwnerName", "value": ""}, {"tabLabel": "eeOtherOwnerName2", "value": ""}, {"tabLabel": "eeOtherOwnerSSN", "value": ""}, {"tabLabel": "eeEmailPart1", "value": "kwoods25w"}, {"tabLabel": "eeEmailPart2", "value": "chron.com"}, {"tabLabel": "eeSOH1gi", "value": ""}, {"tabLabel": "eeSOH2gi", "value": ""}, {"tabLabel": "eeSOH3gi", "value": ""}, {"tabLabel": "eeSOH4gi", "value": ""}, {"tabLabel": "eeSOH5gi", "value": ""}, {"tabLabel": "eeSOH6gi", "value": ""}, {"tabLabel": "eeSOH7gi", "value": ""}, {"tabLabel": "spName", "value": "Joan Ray"}, {"tabLabel": "spFName", "value": "Joan"}, {"tabLabel": "spLName", "value": "Ray"}, {"tabLabel": "spDOB", "value": "05/07/1985"}, {"tabLabel": "spSSN", "value": "387-50-0865"}, {"tabLabel": "spSOH1gi", "value": ""}, {"tabLabel": "spSOH2gi", "value": ""}, {"tabLabel": "spSOH3gi", "value": ""}, {"tabLabel": "spSOH4gi", "value": ""}, {"tabLabel": "spSOH5gi", "value": ""}, {"tabLabel": "spSOH6gi", "value": ""}, {"tabLabel": "spSOH7gi", "value": ""}, {"tabLabel": "eeCoverage", "value": "25,000"}, {"tabLabel": "eePremium", "value": "76.48"}, {"tabLabel": "spCoverage", "value": "10,000"}, {"tabLabel": "spPremium", "value": "8.67"}, {"tabLabel": "eePremiumTotal", "value": "76.48"}, {"tabLabel": "spPremiumTotal", "value": "8.67"}, {"tabLabel": "childPremiumTotal", "value": "29.91"}, {"tabLabel": "totalAllPremium", "value": "115.06"}, {"tabLabel": "eeBeneFullName", "value": "Joan Ray"}, {"tabLabel": "eeBeneAge", "value": "30"}, {"tabLabel": "eeBeneRelationship", "value": "Spouse"}, {"tabLabel": "eeBeneDOB", "value": "05/07/1985"}, {"tabLabel": "eeBeneSSN", "value": "387-50-0865"}, {"tabLabel": "spBeneFullName", "value": "Test"}, {"tabLabel": "spBeneAge", "value": "9"}, {"tabLabel": "spBeneRelationship", "value": "asoentuh"}, {"tabLabel": "spBeneDOB", "value": "12/31/2005"}, {"tabLabel": "spBeneSSN", "value": "098-09-8098"}, {"tabLabel": "child1Name", "value": "Anna Roberts"}, {"tabLabel": "child1DOB", "value": "03/29/1995"}, {"tabLabel": "child1SSN", "value": "234-52-3453"}, {"tabLabel": "child1Coverage", "value": "20,000"}, {"tabLabel": "child1Premium", "value": "9.97"}, {"tabLabel": "c1SOH1gi", "value": ""}, {"tabLabel": "c1SOH2gi", "value": ""}, {"tabLabel": "c1SOH3gi", "value": ""}, {"tabLabel": "c1SOH4gi", "value": ""}, {"tabLabel": "c1SOH5gi", "value": ""}, {"tabLabel": "c1SOH6gi", "value": ""}, {"tabLabel": "c1SOH7gi", "value": ""}, {"tabLabel": "child2Name", "value": "Kelly Woods"}, {"tabLabel": "child2DOB", "value": "02/13/2007"}, {"tabLabel": "child2SSN", "value": "234-52-3452"}, {"tabLabel": "child2Coverage", "value": "20,000"}, {"tabLabel": "child2Premium", "value": "9.97"}, {"tabLabel": "c2SOH1gi", "value": ""}, {"tabLabel": "c2SOH2gi", "value": ""}, {"tabLabel": "c2SOH3gi", "value": ""}, {"tabLabel": "c2SOH4gi", "value": ""}, {"tabLabel": "c2SOH5gi", "value": ""}, {"tabLabel": "c2SOH6gi", "value": ""}, {"tabLabel": "c2SOH7gi", "value": ""}, {"tabLabel": "child3Name", "value": "Third Child"}, {"tabLabel": "child3DOB", "value": "01/01/2010"}, {"tabLabel": "child3SSN", "value": "123-12-1234"}, {"tabLabel": "child3Coverage", "value": "20,000"}, {"tabLabel": "child3Premium", "value": "9.97"}, {"tabLabel": "c3SOH1gi", "value": ""}, {"tabLabel": "c3SOH2gi", "value": ""}, {"tabLabel": "c3SOH3gi", "value": ""}, {"tabLabel": "c3SOH4gi", "value": ""}, {"tabLabel": "c3SOH5gi", "value": ""}, {"tabLabel": "c3SOH6gi", "value": ""}, {"tabLabel": "c3SOH7gi", "value": ""}]}, "roleName": "Employee", "recipientId": "1", "email": "zach@zachmason.com"}]}, "sequence": "2"}]}, {"serverTemplates": [{"sequence": "1", "templateId": "3E0CF882-8678-4476-A6B3-D60AA4111C85"}], "inlineTemplates": [{"recipients": {"signers": [{"templateRequired": false, "name": "Agent Mason", "routingOrder": "0", "tabs": {}, "roleName": "Agent", "recipientId": "0", "email": "agent@zachmason.com"}, {"templateRequired": true, "name": "Katherine Adams", "routingOrder": "1", "clientUserId": "123456", "tabs": {"radioGroupTabs": [{"groupName": "read_aloud", "radios": [{"selected": "True", "value": "no"}]}, {"groupName": "considering_terminating_existing", "radios": [{"selected": "True", "value": "yes"}]}, {"groupName": "considering_using_funds", "radios": [{"selected": "True", "value": "yes"}]}], "textTabs": [{"tabLabel": "policy_insurer_name", "value": "Test Name"}, {"tabLabel": "policy_number", "value": "Contract #12345"}, {"tabLabel": "policy_insured", "value": "Myself"}, {"tabLabel": "policy_replaced_or_financing", "value": "R"}, {"tabLabel": "policy_reason", "value": "Just because..."}, {"tabLabel": "eeName", "value": "Katherine Adams"}]}, "roleName": "Employee", "recipientId": "1", "email": "zach@zachmason.com"}]}, "sequence": "2"}]}, {"document": {"name": "ChildAttachmentForm", "sequence": "1", "fileExtension": "pdf", "documentBase64": "JVBERi0xLjQNCiWTjIueIFJlcG9ydExhYiBHZW5lcmF0ZWQgUERGIGRvY3VtZW50IGh0dHA6Ly93d3cucmVwb3J0bGFiLmNvbQ0KMSAwIG9iag0KPDwgL0YxIDIgMCBSIC9GMiAzIDAgUiAvRjMgNCAwIFIgPj4NCmVuZG9iag0KMiAwIG9iag0KPDwgL0Jhc2VGb250IC9IZWx2ZXRpY2EgL0VuY29kaW5nIC9XaW5BbnNpRW5jb2RpbmcgL05hbWUgL0YxIC9TdWJ0eXBlIC9UeXBlMSAvVHlwZSAvRm9udCA+Pg0KZW5kb2JqDQozIDAgb2JqDQo8PCAvQmFzZUZvbnQgL0hlbHZldGljYS1Cb2xkIC9FbmNvZGluZyAvV2luQW5zaUVuY29kaW5nIC9OYW1lIC9GMiAvU3VidHlwZSAvVHlwZTEgL1R5cGUgL0ZvbnQgPj4NCmVuZG9iag0KNCAwIG9iag0KPDwgL0Jhc2VGb250IC9UaW1lcy1Sb21hbiAvRW5jb2RpbmcgL1dpbkFuc2lFbmNvZGluZyAvTmFtZSAvRjMgL1N1YnR5cGUgL1R5cGUxIC9UeXBlIC9Gb250ID4+DQplbmRvYmoNCjUgMCBvYmoNCjw8IC9Db250ZW50cyAxMCAwIFIgL01lZGlhQm94IFsgMCAwIDYxMiA3OTIgXSAvUGFyZW50IDkgMCBSIC9SZXNvdXJjZXMgPDwgL0ZvbnQgMSAwIFIgL1Byb2NTZXQgWyAvUERGIC9UZXh0IC9JbWFnZUIgL0ltYWdlQyAvSW1hZ2VJIF0gPj4gL1JvdGF0ZSAwIC9UcmFucyA8PCAgPj4gDQogIC9UeXBlIC9QYWdlID4+DQplbmRvYmoNCjYgMCBvYmoNCjw8IC9Db250ZW50cyAxMSAwIFIgL01lZGlhQm94IFsgMCAwIDYxMiA3OTIgXSAvUGFyZW50IDkgMCBSIC9SZXNvdXJjZXMgPDwgL0ZvbnQgMSAwIFIgL1Byb2NTZXQgWyAvUERGIC9UZXh0IC9JbWFnZUIgL0ltYWdlQyAvSW1hZ2VJIF0gPj4gL1JvdGF0ZSAwIC9UcmFucyA8PCAgPj4gDQogIC9UeXBlIC9QYWdlID4+DQplbmRvYmoNCjcgMCBvYmoNCjw8IC9PdXRsaW5lcyAxMiAwIFIgL1BhZ2VNb2RlIC9Vc2VOb25lIC9QYWdlcyA5IDAgUiAvVHlwZSAvQ2F0YWxvZyA+Pg0KZW5kb2JqDQo4IDAgb2JqDQo8PCAvQXV0aG9yIChcKGFub255bW91c1wpKSAvQ3JlYXRpb25EYXRlIChEOjIwMTUwNjA2MDUzOTI1KzAwJzAwJykgL0NyZWF0b3IgKFwodW5zcGVjaWZpZWRcKSkgL0tleXdvcmRzICgpIC9Qcm9kdWNlciAoUmVwb3J0TGFiIFBERiBMaWJyYXJ5IC0gd3d3LnJlcG9ydGxhYi5jb20pIC9TdWJqZWN0IChcKHVuc3BlY2lmaWVkXCkpIA0KICAvVGl0bGUgKFwoYW5vbnltb3VzXCkpID4+DQplbmRvYmoNCjkgMCBvYmoNCjw8IC9Db3VudCAyIC9LaWRzIFsgNSAwIFIgNiAwIFIgXSAvVHlwZSAvUGFnZXMgPj4NCmVuZG9iag0KMTAgMCBvYmoNCjw8IC9GaWx0ZXIgWyAvQVNDSUk4NURlY29kZSAvRmxhdGVEZWNvZGUgXSAvTGVuZ3RoIDE1OTkgPj4NCnN0cmVhbQ0KR2IiLyc+dTBLPydSYEwyNT9xbm0+JC4lJDQ1W11QUW5lZDFhIjBsYUgvY2RnVkRUV0JgXnM6RF5PRUFXVWtESzlKVUpJXCwzOFNBaGc5ZCJJLWNOOk1nclxFOkNEXTY7YE9cS0o1RFdpITpfJ09WTFFDWmwwY2oraCZYcFBVRFAyQSwyRG4nW1RDZHVHQW8qOVxqSF1rbGNRVzxXLF0wXDlQVUhgMUZrbC1KLiZKb1QlNjUiXEIoKGtqMEhsdGIiVGk+OVErQTlNaUlZRElAZUZoaiMtQV1mJmMvYU5caCYlOyNnYnIyZnNsNXJsU1pUUCw3Jko4YygoK0ZTQCprQiM0cC5DUjJrTlYySFE4Y1VGRj4wIT9cIidxRUdkbSFXJDYrYFRpPGxoO19qa3RKOTRqLzhXcWVVbSwpO2x1TVojYF8tMFwpdVg8cyI+M1JLJjldb2s/OkNkYHUtQldHXitIbV5hQlVzR0FESGczSFJWK2d0b0N1Wmg8V0A9OWAuZ0s8YCNOUmEoVzlBOmglSHBhIllbYmcjV0MoKlFOI1VwNFJDISdecGYkdC1URDM0c2xtQDBQUD0sQXVOOTk6cUVpUVZGdDxHL3Q7P2IlOFRjInQ3RzcjR2QtKCdZOW1SJCotIXI1aV1XZEsmcyliJ1E0IUZJK0xyOW0iT1NVQlNvTSVhLFZVUzc9KjlcV1BxUW82PixacUhiTk40UzVBWyk6amJCVCcwTDkqMF5AU0c1JHRCLGpIO2xYVyhZbz9rUURXcmNlbm5KOCQ5SDFsMzJfUHBbZUVIMyhZcTA9c2o1XHJuUyYhYkZudDdkZ0RFWFUyNSpwTHVsWFJlTFlFU0lCV09UZGlINU1eY0RdRz8/VGhibGgzS0olVkB1XEg1bzFoPkBQajpXRzt1WyZOLSljW15RIUZVRWhVOzI3QllKWVhPQ1dTQCxjTDYwYichVmYjX1BBYUtdcUBqYlRkLTM8TC5ZPmw1dVJWRkFMVFBpXEhaYjJsSicsIU9kKmZQaGg8Vjs7TlcqLCZYZCkyZzZTNGdwK1JuNldNJSRaS01GJWUkOGxOSnNqa0RNcy5LI3MtNG5dRnMrPEphbzdGJExtMkshVj1eYjYpSkldSTRcdDE0RmtOXDRkXy48VHA8M1gxYmsyWT8xVEczKTc0RzZERzIlakRhW0dTMUozSjxQXThwSSIhaCpCVC1ZbyNGQFAzM0ElImInY21uYF8mdERPPThUYU0vUXEuWC0kdTNWZz9uTkMnNlA4Q2BCL3RPVk0hLlljRW08WixoI3UySW8wdWVZSDgqRHJmJmBxUVtpXihQY1VtRUBEU0E3N1BPayY+XVhLcEFOO1lNPEBSRVAsUT5bJDgpZktdbVhmKS1jMSw4TyxpTkZJYlQuRiZtXWVqLFFeViFUKDhYMEJnVGUyNEEhZUhRNjhIYWVlI0BvSzIsIUMtOnQ4YipuXDRhREpPcnA+MjEhOjhpXDsoYFopWiNmNkFsR084aDgiP25tQyIpNjVSLXJLcS1nWTxmTk5HMzMyZ3QkQmpuTEpoJl9LQGVbcDtpLTo6QyYxT1BuVWc5MGZPWSEhN0JHbzhyIl1dQDlmPlMnXWdHMCpoTjZxLUlmZjNSSmIkOTY5bUpMRDY7SjkrM2VyLnQkKj8sJC84Rm4vcUBLS1M9P0VnT09vS3JFIl86PXRXMEF0YXIlb1xMVD1eXkIpOS8tNmw/b2IrWy9cWj1NXXQoWjMrPSlnVVBOZDhQbkBcS3E+SEozK19CMkUyTnE2NXA8QSg7Z2klczM1WDpoblJeb2ouNmNgZ0FeTk4hMkkwLyU8YUQ0NCctTmoybk5IRUciZlYzdTFcOHUvcE9OQ05HImRvTDhEZGlYS14tRldgJUBBbzssIVElLDBqaUVyPDgvMHE5aUcxPi1ePFRcOV0rYWJ1QylROEhIJUoyTjRdOUIoK24mUWYnSD5fNCJsQGgsLk11LEVccjYqMTkwL3BLNDhXJm82PS5JQWY9XkIzaUtrQW0xaVhbS01lX1EtT2xVcVVeOG0rWjU1P3RcZUtaaitpbEE3VUs6bWNjKigjJ2lLTUlvXUtYPipyZlNsUjsyXlZnZSVEK18lIVckQV9rUH4+ZW5kc3RyZWFtDQplbmRvYmoNCjExIDAgb2JqDQo8PCAvRmlsdGVyIFsgL0FTQ0lJODVEZWNvZGUgL0ZsYXRlRGVjb2RlIF0gL0xlbmd0aCAyNzQgPj4NCnN0cmVhbQ0KR2FybzxiNmwqPyY0UT5FcjUyZz9NcVVhPFhcL2c0ZTppdE1gJ3IpTS1zI2o9cms/S2ZDX1MuJyhtOkNMUDlHVGsmXV0jI29KRSE9IVwqPWItazlfTSQyUmhfTVxJdC1vSHI5QFAjclRYOlI8U1FcPDJDPSNTXSV0KikxWksmTWM/ZzxdKzJhSFcwaTI/ZHM0MlpkRGhaTnFXKmA6WjlKNjQnPGFuJCdoQUczRl5yMmpMaUZmLi85MjA/O3JNRS1bbiJjNnVBSU1wXEZLJGIwZiRVamZLInU1Vz4oIkgvIjIjK0RwcV1SXj8vJidpQDRHMkciTiwqUnU7azlYQSI3TCU/KUZWYExWdSFkOz1fV1d+PmVuZHN0cmVhbQ0KZW5kb2JqDQoxMiAwIG9iag0KPDwgL0NvdW50IDAgL1R5cGUgL091dGxpbmVzID4+DQplbmRvYmoNCnhyZWYNCjAgMTMNCjAwMDAwMDAwMDAgNjU1MzUgZg0KMDAwMDAwMDA3NSAwMDAwMCBuDQowMDAwMDAwMTI5IDAwMDAwIG4NCjAwMDAwMDAyMzkgMDAwMDAgbg0KMDAwMDAwMDM1NCAwMDAwMCBuDQowMDAwMDAwNDY2IDAwMDAwIG4NCjAwMDAwMDA2NjQgMDAwMDAgbg0KMDAwMDAwMDg2MiAwMDAwMCBuDQowMDAwMDAwOTUwIDAwMDAwIG4NCjAwMDAwMDExODYgMDAwMDAgbg0KMDAwMDAwMTI1NCAwMDAwMCBuDQowMDAwMDAyOTUwIDAwMDAwIG4NCjAwMDAwMDMzMjAgMDAwMDAgbg0KdHJhaWxlcg0KPDwgL0lEIA0KICUgUmVwb3J0TGFiIGdlbmVyYXRlZCBQREYgZG9jdW1lbnQgLS0gZGlnZXN0IChodHRwOi8vd3d3LnJlcG9ydGxhYi5jb20pDQogWyg2M1wzNjNcMjc1XDIwNVwzMDVcMjI2QnhaXDIzNVwzNjV9XDM0MFwzMTR3KSAoNjNcMzYzXDI3NVwyMDVcMzA1XDIyNkJ4WlwyMzVcMzY1fVwzNDBcMzE0dyldDQogL0luZm8gOCAwIFIgL1Jvb3QgNyAwIFIgL1NpemUgMTMgPj4NCnN0YXJ0eHJlZg0KMzM3MA0KJSVFT0YNCg==", "pages": "2", "documentId": "1"}, "inlineTemplates": [{"recipients": {"carbonCopies": [{"templateRequired": false, "name": "Agent Mason", "routingOrder": "0", "tabs": {}, "roleName": "Agent", "recipientId": "0", "email": "agent@zachmason.com"}], "signers": [{"templateRequired": true, "name": "Katherine Adams", "routingOrder": "1", "clientUserId": "123456", "tabs": {"signHereTabs": [{"xPosition": 78.0, "yPosition": 78.0, "pageNumber": "2", "documentId": "1"}]}, "roleName": "Employee", "recipientId": "1", "email": "zach@zachmason.com"}]}, "sequence": "1"}]}], "emailSubject": "Signature needed", "accountID": "8271282c-7a4e-4e00-a2e9-878924c316d5"}'''
    #import requests
    #r = requests.post('https://na2.docusign.net/restapi/v2/accounts/8271282c-7a4e-4e00-a2e9-878924c316d5/envelopes',
    #                  data=json.dumps(data),
    #                  headers={'Accept': 'application/json', 'X-DocuSign-Authentication': '<DocuSignCredentials><Username>8dd0f65d-ae78-4026-8d32-81f63818bf16</Username><Password>edJSzv7Rqc2XNFI3GqM/IrZ9SvM=</Password><IntegratorKey>STAR-0baef057-d5b4-46bd-831f-e8e66f271aa7</IntegratorKey></DocuSignCredentials>'})
    #print r
    #print r.text