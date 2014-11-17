# DocuSign API Walkthrough 08 (PYTHON) - Embedded Signing
import sys, httplib2, json;
import random
from string import ascii_letters

from flask import url_for
from flask.ext.stormpath import user
from taa.model.DocuSign_config import (
    dsAPIAuthenticateString, 
    dsAgentAuthenticateString, 
    baseUrl, 
    apiAccountID, 
    templateClientID,
    sessionUserApprovedForDocusign,
    get_template_id
)


#  14-May-23 WSD for now, by the time we get here the radios are wholesale "no" answers, so just fill without logic 
def generate_SOHRadios(prefix):
    radioList = []
    for i in range(7):
        radioList.append(
            {"groupName": prefix + "SOH" + str(i+1),
             "radios": [{"selected" : "True",
                         "value" : "no"}]}
            )
    return radioList

def generate_BeneficiaryTabs(prefix):
    
    beneTabs = []
    for i in range(7):
        beneTabs.append(
            {"groupName": prefix + "SOH" + str(i+1),
             "radios": [{"selected" : "True",
                         "value" : "no"}]}
            )
    return beneTabs


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
         "value" : format(wizard_data["child_coverages"][child_index]["face_value"], ",.0f")},
        {"tabLabel" : childStr + "Premium",
         "value" : format(wizard_data["child_coverages"][child_index]["weekly_premium"]*52/12, ",.2f")},
    ]
    return tabsList
    

    
def random_email_id(name='', token_length=8):
    chars = "ABCDEF0123456789"
    name = filter(lambda x: x in ascii_letters + ".",name)
    if name != '':
        name = name + "_"
    return name + ''.join([random.choice(chars) for i in range(token_length)])

def create_envelope_and_get_signing_url(wizard_data):
    # return is_error(bool), error_message, and redirectURL
    
    # FPPTI or FPPCI
    productType = wizard_data["product_type"]
    enrollmentState = wizard_data["agent_data"]["state"]
    
    # for now, just pull into former variables we've been using
    recipName = wizard_data["agent_data"]["employee"]["first"] + " " + wizard_data["agent_data"]["employee"]["last"]
    employer = wizard_data["agent_data"]["product_name"]
    emailTo = wizard_data["agent_data"]["employee"]["email"]
    
    if emailTo == "" or emailTo == None:
        # fallback email if none was entered - just need a unique address
        emailTo = random_email_id(wizard_data["agent_data"]["employee"]["first"] + "." + wizard_data["agent_data"]["employee"]["last"]) + "@5StarEnroll.com"

    if wizard_data["agent_data"]["is_in_person"]:
        sessionType = "inperson"
    else:
        sessionType = "email"

    if wizard_data["identityType"]:
        idType = wizard_data["identityType"]
    else:
        idType = "email"
    
    if wizard_data["identityToken"]:
        idToken = wizard_data["identityToken"]
    else:
        idToken = emailTo

    # note: DS supplies the last parm of 'event' in the callback
    #callbackURL = "https://5starenroll.com/application_completed" + "?name=" + wizard_data["employee"]["first"] + "&type=" + sessionType
    callbackURL = "http://5starenroll.com/application_completed" + "?name=" + wizard_data["employee"]["first"] + "&type=" + sessionType
    idTokenStr = "Authentication via " + idType + ": " + idToken

    
    SOH_RadiosList = []

    if ((recipName != "") and (recipName != None)):
        recipientName = recipName
    else:
        recipientName = "Applicant"
        
    eeCoverageNullToken = "NONE"
    if wizard_data["employee_coverage"]:
        if wizard_data["employee_coverage"]["face_value"]:
            employeeCoverage = format(wizard_data["employee_coverage"]["face_value"], ",.0f")
            eePremium = format(round((wizard_data["employee_coverage"]["weekly_premium"]*100 * 52) / 12)/100.0, ",.2f")
            SOH_RadiosList += generate_SOHRadios("ee")
        else:
            employeeCoverage = eeCoverageNullToken
            eePremium = " "
    else:
        employeeCoverage = eeCoverageNullToken
        eePremium = " "

    
    if wizard_data["spouse_coverage"]:
        if wizard_data["spouse_coverage"]["face_value"]:
            spouseCoverage = format(wizard_data["spouse_coverage"]["face_value"], ",.0f")
            spPremium = format(round((wizard_data["spouse_coverage"]["weekly_premium"]*100 * 52) / 12)/100.0, ",.2f")
            SOH_RadiosList += generate_SOHRadios("sp")
        else:
            spouseCoverage = " "
            spPremium = " "
    else:
        spouseCoverage = " "
        spPremium = " "
      

    childTabsList = []
    childRadiosList = []

    if wizard_data["children"]:
        childTabsList += generate_ChildTabsEntry(0, wizard_data)
        childRadiosList.append(generate_ChildGenderRadio(0, wizard_data))
        childRadiosList += generate_SOHRadios("c1")
        
    if wizard_data["children"] and len(wizard_data["children"])>1:
        childTabsList += generate_ChildTabsEntry(1, wizard_data)
        childRadiosList.append(generate_ChildGenderRadio(1, wizard_data))
        childRadiosList += generate_SOHRadios("c2")
     
    if wizard_data["children"] and len(wizard_data["children"])>2:
        childTabsList += generate_ChildTabsEntry(2, wizard_data)
        childTabsList.append(generate_ChildGenderAbbrevTab(2, wizard_data))
        childRadiosList += generate_SOHRadios("c3")

    if wizard_data["children"] and len(wizard_data["children"])>3:
        childTabsList += generate_ChildTabsEntry(3, wizard_data)
        childTabsList.append(generate_ChildGenderAbbrevTab(3, wizard_data))
        childRadiosList += generate_SOHRadios("c4")

    eeTabsList = [
        {"tabLabel" : "eeEnrollCityState",
         "value" : wizard_data["enrollCity"] + ", " + wizard_data["enrollState"]},
        {"tabLabel" : "identityToken",
         "value" : idTokenStr},
        {"tabLabel" : "agentCode",
         "value" : user.custom_data["agent_code"]},
        {"tabLabel" : "agentSignName",
         "value" : user.custom_data["signing_name"]},
        {"tabLabel" : "eeFName",
         "value" : wizard_data["employee"]["first"]},
        {"tabLabel" : "eeLName",
         "value" : wizard_data["employee"]["last"]},
        {"tabLabel" : "eeDOB",
         "value" : wizard_data["employee"]["birthdate"]},
        {"tabLabel" : "eeSSN",
         "value" : wizard_data["employee"]["ssn"]},
        {"tabLabel" : "eeCoverage",
         "value" : employeeCoverage},
        {"tabLabel" : "eePremium",
         "value" : eePremium if employeeCoverage !=eeCoverageNullToken else ""} ,
        {"tabLabel" : "Employer",
         "value" : wizard_data["agent_data"]["product_name"]},
        {"tabLabel" : "eeOtherOwnerName",
         "value" : wizard_data["employee_other_owner_name"] if wizard_data["employee_owner"] == "other" else  ""} ,
        {"tabLabel" : "eeOtherOwnerName2",
         "value" : wizard_data["employee_other_owner_name"] if wizard_data["employee_owner"] == "other" else  ""} ,
        {"tabLabel" : "eeOtherOwnerSSN",
         "value" : wizard_data["employee_other_owner_ssn"] if wizard_data["employee_owner"] == "other" else  ""} ,
        {"tabLabel" : "eeStreet1",
         "value" : wizard_data["employee"]["address1"]},
        {"tabLabel" : "eeStreet2",
         "value" : wizard_data["employee"]["address2"]},
        {"tabLabel" : "eeCity",
         "value" : wizard_data["employee"]["city"]},
        {"tabLabel" : "eeState",
         "value" : wizard_data["employee"]["state"]},
        {"tabLabel" : "eeZip",
         "value" : wizard_data["employee"]["zip"]},
        {"tabLabel" : "eePhone",
         "value" : wizard_data["employee"]["phone"]},
        {"tabLabel" : "eeEmail",
         "value" : wizard_data["employee"]["email"]}
    ]
    # add in beneficiaries if appropriate
    if employeeCoverage != eeCoverageNullToken:
        if wizard_data["employee_beneficiary"] == "spouse":
            eeTabsList += [
                {"tabLabel" : "eeBeneFullName",
                 "value" : wizard_data["spouse"]["first"] + " " + wizard_data["spouse"]["last"]},
                {"tabLabel" : "eeBeneRelationship",
                 "value" : "spouse"},
                {"tabLabel" : "eeBeneDOB",
                 "value" : wizard_data["spouse"]["birthdate"]},
                {"tabLabel" : "eeBeneSSN",
                 "value" : wizard_data["spouse"]["ssn"]} 
            ]
        else:
            eeTabsList += [
                {"tabLabel" : "eeBeneFullName",
                 "value" : wizard_data["employee_beneficiary_name"]},
                {"tabLabel" : "eeBeneRelationship",
                 "value" : wizard_data["employee_beneficiary_relationship"]},
                {"tabLabel" : "eeBeneDOB",
                 "value" : wizard_data["employee_beneficiary_dob"]},
                {"tabLabel" : "eeBeneSSN",
                 "value" : wizard_data["employee_beneficiary_ssn"]} 
            ]

    if wizard_data["spouse_owner"] == "other":
        spouseOtherOwnerName = wizard_data["spouse_other_owner_name"]
        spouseOtherOwnerSSN = wizard_data["spouse_other_owner_ssn"]
    elif wizard_data["spouse_owner"] == "employee":
        spouseOtherOwnerName = wizard_data["employee"]["first"] + " " + wizard_data["employee"]["last"]
        spouseOtherOwnerSSN = wizard_data["employee"]["ssn"]
    else:
        spouseOtherOwnerName = ""
        spouseOtherOwnerSSN = ""
        


    spouseTabsList = []
    if spouseCoverage != " ":
        spouseTabsList += [
            {"tabLabel" : "spFName",
             "value" : wizard_data["spouse"]["first"]},
            {"tabLabel" : "spLName",
             "value" : wizard_data["spouse"]["last"]},
            {"tabLabel" : "spDOB",
             "value" : wizard_data["spouse"]["birthdate"]},
            {"tabLabel" : "spSSN",
             "value" : wizard_data["spouse"]["ssn"]},
            {"tabLabel" : "spOtherOwnerName",
             "value" : spouseOtherOwnerName},
            {"tabLabel" : "spOtherOwnerSSN",
             "value" : spouseOtherOwnerSSN},
            {"tabLabel" : "spCoverage",
             "value" : spouseCoverage},
            {"tabLabel" : "spPremium",
             "value" : spPremium}
        ]
        if wizard_data["spouse_beneficiary"] == "employee":
            spouseTabsList += [
                {"tabLabel" : "spBeneFullName",
                 "value" : wizard_data["employee"]["first"] + " " + wizard_data["employee"]["last"]},
                {"tabLabel" : "spBeneRelationship",
                 "value" : "spouse"},
                {"tabLabel" : "spBeneDOB",
                 "value" : wizard_data["employee"]["birthdate"]},
                {"tabLabel" : "spBeneSSN",
                 "value" : wizard_data["employee"]["ssn"]} 
            ]
        else:
            spouseTabsList += [
                {"tabLabel" : "spBeneFullName",
                 "value" : wizard_data["spouse_beneficiary_name"]},
                {"tabLabel" : "spBeneRelationship",
                 "value" : wizard_data["spouse_beneficiary_relationship"]},
                {"tabLabel" : "spBeneDOB",
                 "value" : wizard_data["spouse_beneficiary_dob"]},
                {"tabLabel" : "spBeneSSN",
                 "value" : wizard_data["spouse_beneficiary_ssn"]} 
            ]


    generalRadiosList = []
    # Note: UI screens out any "yes" replacement - so all applications are "no" to replacement
    generalRadiosList.append({"groupName": "productType",
                              "radios": [
                                  {"selected" : "True",
                                   # FPPTI or FPPCI
                                   "value" : productType}
                              ]})
    generalRadiosList.append({"groupName": "existingIns",
                              "radios": [
                                  {"selected" : "True",
                                   "value" : wizard_data["existing_insurance"]}
                              ]})
    generalRadiosList.append({"groupName": "replace",
                              "radios": [
                                  {"selected" : "True",
                                   "value" : "no"}
                              ]})
    
    for (prefix_short, prefix_long) in {("ee", "employee"), ("sp", "spouse")}:
        generalRadiosList.append({"groupName": prefix_short + "Gender",
                              "radios": [
                                  {"selected" : "True" if wizard_data[prefix_long] and wizard_data[prefix_long]["gender"] == "male" else "False",
                                   "value" : "male"},
                                  {"selected" : "True" if wizard_data[prefix_long] and wizard_data[prefix_long]["gender"] == "female" else "False",
                                   "value" : "female"}
                              ]})
        # only include Owner checkbox if coverage was selected
        if ((prefix_short == "ee" and employeeCoverage != eeCoverageNullToken) or 
            (prefix_short == "sp" and spouseCoverage != " ")):
            generalRadiosList.append({"groupName": prefix_short + "Owner",
                                      "radios": [
                                          {"selected" : "True" if wizard_data[prefix_long + "_owner"] == "self" else "False",
                                           "value" : "self"},
                                          {"selected" : "True" if ((wizard_data[prefix_long + "_owner"] == "other") or (wizard_data[prefix_long + "_owner"] == "employee")) else "False",
                                           "value" : "other"}
                                      ]})
         
    agentRadiosList = []
    # identical to whatever EE said
    agentRadiosList.append({"groupName": "existingInsAgent",
                              "radios": [
                                  {"selected" : "True",
                                   "value" : wizard_data["existing_insurance"]}
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

    #construct the body of the request in JSON format  
    requestBody ={
        "accountID" : accountId,
        "status" : "sent",
        "emailSubject": "signature needed: " + productType + " for " +  recipientName + " (" + employer + ")",
        "templateId": get_template_id(productType, enrollmentState),
        "templateRoles": [
            {"email" : emailTo,
             "name" :recipientName,
             "tabs" : {
                 "textTabs": eeTabsList + spouseTabsList + childTabsList,
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
        "returnUrl" :  callbackURL,
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
