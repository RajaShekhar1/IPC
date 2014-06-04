# DocuSign API Walkthrough 08 (PYTHON) - Embedded Signing
import sys, httplib2, json;

from flask import url_for

 
# I really want to read this from a config file...
authEmail = "docrequest@5starima.com"
authUserName = "e35944e2-e4b1-4e3c-8a69-af9d0ed58b54"  #demo login
password = "1Opb8JZ1JVQwLHpzqylTlspVIUg="  # was "5st1rd2m4";
integratorKey = "STAR-0baef057-d5b4-46bd-831f-e8e66f271aa7";

# these should be selected 
templateRoleName = "Employee"  # same role name that exists on the template in the console
templateClientID = "123456";   # to add an embedded recipient you must set their clientUserId property in addition to
				# the recipient name and email.  Whatever you set the clientUserId to you must use the same
				# value when requesting the signing URL

#these should be passed in by the calling function
recipientName = "Ernie Employee";
recipEmail = "employee@thumbprintcpm.com";
 

authenticateStr = "<DocuSignCredentials>" \
                    "<Username>" + authUserName + "</Username>" \
                    "<Password>" + password + "</Password>" \
                    "<IntegratorKey>" + integratorKey + "</IntegratorKey>" \
                    "</DocuSignCredentials>";

def get_template_id(product_type, state):
    templates_by_product_and_state = {
        "FPPTI": {
            "CO" : "TBD",
            "CT" : "TBD",
            "DC" : "TBD",
            "FL" : "TBD",
            "IL" : "TBD",
            "TX" : "65D80628-EA67-45C9-B50D-35932CA28814",
            "VA" : "65D80628-EA67-45C9-B50D-35932CA28814"
            },
        "FPPCI": {
            "CO" : "TBD",
            "CT" : "TBD",
            "DC" : "TBD",
            "FL" : "TBD",
            "IL" : "TBD",
            "TX" : "65D80628-EA67-45C9-B50D-35932CA28814",
            "VA" : "TBD"
        }
    }

    templateID = templates_by_product_and_state.get(product_type).get(state)
    if templateID:
        return templateID
    else:
        return "Failed Template Lookup"


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
        radioList.append(
            {"groupName": prefix + "SOH" + str(i+1),
             "radios": [{"selected" : "True",
                         "value" : "no"}]}
            )
    return radioList


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
    

    
def create_envelope_and_get_signing_url(wizard_data):
    # return is_error(bool), error_message, and redirectURL

    # for now, just pull into former variables we've been using
    recipName = wizard_data["agent_data"]["employee_first"] + " " + wizard_data["agent_data"]["employee_last"]
    employer = wizard_data["agent_data"]["company_name"]
    emailTo = wizard_data["agent_data"]["employee_email"]
    
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

    landingURL = url_for ('ds_landing_page') + "?name=" + wizard_data["employee"]["first"] + "&type=" + sessionType
                # note: DS supplied the last parm of 'event' in the callback
    idTokenStr = "Authentication via " + idType + ": " + idToken
    
    #if wizard_results["employee_coverage"]["face_value"]:
    #
        

    SOH_RadiosList = []

    if ((recipName != "") and (recipName != None)):
        recipientName = recipName;

    if wizard_data["employee_coverage"]:
        if wizard_data["employee_coverage"]["face_value"]:
            employeeCoverage = format(wizard_data["employee_coverage"]["face_value"], ",.0f")
            eePremium = format(round((wizard_data["employee_coverage"]["weekly_premium"]*100 * 52) / 12)/100.0, ",.2f")
            SOH_RadiosList += generate_SOHRadios("ee")
        else:
            employeeCoverage = " "
            eePremium = " "
    else:
        employeeCoverage = " "
        eePremium = " "

    
    if wizard_data["spouse_coverage"]:
        if wizard_data["spouse_coverage"]["face_value"]:
            spouseCoverage = format(wizard_data["spouse_coverage"]["face_value"], ",.0f")
            spPremium = format(round((wizard_data["spouse_coverage"]["weekly_premium"]*100 * 52) / 12)/100.0, ",.2f")
            SOH_RadiosList += generate_SOHRadios("sp")
        else:
            spouseCoverage = " "
            spousePremium = " "
    else:
        spouseCoverage = " "
        spousePremium = " "
      

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
        {"tabLabel" : "identityToken",
         "value" : idTokenStr},
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
         "value" : eePremium if employeeCoverage !="" else ""} ,
        {"tabLabel" : "Employer",
         "value" : wizard_data["agent_data"]["company_name"]},
        {"tabLabel" : "eeOtherOwnerName",
         "value" : wizard_data["employee_other_owner_name"] if wizard_data["employee_owner"] == "other" else  ""} ,
        {"tabLabel" : "eeOtherOwnerName2",
         "value" : wizard_data["employee_other_owner_name"] if wizard_data["employee_owner"] == "other" else  ""} ,
        {"tabLabel" : "eeOtherOwnerSSN",
         "value" : wizard_data["employee_other_owner_ssn"] if wizard_data["employee_owner"] == "other" else  ""} ,
        {"tabLabel" : "eeStreet1",
         "value" : wizard_data["employee_addr1"]},
        {"tabLabel" : "eeStreet2",
         "value" : wizard_data["employee_addr2"]},
        {"tabLabel" : "eeCity",
         "value" : wizard_data["employee_city"]},
        {"tabLabel" : "eeState",
         "value" : wizard_data["employee_state"]},
        {"tabLabel" : "eeZip",
         "value" : wizard_data["employee_zip"]},
        {"tabLabel" : "eePhone",
         "value" : wizard_data["employee_phone"]},
        {"tabLabel" : "eeEmail",
         "value" : wizard_data["employee"]["email"]}
    ]
    # add in beneficiaries if appropriate
    if employeeCoverage != " ":
        eeTabsList += [
            {"tabLabel" : "eeBeneFullName",
             "value" : wizard_data["spouse"]["last"]+", "+wizard_data["spouse"]["first"] if wizard_data["employee_beneficiary"] == "spouse" else  wizard_data["employee_beneficiary_name"]},
            {"tabLabel" : "eeBeneRelationship",
             "value" : "spouse" if wizard_data["employee_beneficiary"] == "spouse" else  wizard_data["employee_beneficiary_relationship"]},
            {"tabLabel" : "eeBeneDOB",
             "value" : wizard_data["spouse"]["birthdate"] if wizard_data["employee_beneficiary"] == "spouse" else  wizard_data["employee_beneficiary_dob"]},
            {"tabLabel" : "eeBeneSSN",
             "value" : wizard_data["spouse"]["ssn"] if wizard_data["employee_beneficiary"] == "spouse" else  wizard_data["employee_beneficiary_ssn"]} 
        ]
    
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
             "value" : wizard_data["spouse_other_owner_name"] if wizard_data["spouse_owner"] == "other" else  ""} ,
            {"tabLabel" : "spCoverage",
             "value" : spouseCoverage},
            {"tabLabel" : "spPremium",
             "value" : spPremium if spouseCoverage !="" else ""},

            {"tabLabel" : "spBeneFullName",
             "value" : wizard_data["employee"]["last"]+", "+wizard_data["employee"]["first"] if wizard_data["spouse_beneficiary"] == "employee" else  wizard_data["spouse_beneficiary_name"]},
            {"tabLabel" : "spBeneRelationship",
             "value" : "spouse" if wizard_data["spouse_beneficiary"] == "employee" else  wizard_data["spouse_beneficiary_relationship"]},
            {"tabLabel" : "spBeneDOB",
             "value" : wizard_data["employee"]["birthdate"] if wizard_data["spouse_beneficiary"] == "employee" else  wizard_data["spouse_beneficiary_dob"]},
            {"tabLabel" : "spBeneSSN",
             "value" : wizard_data["employee"]["ssn"] if wizard_data["spouse_beneficiary"] == "employee" else  wizard_data["spouse_beneficiary_ssn"]} 
        ]


    generalRadiosList = []
    # Note: UI screens out any "yes" replacement - so all applications are "no" to replacement
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
        generalRadiosList.append({"groupName": prefix_short + "Owner",
                              "radios": [
                                  {"selected" : "True" if wizard_data[prefix_long + "_owner"] == "self" else "False",
                                   "value" : "self"},
                                  {"selected" : "True" if wizard_data[prefix_long + "_owner"] == "other" else "False",
                                   "value" : "other"}
                              ]})
         


    #
    # STEP 1 - Login - get base URL - should be able to cache such a URL and bypass this step
    #          (or at least move to a routine to "initialize" on startup or for session)
    #
    url = 'https://demo.docusign.net/restapi/v2/login_information';   
    headers = {'X-DocuSign-Authentication': authenticateStr, 'Accept': 'application/json'};
    http = httplib2.Http();
    response, content = http.request(url, 'GET', headers=headers);
    
    status = response.get('status');

    if (status != '200'): 
        print("Error initially calling webservice, status is: %s" % status); 
        return True, "Error connecting to Docusign server", None;
        
    # get the baseUrl and accountId from the response body
    data = json.loads(content);
    loginInfo = data.get('loginAccounts');
    D = loginInfo[0];
    baseUrl = D['baseUrl'];
    accountId = D['accountId'];
        
    #
    # STEP 2 - Create envelope with an embedded recipient
    #
 
    #construct the body of the request in JSON format  
    requestBody ={
        "accountID" : accountId,
        "status" : "sent",
        "emailSubject": "signature needed: FPP for " +  recipientName + " (" + employer + ")",
        "templateId": get_template_id("FPPTI", "TX"),
        "templateRoles": [
            {"email" : recipEmail,
             "name" :recipientName,
             "tabs" : {
                 "textTabs": eeTabsList + spouseTabsList + childTabsList,
                 "radioGroupTabs": generalRadiosList + SOH_RadiosList + childRadiosList 
             },
             "roleName" :  templateRoleName,
             "clientUserId": templateClientID 
             }]
    };

    requestBodyStr = json.dumps(requestBody);
    
    
    # append "/envelopes" to baseURL and use in the request
    url = baseUrl + "/envelopes";
    headers = {'X-DocuSign-Authentication': authenticateStr, 'Accept': 'application/json', 'Content-Length': str(len(requestBodyStr))};
    http = httplib2.Http();
    response, content = http.request(url, 'POST', headers=headers, body=requestBodyStr);
    #response, content = http.request("http://requestb.in/usjdq1us", 'POST', headers=headers, body=requestBodyStr);
    status = response.get('status');
    if (status != '201'): 
        print("Error calling webservice, status is: %s" % status); return True, "Error generating Docusign envelope", None;
    
    data = json.loads(content);
 
    # store the uri for next request
    uri = data.get('uri');
     
    #
    # STEP 3 - Get the Embedded Send View
    #
 
    # construct the body of the request in JSON format  
    requestBody =   {
        "authenticationMethod" : "none",
        "email" : recipEmail,
        "returnUrl" :  landingURL,
        "clientUserId" : templateClientID,
        "userName" : recipientName
    }
    
    requestBodyStr = json.dumps(requestBody);
    
    # append uri + "/views/recipient" to baseUrl and use in the request
    url = baseUrl + uri + "/views/recipient";
    headers = {'X-DocuSign-Authentication': authenticateStr, 'Accept': 'application/json', 'Content-Length': str(len(requestBodyStr))};
    http = httplib2.Http();
    response, content = http.request(url, 'POST', headers=headers, body=requestBodyStr);
    status = response.get('status');
    
    # print ("response: %s\ncontent: %s" % (response, content))

    if (status != '201'): 
        print("Error calling webservice, status is: %s" % status); return True, "Error retrieving signature URL", None;

    data = json.loads(content);
    viewUrl = data.get('url');
 

    #--- display results
    print ("View URL = %s\n" % viewUrl)

    return False, None, viewUrl
