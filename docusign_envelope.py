# DocuSign API Walkthrough 08 (PYTHON) - Embedded Signing
import sys, httplib2, json;

from flask import url_for

 
# I really want to read this from a config file...
authEmail = "docrequest@5starima.com"
password = "5st1rd2m4";
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
                    "<Username>" + authEmail + "</Username>" \
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
            "VA" : "TBD"
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


def create_envelope_and_get_signing_url(wizard_data):
    # return is_error(bool), error_message, and redirectURL

    # for now, just pull into former variables this fcn was using
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
        

    if ((recipName != "") and (recipName != None)):
        recipientName = recipName;

    if wizard_data["employee_coverage"]:
        if wizard_data["employee_coverage"]["face_value"]:
            employeeCoverage = format(wizard_data["employee_coverage"]["face_value"], ",.0f")
            eePremium = format(round((wizard_data["employee_coverage"]["weekly_premium"]*100 * 52) / 12)/100.0, ",.2f")
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
        else:
            spouseCoverage = " "
            spousePremium = " "
    else:
        spouseCoverage = " "
        spousePremium = " "
      
   
    childTabsList = []

    if wizard_data["children"]:
        childTabsList += [
            {"tabLabel" : "child1FName",
             "value" : wizard_data["children"][0]["first"]},
            {"tabLabel" : "child1LName",
             "value" : wizard_data["children"][0]["last"]},
            {"tabLabel" : "child1DOB",
             "value" : wizard_data["children"][0]["birthdate"]},
            {"tabLabel" : "child1SSN",
             "value" : wizard_data["children"][0]["ssn"]},
            {"tabLabel" : "child1Coverage",
             "value" : format(wizard_data["child_coverages"][0]["face_value"], ",.0f")},
            {"tabLabel" : "child1Premium",
             "value" : format(wizard_data["child_coverages"][0]["weekly_premium"]*52/12, ",.2f")},
        ]
    
    if wizard_data["children"] and len(wizard_data["children"])>1:
        childTabsList += [
            {"tabLabel" : "child2FName",
             "value" : wizard_data["children"][1]["first"]},
            {"tabLabel" : "child2LName",
             "value" : wizard_data["children"][1]["last"]},
            {"tabLabel" : "child2DOB",
             "value" : wizard_data["children"][1]["birthdate"]},
            {"tabLabel" : "child2SSN",
             "value" : wizard_data["children"][1]["ssn"]},
            {"tabLabel" : "child2Coverage",
             "value" : format(wizard_data["child_coverages"][1]["face_value"], ",.0f")},
            {"tabLabel" : "child2Premium",
             "value" : format(wizard_data["child_coverages"][1]["weekly_premium"]*52/12, ",.2f")},
        ]
     
    if wizard_data["children"] and len(wizard_data["children"])>2:
        childTabsList += [
            {"tabLabel" : "child3FullName",
             "value" : wizard_data["children"][2]["first"] + " " + wizard_data["children"][2]["last"]},
            {"tabLabel" : "child3DOB",
             "value" : wizard_data["children"][2]["birthdate"]},
            {"tabLabel" : "child3SSN",
             "value" : wizard_data["children"][2]["ssn"]},
            {"tabLabel" : "child3Coverage",
             "value" : format(wizard_data["child_coverages"][2]["face_value"], ",.0f")},
            {"tabLabel" : "child3Premium",
             "value" : format(wizard_data["child_coverages"][2]["weekly_premium"]*52/12, ",.2f")},
        ]

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
        {"tabLabel" : "employeeEmail",
         "value" : wizard_data["employee"]["email"] } 
    ]

    spouseTabsList = [
        {"tabLabel" : "spFName",
         "value" : wizard_data["spouse"]["first"]},
        {"tabLabel" : "spLName",
         "value" : wizard_data["spouse"]["last"]},
        {"tabLabel" : "spDOB",
         "value" : wizard_data["spouse"]["birthdate"]},
        {"tabLabel" : "spSSN",
         "value" : wizard_data["spouse"]["ssn"]},
        {"tabLabel" : "spCoverage",
         "value" : spouseCoverage},
        {"tabLabel" : "spPremium",
         "value" : spPremium if spouseCoverage !="" else ""}
    ]
    
                          


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
        "documentFields": [
            {"name": "driver's license",
             "value": "123456"}],
        "templateId": get_template_id("FPPTI", "TX"),
        "templateRoles": [
            {"email" : recipEmail,
             "name" :recipientName,
             "tabs" : {
                 "textTabs": eeTabsList + spouseTabsList + childTabsList
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
    # response, content = http.request("http://requestb.in/1h0hvno1", 'POST', headers=headers, body=requestBodyStr);
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
