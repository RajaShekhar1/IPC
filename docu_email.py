# DocuSign API Walkthrough 08 (PYTHON) - Embedded Signing
import sys, httplib2, json, bleach;
from flask import url_for


#enter your info:
accountEmail = "docrequest@5starima.com"    # was "bdavis@5starima.com";
password = "5st1rd2m4";
integratorKey = "STAR-0baef057-d5b4-46bd-831f-e8e66f271aa7";
templateId = "65D80628-EA67-45C9-B50D-35932CA28814"  # was "A429E3E3-446A-46F1-9DD0-D417CCEBE1C1";
templateRoleName = "Employee"  #same role name that exists on the template in the console

recipientName = "Edgar Employee";
recipEmail = "employee@thumbprintcpm.com";
 
authenticateStr = "<DocuSignCredentials>" \
                    "<Username>" + accountEmail + "</Username>" \
                    "<Password>" + password + "</Password>" \
                    "<IntegratorKey>" + integratorKey + "</IntegratorKey>" \
                    "</DocuSignCredentials>";


def emailing_sample(recipName, emailTo, emailComments):

    if ((recipName != "") and (recipName != None)):
        recipientName = recipName;
    
    if ((emailTo != "") and (emailTo != None)):
        recipEmail = emailTo;
        
    #
    # STEP 1 - Login
    #
    url = 'https://demo.docusign.net/restapi/v2/login_information'   
    headers = {'X-DocuSign-Authentication': authenticateStr, 'Accept': 'application/json'}
    http = httplib2.Http()
    response, content = http.request(url, 'GET', headers=headers)
 
    status = response.get('status');
    if (status != '200'): 
        print("Error calling webservice, status is: %s" % status); sys.exit();
 
    # get the baseUrl and accountId from the response body
    data = json.loads(content);
    loginInfo = data.get('loginAccounts');
    D = loginInfo[0];
    baseUrl = D['baseUrl'];
    accountId = D['accountId'];
 
    #--- display results
    #print ("baseUrl = %s\naccountId = %s" % (baseUrl, accountId));
 
    #
    # STEP 2 - Create an Envelope with a Recipient and Send...
    #
 
    # construct the body of the request in JSON format  
    requestBody = "{\"accountId\": \"" + accountId + "\"," + \
        "\"status\": \"sent\"," + \
        "\"emailSubject\": \"signature needed: FPP for " +  recipientName + " (XYZ Company)" + "\"," + \
        "\"emailBlurb\": \"" + bleach.clean(emailComments, strip=True).replace("\"","'") +"\"," + \
        "\"templateId\": \"" + templateId + "\"," + \
        "\"templateRoles\": [{ " + \
        "\"email\": \"" + recipEmail + "\"," + \
        "\"name\": \"" + recipientName + "\"," + \
        "\"roleName\": \"" +  templateRoleName + "\" }]" + \
        " }" ;
        # "\"url\": \"5starlifeinsurance.com\" }";
 
    # append "/envelopes" to baseURL and use in the request
    url = baseUrl + "/envelopes";
    headers = {'X-DocuSign-Authentication': authenticateStr, 'Accept': 'application/json'}
    http = httplib2.Http()
    response, content = http.request(url, 'POST', headers=headers, body=requestBody);
    #  debug:  print ("response: %s" % response)

    status = response.get('status');
    
    if (status != '201'): 
        print("Error calling webservice, response is: %s" % response); sys.exit();
    
    data = json.loads(content);
    envId = data.get('envelopeId');
 
    #--- display results
    print("Signature request sent!  EnvelopeId is: %s\n" % envId)

    return 1;
