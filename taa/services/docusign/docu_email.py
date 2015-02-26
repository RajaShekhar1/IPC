# DocuSign API Walkthrough 08 (PYTHON) - Embedded Signing
import sys, httplib2, json, bleach;
from flask import url_for, flash


#enter your info:
accountEmail = "docrequest@5starima.com"    # was  "5staragent@thumbprintcpm.com"
password = "5st1rd2m4";
integratorKey = "STAR-0baef057-d5b4-46bd-831f-e8e66f271aa7";
templateId = "65D80628-EA67-45C9-B50D-35932CA28814"  # was "A429E3E3-446A-46F1-9DD0-D417CCEBE1C1";
templateRoleName = "Employee"  #same role name that exists on the template in the console

recipientName = "Edgar Employee";
recipEmail = "employee@thumbprintcpm.com";

agentName = "Andrew Agent"
agentEmail = "5staragent@thumbprintcpm.com"
 
loginAuthenticateStr = "<DocuSignCredentials>" \
                    "<Username>" + accountEmail + "</Username>" \
                    "<Password>" + password + "</Password>" \
                    "<IntegratorKey>" + integratorKey + "</IntegratorKey>" \
                    "</DocuSignCredentials>";

sendAuthenticateStr = "<DocuSignCredentials>" \
                      "<SendOnBehalfOf>" + agentEmail + "</SendOnBehalfOf>" \
                      "<Username>" + accountEmail + "</Username>" \
                      "<Password>" + password + "</Password>" \
                      "<IntegratorKey>" + integratorKey + "</IntegratorKey>" \
                      "</DocuSignCredentials>";

                    
def emailing_sample(recipName, employer, emailTo, emailComments):

    errorp = False;

    if ((recipName != "") and (recipName != None)):
        recipientName = recipName;
    
    if ((emailTo != "") and (emailTo != None)):
        recipEmail = emailTo;

    label2 = "employeeEmail"
    value2 = recipEmail
    label1 = "Employer"
    value1 = employer or ""
    

        
    #
    # STEP 1 - Login
    #
    url = 'https://demo.docusign.net/restapi/v2/login_information'   
    headers = {'X-DocuSign-Authentication': loginAuthenticateStr, 'Accept': 'application/json'}
    http = httplib2.Http()
    response, content = http.request(url, 'GET', headers=headers)
 
    status = response.get('status');
    if (status != '200'): 
        print("Error calling webservice upon login, status is: ", status)
        flash("There was a problem logging into the document server. Your application was not sent. Please contact the system administrator.")
        errorp = True;
 
    if not errorp:
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
                      "\"emailSubject\": \"signature needed: FPP for " +  recipientName + " (" + employer + ")" + "\"," + \
                      "\"emailBlurb\": \"" + bleach.clean(emailComments, strip=True).replace("\"","'") +"\"," + \
                      "\"templateId\": \"" + templateId + "\"," + \
                      "\"templateRoles\": [{ " + \
                      "\"email\": \"" + recipEmail + "\"," + \
                      "\"name\": \"" + recipientName + "\"," + \
                      "\"tabs\": {\"textTabs\": [" + \
                      "{\"tabLabel\": \"" + label1 + "\"" + \
                      ", \"value\": \"" + value1 + "\"}" + \
                      ", {\"tabLabel\": \"" + label2 + "\"" + \
                      ", \"value\": \"" + value2 + "\"}" + \
                      "]}, " + \
                      "\"roleName\": \"" +  templateRoleName + "\" }]" + \
                      " }" ;
        # "\"url\": \"5starlifeinsurance.com\" }";
 
        # append "/envelopes" to baseURL and use in the request
        url = baseUrl + "/envelopes";
        headers = {'X-DocuSign-Authentication': loginAuthenticateStr, 'Accept': 'application/json'}
        http = httplib2.Http()
        response, content = http.request(url, 'POST', headers=headers, body=requestBody);
        #  debug:  print ("response: %s" % response)

        status = response.get('status');
        
        if (status != '201'): 
            print "Error calling webservice, response is: ", response;
            print "Header: ", headers
            print "Body: ", requestBody
            flash("There was a problem connecting to the document server. Your application was not sent. Please contact the system administrator.") 
            errorp = True;
 
        if not errorp:
            data = json.loads(content);
            envId = data.get('envelopeId');
 
            #--- display results
            print("Signature request sent!  EnvelopeId is: %s\n" % envId)
            print "Request was: ", requestBody

    sys.stdout.flush()
    return not errorp;
