# DocuSign API Walkthrough 08 (PYTHON) - Embedded Signing
import sys, httplib2, json;
 
#enter your info:
agentEmail = "5staragent@thumbprintcpm.com";
password = "5st1rd2m4";
integratorKey = "STAR-0baef057-d5b4-46bd-831f-e8e66f271aa7";
templateId = "65D80628-EA67-45C9-B50D-35932CA28814"  # was "A429E3E3-446A-46F1-9DD0-D417CCEBE1C1";

authenticateStr = "<DocuSignCredentials>" \
                    "<Username>" + agentEmail + "</Username>" \
                    "<Password>" + password + "</Password>" \
                    "<IntegratorKey>" + integratorKey + "</IntegratorKey>" \
                    "</DocuSignCredentials>";


def console_sample():

    # STEP 1 - Login
    #
    url = 'https://demo.docusign.net/restapi/v2/login_information';   
    headers = {'X-DocuSign-Authentication': authenticateStr, 'Accept': 'application/json'};
    http = httplib2.Http();
    response, content = http.request(url, 'GET', headers=headers);
 
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
    print ("baseUrl = %s\naccountId = %s" % (baseUrl, accountId));
 
    #
    # STEP 2 - Get Console View
    #
 
    #construct the body of the request in JSON format.  In this case all we need is the accountId  
    requestBody = "{\"accountId\": \"" + accountId + "\"}";
 
    # append "/views/console" to the baseUrl and use in the request
    url = baseUrl + "/views/console";
    headers = {'X-DocuSign-Authentication': authenticateStr, 'Accept': 'application/json', 'Content-Length': str(len(requestBody))};
    http = httplib2.Http();
    response, content = http.request(url, 'POST', headers=headers, body=requestBody);
    status = response.get('status');
    if (status != '201'): 
        print("Error calling webservice, status is: %s" % status); sys.exit();
    data = json.loads(content);
    viewUrl = data.get('url');
 
    #--- display results
    print ("View URL = %s\n" % viewUrl)

    return viewUrl

