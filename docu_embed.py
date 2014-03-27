# DocuSign API Walkthrough 08 (PYTHON) - Embedded Signing
import sys, httplib2, json;
 
#enter your info:
accountEmail = "docrequest@5starima.com"    # was "bdavis@5starima.com";
password = "5st1rd2m4";
integratorKey = "STAR-0baef057-d5b4-46bd-831f-e8e66f271aa7";
templateId = "65D80628-EA67-45C9-B50D-35932CA28814"  # was "A429E3E3-446A-46F1-9DD0-D417CCEBE1C1";
templateRoleName = "Employee"  #same role name that exists on the template in the console
templateClientID = "123456";   # to add an embedded recipient you must set their clientUserId property in addition to
				# the recipient name and email.  Whatever you set the clientUserId to you must use the same
				# value when requesting the signing URL
recipientName = "Ernie Employee";
recipEmail = "employee@thumbprintcpm.com";
 
authenticateStr = "<DocuSignCredentials>" \
                    "<Username>" + accountEmail + "</Username>" \
                    "<Password>" + password + "</Password>" \
                    "<IntegratorKey>" + integratorKey + "</IntegratorKey>" \
                    "</DocuSignCredentials>";

def signing_sample(recipName, returnURL):

    if ((recipName != "") and (recipName != None)):
        recipientName = recipName;
    
    #
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
    # print ("baseUrl = %s\naccountId = %s" % (baseUrl, accountId));
        
    #
    # STEP 2 - Create envelope with an embedded recipient
    #
 
    #construct the body of the request in JSON format  
    requestBody = "{\"accountId\": \"" + accountId + "\"," + \
        "\"status\": \"sent\"," + \
        "\"emailSubject\": \"signature needed: FPP for " +  recipientName + " (XYZ Company)" + "\"," + \
        "\"emailBlurb\": \"This comes from 5Star Take an App Demo, other instructions will replace this statement...\"," + \
        "\"templateId\": \"" + templateId + "\"," + \
        "\"templateRoles\": [{" + \
        "\"email\": \"" + recipEmail + "\"," + \
        "\"name\": \"" + recipientName + "\"," + \
        "\"roleName\": \"" +  templateRoleName + "\"," + \
        "\"clientUserId\": \"" + templateClientID + "\" }] }";
 
    # append "/envelopes" to baseURL and use in the request
    url = baseUrl + "/envelopes";
    headers = {'X-DocuSign-Authentication': authenticateStr, 'Accept': 'application/json', 'Content-Length': str(len(requestBody))};
    http = httplib2.Http();
    response, content = http.request(url, 'POST', headers=headers, body=requestBody);
    status = response.get('status');
    if (status != '201'): 
        print("Error calling webservice, status is: %s" % status); sys.exit();
    
    data = json.loads(content);
 
    # store the uri for next request
    uri = data.get('uri');
     
    #
    # STEP 3 - Get the Embedded Send View
    #
 
    # construct the body of the request in JSON format  
    requestBody =   "{\"authenticationMethod\": \"none\"," + \
                    "\"email\":\"" + recipEmail + "\"," + \
                    "\"returnUrl\": \"" + returnURL + "\"," + \
                    "\"clientUserId\": \"" + templateClientID + "\", " + \
                    "\"userName\": \"" + recipientName + "\"}";
    
    # append uri + "/views/recipient" to baseUrl and use in the request
    url = baseUrl + uri + "/views/recipient";
    headers = {'X-DocuSign-Authentication': authenticateStr, 'Accept': 'application/json', 'Content-Length': str(len(requestBody))};
    http = httplib2.Http();
    response, content = http.request(url, 'POST', headers=headers, body=requestBody);
    status = response.get('status');
    
    # print ("response: %s\ncontent: %s" % (response, content))

    if (status != '201'): 
        print("Error calling webservice, status is: %s" % status); sys.exit();

    data = json.loads(content);
    viewUrl = data.get('url');
 

    #--- display results
    print ("View URL = %s\n" % viewUrl)

    return viewUrl
