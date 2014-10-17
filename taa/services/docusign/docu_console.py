import sys, httplib2, json;
from taa.model.DocuSign_config import (
    dsAgentAuthenticateString, 
    dsAPIAuthenticateString, 
    dsServer,
    baseUrl, 
    apiAccountID
    )
 

def console_url():

    authenticateStr = dsAgentAuthenticateString()
    agentAuthStr =  dsAgentAuthenticateString()

    """
    # STEP 1 - Login
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
 
    #--- display results
    #print ("baseUrl = %s\naccountId = %s" % (baseUrl, accountId));
    """

    #
    # STEP 2 - Get Console View
    #
 
    #construct the body of the request in JSON format.  In this case all we need is the accountId  

    accountId = apiAccountID
    requestBody = "{\"accountId\": \"" + accountId + "\"}";
 
    # append "/views/console" to the baseUrl and use in the request
    url = baseUrl + "/views/console";
    print url
    headers = {'X-DocuSign-Authentication': agentAuthStr, 'Accept': 'application/json', 'Content-Length': str(len(requestBody))};
    http = httplib2.Http();
    response, content = http.request(url, 'POST', headers=headers, body=requestBody);
    # When troubleshooting, send instead to requestb.in (or similar listener) to capture/examing the JSON trace.  Past that trace into SOAPUI to explore the response if needed.
    #response, content = http.request("http://requestb.in/u8jcp3u8", 'POST', headers=headers, body=requestBody);
    status = response.get('status');
    if (status != '201'): 
        print("Error calling webservice, status is: %s" % status); return True, "Error retrieving inbox URL", None;
    data = json.loads(content);
    viewUrl = data.get('url');

    # counting on the prior get request to handle the login, now just go directly to hardwired ManageEnvelopes screen
    # a bit kludgey, but should work
    #viewUrl = "https://" + dsServer + "/MEMBER/ManageEnvelopes.aspx?loaddash=1&action=redirect&report=ForMyAttention"

    #--- display results
    #print ("View URL = %s\n" % viewUrl)

    return viewUrl
