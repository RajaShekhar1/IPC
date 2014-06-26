# DocuSign API Walkthrough 08 (PYTHON) - Embedded Signing
from flask.ext.stormpath import user
 
#Docusign credential info
agentPassword = "5st1r1g2nt";
#authEmail = "docrequest@5starima.com"  #don't need if we use the api username
apiUserName = "e35944e2-e4b1-4e3c-8a69-af9d0ed58b54"  #demo login
apiPassword = "1Opb8JZ1JVQwLHpzqylTlspVIUg="  #demo password
apiAccountID = "599b0608-269f-4598-a1c9-da3b67281cb7"  #demo account
dsServer = "demo.docusign.net"

integratorKey = "STAR-0baef057-d5b4-46bd-831f-e8e66f271aa7";

baseUrl = "https://" + dsServer + "/restapi/v2/accounts/" + apiAccountID

"""
to add an embedded recipient you must set their clientUserId property in addition to the recipient name and email.  Whatever you set the clientUserId to you MUST use the same value when requesting the signing URL; this seems to be only a Docusign protocol matter and not necessrily related *in our use* to security integrity.  So just treating this like a constant and reusing anywhere we need such an client ID
"""
templateClientID = "123456" 

"""
def dsAgentAuthenticateString():
    #agentEmail = user.email
    #agentEmail = "5staragent@thumbprintcpm.com"
    #agentPassword = "5st1rd2m4"
    agentEmail = "edab6bb9-932d-4ad1-b19f-ce467cb8db2e"
    agentPassword = "d4c5s3gn"

    return "<DocuSignCredentials>" \
        "<Username>" + agentEmail + "</Username>" \
        "<Password>" + agentPassword + "</Password>" \
        "<IntegratorKey>" + integratorKey + "</IntegratorKey>" \
        "</DocuSignCredentials>"

        
"""

def dsAgentAuthenticateString():
    if 'ds_apikey' in user.custom_data.keys():
        agentName = user.custom_data['ds_apikey']
    else:
        agentName = ""  # this should ulitmately fail gracefully with a console "page not found"
    
    return "<DocuSignCredentials>" \
        "<Username>" + apiUserName + "</Username>" \
        "<Password>" + apiPassword + "</Password>" \
        "<IntegratorKey>" + integratorKey + "</IntegratorKey>" \
        "<SendOnBehalfOf>" + agentName + "</SendOnBehalfOf>" \
        "</DocuSignCredentials>"



def dsAPIAuthenticateString():
    return "<DocuSignCredentials>" \
        "<Username>" + apiUserName + "</Username>" \
        "<Password>" + apiPassword + "</Password>" \
        "<IntegratorKey>" + integratorKey + "</IntegratorKey>" \
        "</DocuSignCredentials>";


def sessionUserApprovedForDocusign():
    """
    not "authorized" unless there's a (hopefully valid) api key stored in the user record
    """
    return ('ds_apikey' in user.custom_data.keys()) and (user.custom_data['ds_apikey'] != None) and (user.custom_data['ds_apikey'] != "")
    
