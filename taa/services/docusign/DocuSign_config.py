from flask.ext.stormpath import user

from taa.config_defaults import (
    DOCUSIGN_API_ACCOUNT_ID,
    DOCUSIGN_API_ENDPOINT,
    DOCUSIGN_API_PASSWORD,
    DOCUSIGN_API_USERNAME,
    DOCUSIGN_INTEGRATOR_KEY,
)
from taa.services.products.product_forms import ProductFormService

# Docusign credential info - currently only used by Group CI with the older code.
apiUserName = DOCUSIGN_API_USERNAME
apiPassword = DOCUSIGN_API_PASSWORD
apiAccountID = DOCUSIGN_API_ACCOUNT_ID
#dsServer = "demo.docusign.net"
integratorKey = DOCUSIGN_INTEGRATOR_KEY
# Strip off the trailing slash for compatibility with TAA 1.0 code
baseUrl = DOCUSIGN_API_ENDPOINT[:-1]
#"https://" + dsServer + "/restapi/v2/accounts/" + apiAccountID

# to add an embedded recipient you must set their clientUserId property in addition to the recipient name and email.
# Whatever you set the clientUserId to you MUST use the same value when requesting the signing URL;
# this seems to be only a Docusign protocol matter and not necessrily related *in our use* to security integrity.
# So just treating this like a constant and reusing anywhere we need such an client ID
templateClientID = "123456" 

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
        "</DocuSignCredentials>"

def sessionUserApprovedForDocusign():
    """
    not "authorized" unless there's a (hopefully valid) api key stored in the user record
    """
    return ('ds_apikey' in user.custom_data.keys()) and (user.custom_data['ds_apikey'] != None) and (user.custom_data['ds_apikey'] != "")

def get_template_id(product_type, state):

    """
    Templates are either State specific, the generic template applying to a group of states, or else not-
    available for a given state.  So, look for specific states first, then if generic, otherwise fail.
    """

    return ProductFormService().get_application_form_template_id(product_type, state)


def get_replacement_template_id(product_type, state):
    return ProductFormService().get_replacement_template_id(product_type, state)
    #return "7286ACB9-8B08-43BB-99EB-C7A37B8B8F2A"
    #return "3E0CF882-8678-4476-A6B3-D60AA4111C85"

