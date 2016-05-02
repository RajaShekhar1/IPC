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
    if should_use_demo_account():
        return {'FPPTI':'857F9448-88B2-4DA1-AFCB-8F5B354E137F',
                'FPPCI':'857F9448-88B2-4DA1-AFCB-8F5B354E137F',
                'FPP-Gov':'857F9448-88B2-4DA1-AFCB-8F5B354E137F',
                'FPPTIY':'857F9448-88B2-4DA1-AFCB-8F5B354E137F',
                'FPPTIB':'857F9448-88B2-4DA1-AFCB-8F5B354E137F',
                'FPPTIW':'857F9448-88B2-4DA1-AFCB-8F5B354E137F',
                'Group CI':'8A95ABEA-707C-4C69-9929-7CCDD8173ED6'}.get(product_type)

    return ProductFormService().get_application_form_template_id(product_type, state)


def get_replacement_template_id(product_type, state):
    if should_use_demo_account():
        return {'MI':'D11AA8C9-F1AA-43ED-A31E-0EC4F85CDF65',
                'IL':'33CF1C42-0205-460E-B1A4-25EE5A736AB2',
                'IN':'8D3FBE45-124C-4E12-9BAC-85839E9FF826',
                }.get(state, 'BE949002-A716-43F1-9F5C-08A561FD1B82')

    return ProductFormService().get_replacement_template_id(product_type, state)


def get_bank_draft_template_id(product_type, state):
    if should_use_demo_account():
        return '44088a96-8bb2-47d1-b589-6ff68148a8af'
    else:
        return 'BA8F7F9C-1CF9-497A-B904-1C9DE2362589'


def should_use_demo_account():
    return 'demo' in DOCUSIGN_API_ENDPOINT.lower()
