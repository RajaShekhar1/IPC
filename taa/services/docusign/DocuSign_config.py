
from taa.services import LookupService
from taa import app
from taa.config_defaults import (
    DOCUSIGN_API_ACCOUNT_ID,
    DOCUSIGN_API_ENDPOINT,
    DOCUSIGN_API_PASSWORD,
    DOCUSIGN_API_USERNAME,
    DOCUSIGN_INTEGRATOR_KEY,
)
from taa.services.products.product_forms import ProductFormService

# Docusign credential info - currently only used by Group CI with the older code.
apiUserName = app.config['DOCUSIGN_API_USERNAME']
apiPassword = app.config['DOCUSIGN_API_PASSWORD']
apiAccountID = app.config['DOCUSIGN_API_ACCOUNT_ID']
# dsServer = "demo.docusign.net"
integratorKey = app.config['DOCUSIGN_INTEGRATOR_KEY']
# Strip off the trailing slash for compatibility with TAA 1.0 code
baseUrl = app.config['DOCUSIGN_API_ENDPOINT'][:-1]


# "https://" + dsServer + "/restapi/v2/accounts/" + apiAccountID

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
    return ('ds_apikey' in user.custom_data.keys()) and (user.custom_data['ds_apikey'] != None) and (
        user.custom_data['ds_apikey'] != "")


def get_template_id(product_type, state, product_id=None):
    """
    Templates are either State specific, the generic template applying to a group of states, or else not-
    available for a given state.  So, look for specific states first, then if generic, otherwise fail.
    """
    product = None
    if product_id is not None and product_type == 'Static Benefit':
        product_service = LookupService('ProductService')
        """:type: taa.services.products.ProductService"""
        product = product_service.get(product_id)
    if should_use_demo_account():
        if product is not None and product_type == 'Static Benefit':
            return product.template_id if product.template_id is not None else 'E71C3F44-6B3E-404C-936B-C722352CB29F'
        return {'FPPTI': '857F9448-88B2-4DA1-AFCB-8F5B354E137F',
                'FPPCI': '857F9448-88B2-4DA1-AFCB-8F5B354E137F',
                'FPP-Gov': '857F9448-88B2-4DA1-AFCB-8F5B354E137F',
                'FPPTIY': '857F9448-88B2-4DA1-AFCB-8F5B354E137F',
                'FPPTIB': '857F9448-88B2-4DA1-AFCB-8F5B354E137F',
                'FPPTIW': '857F9448-88B2-4DA1-AFCB-8F5B354E137F',
                'Group CI': '8A95ABEA-707C-4C69-9929-7CCDD8173ED6',
                'Static Benefit': '857F9448-88B2-4DA1-AFCB-8F5B354E137F'}.get(product_type)

    if product is not None and product_type == 'Static Benefit':
        product_service = LookupService('ProductService')
        """:type: taa.services.products.ProductService"""
        product = product_service.get(product_id)
        return product.template_id if product.template_id is not None else '8DE3A4D3-22B3-4747-8FC4-0B846EE4102E'
    return ProductFormService().get_application_form_template_id(product_type, state)


def get_replacement_template_id(product_type, state):
    if should_use_demo_account():
        return {'MI': 'D11AA8C9-F1AA-43ED-A31E-0EC4F85CDF65',
                'IL': '33CF1C42-0205-460E-B1A4-25EE5A736AB2',
                'IN': '8D3FBE45-124C-4E12-9BAC-85839E9FF826',
                }.get(state, 'BE949002-A716-43F1-9F5C-08A561FD1B82')

    return ProductFormService().get_replacement_template_id(product_type, state)


def get_bank_draft_template_id(product_type, state):
    if should_use_demo_account():
        return '44088a96-8bb2-47d1-b589-6ff68148a8af'
    else:
        return '4EB09801-4B3F-4F5A-9B88-0924EF1CC8F7'


def should_use_demo_account():
    return 'demo' in DOCUSIGN_API_ENDPOINT.lower()
