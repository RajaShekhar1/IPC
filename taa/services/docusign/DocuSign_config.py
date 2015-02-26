from flask.ext.stormpath import user
from taa.services.products import (
    FPPTI_generic_states,
    FPPCI_generic_states,
    GroupCI_generic_states,
    FPPGov_generic_states,
)


useDemoAccount = False 

#Docusign credential info
#authEmail = "docrequest@5starima.com"  #don't need if we use the api username
apiUserName = "e8e7df3d-09be-47d6-922b-387956638a6c" if useDemoAccount else "8dd0f65d-ae78-4026-8d32-81f63818bf16"
apiPassword = "wQtqOMu0AjSE6sGUXoqgK5Iq/Zw=" if useDemoAccount else "edJSzv7Rqc2XNFI3GqM/IrZ9SvM="
apiAccountID = "599b0608-269f-4598-a1c9-da3b67281cb7" if useDemoAccount else "8271282c-7a4e-4e00-a2e9-878924c316d5"
dsServer = "demo.docusign.net" if useDemoAccount else "na2.docusign.net"

integratorKey = "STAR-0baef057-d5b4-46bd-831f-e8e66f271aa7";

baseUrl = "https://" + dsServer + "/restapi/v2/accounts/" + apiAccountID

"""
to add an embedded recipient you must set their clientUserId property in addition to the recipient name and email.  Whatever you set the clientUserId to you MUST use the same value when requesting the signing URL; this seems to be only a Docusign protocol matter and not necessrily related *in our use* to security integrity.  So just treating this like a constant and reusing anywhere we need such an client ID
"""
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
        "</DocuSignCredentials>";


def sessionUserApprovedForDocusign():
    """
    not "authorized" unless there's a (hopefully valid) api key stored in the user record
    """
    return ('ds_apikey' in user.custom_data.keys()) and (user.custom_data['ds_apikey'] != None) and (user.custom_data['ds_apikey'] != "")
    

def get_template_id(product_type, state):
    if useDemoAccount:
        return  get_template_id_DEMO(product_type, state)
    else:
        return get_template_id_PRODUCTION(product_type, state)
    
def get_template_id_PRODUCTION(product_type, state):
    """
    Templates are either State specific, the generic template applying to a group of states, or else not-
    available for a given state.  So, look for specific states first, then if generic, otherwise fail.
    """
    # PRODUCTION template IDs
    templates_by_product_and_state = {
        "FPPTI": {
            "CO" : "2BBEB0C7-E0E0-4F9E-AFC7-B04CB4CEC46B",
            "FL" : "5D738949-D4A0-4CD0-AB55-D79D4D9219A3",
            "IL" : "3409B833-80A2-4F88-9252-E384223FD515",
            "OH" : "7F1691BD-300A-4955-906C-6824A7ADA050",
            "MO" : "1C8525BE-E913-422B-8332-36B13E0B187A",
            "PA" : "57D4EF42-FA9D-49B0-8A2C-5F6AE5F16C9E",
            "TX" : "F5711F73-7D4A-44F2-8D31-2E505B8915F8",
            "VA" : "0AE3D3E9-60C0-4198-82C7-41D1201BF2FE",
            "WI" : "AB16F882-1ED1-4CD6-847E-6BFCCB2ADF98",
            "generic": "F5711F73-7D4A-44F2-8D31-2E505B8915F8",
        },
        "FPPCI": {
            "CO" : "2BBEB0C7-E0E0-4F9E-AFC7-B04CB4CEC46B",
            "IL" : "3409B833-80A2-4F88-9252-E384223FD515",
            "FL" : "5D738949-D4A0-4CD0-AB55-D79D4D9219A3",
            "OH" : "7F1691BD-300A-4955-906C-6824A7ADA050",
            "MO" : "1C8525BE-E913-422B-8332-36B13E0B187A",
            "TX" : "F5711F73-7D4A-44F2-8D31-2E505B8915F8",
            "VA" : "0AE3D3E9-60C0-4198-82C7-41D1201BF2FE",
            "WI" : "AB16F882-1ED1-4CD6-847E-6BFCCB2ADF98",
            "generic": "F5711F73-7D4A-44F2-8D31-2E505B8915F8",
        },
        "Group CI": {
            "generic":"B57234AB-5EA5-48D4-984F-D3BF07793B9B",
        },
    }
    
    # FPP-Gov uses the FPP-TI product forms
    templates_by_product_and_state['FPP-Gov'] = templates_by_product_and_state['FPPTI']
    
    templateID = templates_by_product_and_state.get(product_type, {}).get(state)
    if templateID:
        return templateID
    elif product_type == "FPPTI" and state in FPPTI_generic_states:
        return templates_by_product_and_state[product_type]['generic']
    elif product_type == "FPPCI" and state in FPPCI_generic_states:
        return templates_by_product_and_state[product_type]['generic']
    elif product_type == "Group CI" and state in GroupCI_generic_states:
        return templates_by_product_and_state[product_type]['generic']
    elif product_type == "FPP-Gov" and state in FPPGov_generic_states:
        return templates_by_product_and_state[product_type]['generic']
    else:
        return "Failed product lookup"


def get_template_id_DEMO(product_type, state):
    """
    version with Demo account template IDs
    """

    #
    # bypass lookup while building out templates in production
    #  -- just return generic template ID regardless
    # return "65D80628-EA67-45C9-B50D-35932CA28814"
 


    templates_by_product_and_state = {
        "FPPTI": {
            "CO" : "9972B68F-3D5F-4450-820A-D826DEC38117",
            "IL" : "CB70C876-7310-43A3-8B4B-3FB62D712748"            
            },
        "FPPCI": {
            "CO" : "9972B68F-3D5F-4450-820A-D826DEC38117",
            "IL" : "CB70C876-7310-43A3-8B4B-3FB62D712748"            
        }
    }

    templateID = templates_by_product_and_state.get(product_type).get(state)
    if templateID:
        return templateID
    elif product_type == "FPPTI" and state in FPPTI_generic_states:
        return "65D80628-EA67-45C9-B50D-35932CA28814"
    elif product_type == "FPPCI" and state in FPPCI_generic_states:
        return "65D80628-EA67-45C9-B50D-35932CA28814"
    else:
        # print error to console
        print "Failed lookup for product_type", product_type, " and state", state
        return "Failed product lookup"
