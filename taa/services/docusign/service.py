import json
import requests
from urlparse import urljoin
import base64
import StringIO
from collections import defaultdict

from reportlab.lib.pagesizes import letter
from reportlab.platypus import SimpleDocTemplate

from taa import app

#
#  - Create an envelope for signing
#  - Add either templates or documents to the envelope
#     - Create a server template for signing
#        - provide tabs
#        - provide recipients
#     - Attach a document as a PDF
#        - provide tabs
#        - provide recipients

# create local document for signing
#   - BaseDocument
#       - ChildrenAttachment
#       -

def create_envelope(email_subject, components, docusign_transport):

    data = {
        "accountID" : docusign_transport.api_account_id,
        "status" : "sent",
        "emailSubject": email_subject,
        "compositeTemplates": [
            component.generate_composite_template() for component in components],
    }

    result = docusign_transport.post("envelopes", data)

    return DocusignEnvelope(result['uri'])

class DocusignEnvelope(object):
    def __init__(self, uri):
        self.uri = uri

    def get_signing_url(self, recipient, callback_url, docusign_transport):
        data = dict(
            authenticationMethod="email",
            email=recipient.email,
            returnUrl=callback_url,
            clientUserId="123456",
            userName=recipient.name,
        )
        base_url = docusign_transport.api_endpoint
        if base_url.endswith('/'):
            base_url = base_url[:-1]
        view_url = base_url + self.uri + "/views/recipient"
        result = docusign_transport.post(view_url, data=data)

        return result['url']

def get_docusign_transport():
    return DocuSignTransport(
        app.config['DOCUSIGN_INTEGRATOR_KEY'],
        app.config['DOCUSIGN_API_ACCOUNT_ID'],
        app.config['DOCUSIGN_API_USERNAME'],
        app.config['DOCUSIGN_API_PASSWORD'],
        app.config['DOCUSIGN_API_ENDPOINT'],
    )

class DocuSignTransport(object):
    def __init__(self, integrator_key, api_account_id, api_username, api_password, api_endpoint):

        self.integrator_key = integrator_key
        self.api_account_id = api_account_id
        self.api_username = api_username
        self.api_password = api_password
        self.api_endpoint = api_endpoint

    def post(self, url, data):
        full_url = urljoin(self.api_endpoint, url)

        req = requests.post(
            full_url,
            data=json.dumps(data),
            headers=self._make_headers()
        )

        # Useful when we want to get the direct input to docusign for debugging DocuSign errors with requests module.
        #print("posting to docusign: %s"%full_url)
        #print("data: %s"%json.dumps(data))
        #print('headers: %s'%self._make_headers())

        if req.status_code < 200 or req.status_code >= 300:
            # Print error to Heroku error logs.
            print("""
DOCUSIGN ERROR at URL: %s
posted data: %s
status is: %s
response:
%s""" % (full_url, data, req.status_code, req.text))

            raise Exception("Bad DocuSign Request")

        return req.json()

    def _make_headers(self):
        return {
            'X-DocuSign-Authentication': "<DocuSignCredentials>" \
                "<Username>" + self.api_username + "</Username>" \
                "<Password>" + self.api_password + "</Password>" \
                "<IntegratorKey>" + self.integrator_key + "</IntegratorKey>" \
                "</DocuSignCredentials>",
            'Accept': 'application/json',
        }


# Envelope Recipient
class DocuSignRecipient(object):
    def __init__(self, name, email, cc_only=False, role_name=None):
        self.name = name
        self.email = email
        self.cc_only = cc_only
        self.role_name = role_name

    def is_carbon_copy(self):
        return self.cc_only

    def is_agent(self):
        return False

    def is_employee(self):
        return False

    def get_role_name(self):
        if self.is_agent():
            return "Agent"
        elif self.is_employee():
            return "Employee"
        else:
            return self.role_name if self.role_name else "None"

    def is_required(self):
        """
        This corresponds to the TemplateRequired parameter on the API, not entirely sure if this
        needs to be specified anymore.
        """
        return False

class EmployeeDocuSignRecipient(DocuSignRecipient):
    def is_employee(self):
        return True

    def is_required(self):
        return True

class AgentDocuSignRecipient(DocuSignRecipient):
    def is_employee(self):
        return False

    def is_agent(self):
        return True





# Tabs
class DocuSignTab(object):
    pass

class DocuSignRadioTab(DocuSignTab):
    def __init__(self, group_name, value, is_selected=True):
        self.group_name = group_name
        self.value = value
        self.is_selected = is_selected

    def add_to_tabs(self, tabs):
        if 'radioGroupTabs' not in tabs:
            tabs['radioGroupTabs'] = []

        # Find the radio with this group name if it exists
        radio_group = next((tab for tab in tabs['radioGroupTabs'] if tab['groupName'] == self.group_name), None)
        if not radio_group:
            radio_group = dict(groupName=self.group_name, radios=[])
            tabs['radioGroupTabs'].append(radio_group)

        # Add this radio
        radio_group['radios'].append(dict(
            selected="True" if self.is_selected else "False",
            value=str(self.value),
        ))

class DocuSignTextTab(DocuSignTab):
    def __init__(self, name, value):
        self.name = name
        self.value = value

    def add_to_tabs(self, tabs):
        if 'textTabs' not in tabs:
            tabs['textTabs'] = []

        tabs['textTabs'].append(dict(tabLabel=self.name, value=self.value))

class DocuSignSigTab(DocuSignTab):
    def __init__(self, x, y, document_id, page_number):
        self.x = x
        self.y = y
        self.document_id = document_id
        self.page_number = page_number

    def add_to_tabs(self, tabs):
        if 'signHereTabs' not in tabs:
            tabs['signHereTabs'] = []

        tabs['signHereTabs'].append(dict(
            xPosition=int(self.x),
            yPosition=int(self.y),
            documentId=self.document_id,
            pageNumber=self.page_number,
        ))


# Envelope Components - basically, some sort of document or template
#
# Base class
class DocuSignEnvelopeComponent(object):
    def __init__(self, recipients):
        """
        The order of the recipients dictates the DocuSign routing order for now.
        """
        self.recipients = recipients

    def generate_composite_template(self):
        """
        DocuSign uses 'composite templates' to represent more complex combinations of
        server-side templates, custom tabs, and attached documents (inline templates).
        """
        raise NotImplementedError("Override")

    def generate_recipients(self):

        output = defaultdict(list)

        for num, recipient in enumerate(self.recipients):
            recip_repr = dict(
                name=recipient.name,
                email=recipient.email,
                recipientId=str(num),
                routingOrder=str(num),
                roleName=recipient.get_role_name(),
                templateRequired=recipient.is_required(),
                tabs=self.generate_tabs(recipient),
            )
            if recipient.is_employee():
                recip_repr['clientUserId'] = "123456"

            if self.is_recipient_signer(recipient):
                output["signers"].append(recip_repr)
            else:
                output['carbonCopies'].append(recip_repr)

        return dict(**output)

    def generate_tabs(self, recipient):
        raise NotImplementedError("Override")

    def is_recipient_signer(self, recipient):
        raise NotImplementedError("Override")

# Server-side template base class.
class DocuSignServerTemplate(DocuSignEnvelopeComponent):
    def __init__(self, template_id, recipients):
        DocuSignEnvelopeComponent.__init__(self, recipients)
        self.template_id = template_id

    def generate_composite_template(self):

        return {
            "serverTemplates":[
                {
                    "templateId": self.template_id,
                    "sequence": "1",
                },
            ],
            "inlineTemplates":[
                {
                    "sequence": "2",
                    "recipients": self.generate_recipients(),
                }
            ]
        }

    def generate_tabs(self, recipient):
        return {}

    def is_recipient_signer(self, recipient):
        return recipient.is_employee() or recipient.is_agent()

# Custom PDF documents



class BasePDFDoc(DocuSignEnvelopeComponent):
    def __init__(self, recipients):
        DocuSignEnvelopeComponent.__init__(self, recipients)

        self.page_width, self.page_height = letter
        self._pdf_data = StringIO.StringIO()
        #self._canvas = Canvas(self._pdf_data, pagesize=letter)
        self._doc = SimpleDocTemplate(self._pdf_data, pagesize=letter)

        # Use this to record the last page before generating document.
        self._num_pages = None

    def get_pdf_bytes(self):
        """
        Generates the PDF and encodes the bytes using base64 encoding.
        """

        # Capture number of pages before saving
        self._num_pages = self._doc.page


        return base64.standard_b64encode(self._pdf_data.getvalue())

    def generate(self):
        raise NotImplementedError("Override this method to do custom drawing")

    def generate_composite_template(self):

        # Generate the PDF
        self.generate()

        # Output DocuSign representation
        return dict(
            document=dict(
                name=self.__class__.__name__,
                sequence="1",
                documentId="1",
                pages=str(self.get_num_pages()),
                fileExtension="pdf",
                documentBase64=self.get_pdf_bytes(),
            ),
            inlineTemplates=[dict(
                sequence="1",
                recipients=self.generate_recipients(),
            )],
        )

    def get_num_pages(self):
        if self._num_pages:
            return self._num_pages
        else:
            # Assume we are still drawing and need the current number of pages.
            return self._doc.page



if __name__ == "__main__":
    # Test drive the code

    from documents.extra_children import ChildAttachmentForm

    agent = AgentDocuSignRecipient(name="Test Agent", email="agent@zachmason.com")
    employee = EmployeeDocuSignRecipient(name="Zach Mason", email="zach@zachmason.com")
    test_recipients = [
        agent,
        employee,
    ]

    child_attachment_form = ChildAttachmentForm(test_recipients)
    child_attachment_form.add_child("Joe", "Johnson", child_dob="12/01/2010", child_ssn='123-12-1234', child_soh_answers=[])
    child_attachment_form.add_child("Susie", "Johnson", child_dob="12/01/2012", child_ssn='123-12-3234', child_soh_answers=[])
    child_attachment_form.add_child("Christy", "Johnson", child_dob="12/01/2014", child_ssn='223-12-3234', child_soh_answers=[])

    general_template = DocuSignServerTemplate('666F1F5B-77C6-47CC-AC85-1784B8569C3D', test_recipients)

    transport = get_docusign_transport()
    envelope_result = create_envelope(email_subject="testing: signature needed",
                                      components=[general_template, child_attachment_form],
                                      docusign_transport=transport,
                                     )

    url = envelope_result.get_signing_url(employee, callback_url='https://5starenroll.com', docusign_transport=transport)

    print(url)

