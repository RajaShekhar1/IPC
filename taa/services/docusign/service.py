import json
import requests
from urlparse import urljoin

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
        "accountID" : docusign_transport.docusign_credentials.api_account_id,
        "status" : "sent",
        "emailSubject": email_subject,
        "compositeTemplates": [component.attach_to_envelope() for component in components],
    }

    result = docusign_transport.post("/envelopes", data)

    return DocusignEnvelope(result['uri'])

def get_docusign_credentials():
    'docusign_integrator_key'

    return DocuSignCredentials(
        app.config['DOCUSIGN_INTEGRATOR_KEY'],
        app.config['DOCUSIGN_API_ACCOUNT_ID'],
        app.config['DOCUSIGN_API_USERNAME'],
        app.config['DOCUSIGN_API_PASSWORD'],
        app.config['DOCUSIGN_API_ENDPOINT'],
    )


class DocuSignTransport(object):
    def __init__(self, docusign_credentials):
        self.docusign_credentials = docusign_credentials

    def make_request(self, url, data):
        req = requests.post(
            urljoin(self.docusign_credentials.api_endpoint, url),
            data=json.dumps(data),
            headers=self._make_headers()
        )
        return req.json()

    def _make_headers(self):
        return {
            'X-DocuSign-Authentication': "<DocuSignCredentials>" \
                "<Username>" + self.docusign_credentials.docusign_api_username + "</Username>" \
                "<Password>" + self.docusign_credentials.docusign_api_password + "</Password>" \
                "<IntegratorKey>" + self.docusign_credentials.docusign_integrator_key + "</IntegratorKey>" \
                "</DocuSignCredentials>",
            'Accept': 'application/json',
        }

class DocuSignCredentials(object):
    def __init__(self, integrator_key, api_account_id, api_username, api_password, api_endpoint):
        self.integrator_key = integrator_key
        self.api_account_id = api_account_id
        self.api_username = api_username
        self.api_password = api_password
        self.api_endpoint = api_endpoint

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
        view_url = docusign_transport.docusign_credentials.api_endpoint + self.uri + "/views/recipient"
        result = docusign_transport.post(view_url, data=data)

        return result['uri']

# Envelope Recipient
class DocuSignRecipient(object):
    def __init__(self, name, email, cc_only=False):
        self.name = name
        self.email = email
        self.cc_only = cc_only

    def is_carbon_copy(self):
        return self.cc_only

    def is_agent(self):
        return False

    def is_employee(self):
        return False

class EmployeeDocuSignRecipient(DocuSignRecipient):
    def is_employee(self):
        return True

class AgentDocuSignRecipient(DocuSignRecipient):
    def is_agent(self):
        return True





# Tabs
class DocuSignTab(object):
    pass

class DocuSignRadioTab(DocuSignTab):
    def __init__(self, group_name, value, is_selected=False):
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
            tabs.append(radio_group)

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
            xPosition=self.x,
            yPosition=self.y,
            documentId=self.document_id,
            pageNumber=self.page_number,
        ))


# Envelope Components

class DocuSignEnvelopeComponent(object):
    def __init__(self, recipients):
        self.recipients = recipients

    def get_agent(self):
        return [r for r in recipients if r.is_agent()]

    def get_employee(self):
        return [r for r in recipients if r.is_employee()]

    def attach_to_envelope(self, envelope):
        raise NotImplementedError("Override")

# Server-side template representation
class DocuSignServerTemplate(DocuSignEnvelopeComponent):
    def __init__(self, template_id, recipients):
        DocuSignEnvelopeComponent.__init__(self, recipients)
        self.template_id = template_id

    def attach_to_envelope(self, envelope):
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
                    "recipients": envelope.generate_recipients(),
                }
            ]
        }


# Custom PDF documents
import base64
import StringIO

from reportlab.pdfgen.canvas import Canvas
from reportlab.lib.pagesizes import letter
from reportlab.lib.units import inch


class BasePDFDoc(DocuSignEnvelopeComponent):
    def __init__(self, recipients):
        DocuSignEnvelopeComponent.__init__(self, recipients)

        self.page_width, self.page_height = letter
        self._pdf_data = StringIO.StringIO()
        self._canvas = Canvas(self._pdf_data, pagesize=letter)

    def get_pdf_bytes(self):
        """
        Generates the PDF and encodes the bytes using base64 encoding.
        """
        self._canvas.save()
        return base64.standard_b64encode(self._pdf_data.getvalue())

    def generate(self):
        raise NotImplementedError("Override this method to do custom drawing")

    def get_tabs(self):
        raise NotImplementedError("Override this method to place custom signing or tabs")


class ChildAttachmentForm(BasePDFDoc):
    def __init__(self, recipients):
        BasePDFDoc.__init__(self, recipients)

        self.children = []

    def add_child(self, child_first, child_last, child_ssn, child_dob, child_soh_answers):
        self.children.append((child_first, child_last, child_ssn, child_dob, child_soh_answers))

    def generate(self):
        y = 0
        for child_first, child_last, child_ssn, child_dob, child_soh_answers in self.children:
            self._canvas.drawString(inch, self.page_height - y, "%s %s %s %s"%(child_first, child_last, child_ssn, child_dob))
            y += inch


if __name__ == "__main__":
    # Test drive the code

    agent = AgentDocuSignRecipient(name="Test Agent", email="agent@zachmason.com")
    employee = EmployeeDocuSignRecipient(name="Zach Mason", email="zach@zachmason.com")
    recipients = [
        agent,
        employee,
    ]
    
    child_attachment_form = ChildAttachmentForm(recipients)
    child_attachment_form.add_child("Joe", "Johnson", child_dob="12/01/2010", child_ssn='123-12-1234', child_soh_answers=[])
    child_attachment_form.add_child("Susie", "Johnson", child_dob="12/01/2012", child_ssn='123-12-3234', child_soh_answers=[])
    child_attachment_form.add_child("Christy", "Johnson", child_dob="12/01/2014", child_ssn='223-12-3234', child_soh_answers=[])

    general_template = DocuSignServerTemplate('666F1F5B-77C6-47CC-AC85-1784B8569C3D', recipients)

    transport = DocuSignTransport(get_docusign_credentials())
    envelope_result = create_envelope(email_subject="testing: signature needed",
                                      components=[general_template, child_attachment_form],
                                      docusign_transport=transport,
                                     )

    url = envelope_result.get_signing_url(employee, callback_url='https://5starenroll.com', docusign_transport=transport)

    print(url)