import json
from urlparse import urljoin
import base64
import StringIO
from io import BytesIO
from collections import defaultdict

import requests
from reportlab.lib.pagesizes import letter
from reportlab.platypus import SimpleDocTemplate
from PyPDF2 import PdfFileReader

from taa.services.docusign.docusign_envelope import EnrollmentDataWrap, old_create_envelope_and_get_signing_url, \
    agent_service, build_callback_url
from taa.services import RequiredFeature, LookupService

from taa import app


class DocuSignService(object):

    def create_fpp_envelope(self, enrollment_data, case):
        employee, recipients = self.create_envelope_recipients(case, enrollment_data)
        components = self.create_fpp_envelope_components(enrollment_data, recipients, should_use_docusign_renderer=True)
        transport = get_docusign_transport()
        envelope_result = create_envelope(
            email_subject="Signature needed: {} for {} ({})".format(
                enrollment_data.get_product_code(),
                enrollment_data.get_employee_name(),
                enrollment_data.get_employer_name()),
            components=components,
            docusign_transport=transport,
        )
        return employee, envelope_result, transport

    def create_envelope(self, email_subject, components):
        docusign_transport = get_docusign_transport()
        data = {
            "accountID": docusign_transport.api_account_id,
            "status": "sent",
            "emailSubject": email_subject,
            "compositeTemplates": [
                component.generate_composite_template() for component in components],
        }

        result = docusign_transport.post("envelopes", data)

        return DocusignEnvelope(result['uri'])

    def create_envelope_recipients(self, case, enrollment_data):
        signing_agent = get_signing_agent(case)
        agent = AgentDocuSignRecipient(name=signing_agent.name(),
                                       email=signing_agent.email)
        employee = EmployeeDocuSignRecipient(name=enrollment_data.get_employee_name(),
                                             email=enrollment_data.get_employee_email())
        recipients = [
            agent,
            employee,
            # TODO Check if BCC's needed here
        ]
        return employee, recipients

    def create_fpp_envelope_components(self, enrollment_data, recipients, should_use_docusign_renderer):
        from taa.services.docusign.templates.fpp import FPPTemplate
        from taa.services.docusign.templates.fpp_replacement import FPPReplacementFormTemplate

        # Build the components (sections) needed for signing
        components = []

        # Main form
        fpp_form = FPPTemplate(recipients, enrollment_data, should_use_docusign_renderer)
        components.append(fpp_form)

        # Additional Children
        if fpp_form.is_child_attachment_form_needed():
            child_attachment_form = ChildAttachmentForm(recipients, enrollment_data)
            for i, child in enumerate(fpp_form.get_attachment_children()):
                child.update(dict(
                    coverage=format(enrollment_data['child_coverages'][i + 2]['face_value'], ',.0f'),
                    premium=format(enrollment_data['child_coverages'][i + 2]['premium'], '.2f')
                ))
                child_attachment_form.add_child(child)
            components.append(child_attachment_form)

        # Replacement Form
        if fpp_form.is_replacement_form_needed():
            replacement_form = FPPReplacementFormTemplate(recipients,
                                                          enrollment_data,
                                                          should_use_docusign_renderer)
            components.append(replacement_form)

        # Additional replacement policies form
        if fpp_form.is_additional_replacment_policy_attachment_needed():
            from taa.services.docusign.documents.additional_replacement_policies import AdditionalReplacementPoliciesForm
            components.append(AdditionalReplacementPoliciesForm(recipients,
                                                                enrollment_data))
        return components

    def get_signing_agent(self, case):
        if agent_service.get_logged_in_agent():
            signing_agent = agent_service.get_logged_in_agent()
        else:
            signing_agent = case.owner_agent
        return signing_agent


def create_envelope(email_subject, components, docusign_transport):
    docusign_service = LookupService('DocuSignService')
    return docusign_service.create_envelope(email_subject, components)


def create_envelope_recipients(case, enrollment_data):
    docusign_service = LookupService('DocuSignService')
    return docusign_service.create_envelope_recipients(case, enrollment_data)


def create_fpp_envelope(enrollment_data, case):
    docusign_service = LookupService('DocuSignService')
    return docusign_service.create_fpp_envelope(enrollment_data, case)


def create_fpp_envelope_components(enrollment_data, recipients, should_use_docusign_renderer):
    docusign_service = LookupService('DocuSignService')
    return docusign_service.create_fpp_envelope_components(enrollment_data, recipients, should_use_docusign_renderer)


def create_envelope_and_get_signing_url(wizard_data, census_record, case):
    enrollment_data = EnrollmentDataWrap(wizard_data, census_record, case)
    # Product code
    # product = product_service.get(wizard_data['product_data']['id'])
    productType = wizard_data['product_type']
    is_fpp = ('fpp' in productType.lower())
    # If FPP Product, use the new docusign code, otherwise use old path
    if is_fpp:
        return create_fpp_envelope_and_fetch_signing_url(enrollment_data, case)
    else:
        return old_create_envelope_and_get_signing_url(enrollment_data)


def create_fpp_envelope_and_fetch_signing_url(enrollment_data, case):
    employee, envelope_result, transport = create_fpp_envelope(enrollment_data, case)
    redirect_url = fetch_signing_url(employee, enrollment_data, envelope_result, transport)

    return False, None, redirect_url


def get_signing_agent(case):
    docusign_service = LookupService('DocuSignService')
    return docusign_service.get_signing_agent(case)


def fetch_signing_url(employee, enrollment_data, envelope_result, transport):
    redirect_url = envelope_result.get_signing_url(
        employee,
        callback_url=build_callback_url(
            enrollment_data, enrollment_data.get_session_type()),
        docusign_transport=transport
    )
    return redirect_url


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
    transport_service = LookupService('DocuSignTransport')
    return transport_service(
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
    def __init__(self, name, email, cc_only=False, role_name=None, exclude_from_envelope=False):
        self.name = name
        self.email = email
        self.cc_only = cc_only
        self.role_name = role_name

        self._exclude_from_envelope = exclude_from_envelope

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

    def should_exclude_from_envelope(self):
        return self._exclude_from_envelope

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

class CarbonCopyRecipient(DocuSignRecipient):
    def is_employee(self): return False
    def is_agent(self): return False
    def is_carbon_copy(self): return True



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
            if recipient.should_exclude_from_envelope():
                continue

            recip_repr = dict(
                name=recipient.name,
                email=recipient.email,
                recipientId=str(num),
                routingOrder=str(num),
                roleName=recipient.get_role_name(),
                templateRequired=recipient.is_required(),
                tabs=self.generate_docusign_formatted_tabs(recipient),
            )
            if recipient.is_employee():
                recip_repr['clientUserId'] = "123456"

            if self.is_recipient_signer(recipient):
                output["signers"].append(recip_repr)
            else:
                output['carbonCopies'].append(recip_repr)

        return dict(**output)

    def generate_docusign_formatted_tabs(self, recipient):
        # Format tabs for docusign
        ds_tabs = {}
        for tab in self.generate_tabs(recipient):
            tab.add_to_tabs(ds_tabs)

        return ds_tabs

    def generate_tabs(self, recipient):
        """Returns list of our own internal tab representation"""
        raise NotImplementedError("Override")

    def is_recipient_signer(self, recipient):
        raise NotImplementedError("Override")

    def make_inline_doc_repr(self, num_pages, pdf_bytes, recipients):
        return dict(
            document=dict(
                name=self.__class__.__name__,
                sequence="1",
                documentId="1",
                pages=str(num_pages),
                fileExtension="pdf",
                documentBase64=base64.standard_b64encode(pdf_bytes),
            ),
            inlineTemplates=[dict(
                sequence="1",
                recipients=recipients,
            )],
        )


# Server-side template base class.
class DocuSignServerTemplate(DocuSignEnvelopeComponent):

    pdf_generator_service = RequiredFeature("ImagedFormGeneratorService")

    def __init__(self, template_id, recipients, use_docusign_renderer=True):
        DocuSignEnvelopeComponent.__init__(self, recipients)
        self.template_id = template_id
        self.use_docusign_renderer = use_docusign_renderer

    def generate_composite_template(self):
        if self.use_docusign_renderer:
            return self.generate_server_pdfs()
        else:
            return self.generate_inline_pdfs()

    def generate_server_pdfs(self):
        return {
            "serverTemplates": [
                {
                    "templateId": self.template_id,
                    "sequence": "1",
                },
            ],
            "inlineTemplates": [
                {
                    "sequence": "2",
                    "recipients": self.generate_recipients(),
                }
            ]
        }

    def generate_inline_pdfs(self):
        tabs = []
        for recipient in self.recipients:
            tabs += self.generate_tabs(recipient)
        pdf_bytes = self.pdf_generator_service.generate_form_pdf(self.template_id, tabs)
        num_pages = self.get_num_pages(pdf_bytes)
        return self.make_inline_doc_repr(
            num_pages=num_pages,
            pdf_bytes=pdf_bytes,
            recipients=self.generate_recipients()
        )

    def get_num_pages(self, pdf_bytes):
        reader = PdfFileReader(BytesIO(pdf_bytes))
        num_pages = reader.getNumPages()
        return num_pages

    def generate_tabs(self, recipient):
        return []

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
        return self.make_inline_doc_repr(
            num_pages=self.get_num_pages(),
            pdf_bytes=self.get_pdf_bytes(),
            recipients=self.generate_recipients()
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
