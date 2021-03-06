from datetime import datetime

import StringIO
import base64
import json
from itertools import ifilter

import requests
from PyPDF2 import PdfFileReader
from collections import defaultdict
from decimal import Decimal
from io import BytesIO
from reportlab.lib.pagesizes import letter
from reportlab.platypus import SimpleDocTemplate
from urlparse import urljoin
from dateutil.parser import parse as parse_datetime

from taa.config_defaults import DOCUSIGN_CC_RECIPIENTS

from taa.services.users import UserService

from taa import app, db
from taa.services.enrollments import EnrollmentApplicationCoverage, EnrollmentApplication
from taa.services.agents import AgentService
from taa.services import RequiredFeature, LookupService
from taa.services.docusign.docusign_envelope import EnrollmentDataWrap, build_callback_url, \
    build_callcenter_callback_url


class DocuSignService(object):
    product_service = RequiredFeature('ProductService')

    def get_existing_envelope(self, enrollment_application):

        if enrollment_application.docusign_envelope_id:
            # We already have an envelope created in docusign, just use that one.
            envelope = DocusignEnvelope(enrollment_application.docusign_envelope_id, enrollment_application)

            # We do want to make sure that the enrollment status is up-to-date, though.
            envelope.update_enrollment_status()

            if envelope.enrollment_record.is_voided():
                # If the envelope is voided, we will just create a new envelope to replace it.
                return None

            return envelope

        return None

    def create_multiproduct_envelope(self, product_submissions, case, enrollment_application):
        # Use the first product to get employee and agent data
        first_product_data = EnrollmentDataWrap(product_submissions[0], case, enrollment_record=enrollment_application)
        in_person_signer, recipients = self.create_envelope_recipients(case, first_product_data)

        # Create and combine components of the envelope from each product.
        components = []

        if case.include_cover_sheet and not enrollment_application.did_sign_in_wizard():
            from taa.services.docusign.documents.cover_sheet import CoverSheetAttachment
            components.append(CoverSheetAttachment([in_person_signer], EnrollmentDataWrap(product_submissions[0], case,
                                                                                          enrollment_record=enrollment_application),
                                                   product_submissions))

        for product_submission in product_submissions:
            # Wrap the submission with an object that knows how to pull out key info.
            enrollment_data = EnrollmentDataWrap(product_submission, case, enrollment_record=enrollment_application)

            # Don't use docusign rendering of form if we need to adjust the recipient routing/roles.
            should_use_docusign_renderer = False if enrollment_data.should_use_call_center_workflow() else True

            product_id = product_submission['product_id']
            product = self.product_service.get(product_id)
            if not product.does_generate_form():
                continue

            if product.is_fpp():
                components += self.create_fpp_envelope_components(enrollment_data, recipients,
                                                                  should_use_docusign_renderer)
            elif product.is_static_benefit() and not enrollment_application.did_sign_in_wizard():
                components += self.create_static_benefit_components(enrollment_data, recipients,
                                                                    should_use_docusign_renderer,
                                                                    enrollment_application)
            elif product.is_group_ci():
                components += self.create_group_ci_envelope_components(enrollment_data, recipients,
                                                                       should_use_docusign_renderer)

        product_codes = [
            EnrollmentDataWrap(product_submission, case, enrollment_record=enrollment_application).get_product_code()
            for product_submission in product_submissions
            ]

        envelope_result = self.create_envelope(
            email_subject=u"Enroll {} ({}) | {}".format(
                first_product_data.get_employee_name(),
                first_product_data.get_employer_name(),
                ','.join(product_codes),
            ),
            components=components,
        )
        return in_person_signer, envelope_result

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

        signing_agent = enrollment_data.get_signing_agent()

        agent = AgentDocuSignRecipient(signing_agent, name=signing_agent.name(),
                                       email=signing_agent.email)
        employee = EmployeeDocuSignRecipient(name=enrollment_data.get_employee_name(),
                                             email=enrollment_data.get_employee_email())

        if enrollment_data.should_use_call_center_workflow():
            recipients = self.get_carbon_copy_recipients()
            return agent, recipients
        else:
            recipients = [
                             employee,
                             agent,
                         ] + self.get_carbon_copy_recipients()
            return employee, recipients

    def get_carbon_copy_recipients(self):
        return [
            CarbonCopyRecipient(name, email)
            for name, email in DOCUSIGN_CC_RECIPIENTS
            ]

    def create_fpp_envelope_components(self, enrollment_data, recipients, should_use_docusign_renderer, show_all_documents=False):
        from taa.services.docusign.templates.fpp import FPPTemplate
        from taa.services.docusign.templates.fpp_replacement import FPPReplacementFormTemplate
        from taa.services.docusign.templates.fpp_bank_draft import FPPBankDraftFormTemplate
        from taa.services.docusign.documents.additional_children import ChildAttachmentForm
        from taa.services.docusign.documents.multiple_beneficiaries_attachment import MultipleBeneficiariesAttachment
        from taa.services.docusign.documents.additional_replacement_policies import AdditionalReplacementPoliciesForm
        from taa.services.docusign.documents.cover_sheet import CoverSheetAttachment

        # Build the components (different PDFs) needed for signing
        components = []

        # Main form
        fpp_form = FPPTemplate(recipients, enrollment_data, should_use_docusign_renderer)
        components.append(fpp_form)

        # Additional Children
        if fpp_form.is_child_attachment_form_needed():
            child_attachment_form = ChildAttachmentForm(recipients, enrollment_data)
            for i, child in enumerate(fpp_form.get_attachment_children()):
                # The indexing starts with the 3rd child.
                child_index = i + 2
                child_data = enrollment_data['child_coverages'][child_index]
                child.update(dict(
                    coverage=format(Decimal(unicode(child_data['face_value']), 'utf-8'), ',.0f'),
                    premium=format(Decimal(unicode(child_data['premium']), 'utf-8'), '.2f'),
                    soh_questions=enrollment_data.get_child_soh_questions(child_index),
                ))
                child_attachment_form.add_child(child)
            components.append(child_attachment_form)

        # Percentage/Multiple beneficiaries
        if fpp_form.is_beneficiary_attachment_needed():
            components.append(MultipleBeneficiariesAttachment(recipients, enrollment_data))

        # Replacement Form
        if fpp_form.is_replacement_form_needed():
            replacement_form = FPPReplacementFormTemplate(recipients,
                                                          enrollment_data,
                                                          should_use_docusign_renderer)
            components.append(replacement_form)

        # Additional replacement policies form
        if fpp_form.is_additional_replacement_policy_attachment_needed():
            components.append(AdditionalReplacementPoliciesForm(recipients,
                                                                enrollment_data))

        # Bank draft should be included if
        # - Collection of bank draft info is on for this case
        # - AND we aren't doing paylogix or processing an import.
        if fpp_form.should_include_bank_draft() and (
                    show_all_documents or
                    not self.should_suppress_bank_draft(enrollment_data)):
            components.append(FPPBankDraftFormTemplate(recipients, enrollment_data, should_use_docusign_renderer))

        return components

    def create_group_ci_envelope_components(self, enrollment_data, recipients, should_use_docusign_renderer, show_all_documents=False):
        from taa.services.docusign.templates.group_ci import GroupCITemplate
        from taa.services.docusign.templates.fpp_bank_draft import FPPBankDraftFormTemplate
        from taa.services.docusign.documents.additional_children import ChildAttachmentForm

        # Build the components (different PDFs) needed for signing
        components = []

        # Main form
        form = GroupCITemplate(recipients, enrollment_data, should_use_docusign_renderer)
        components.append(form)

        # Additional Children
        if form.is_child_attachment_form_needed():
            child_attachment_form = ChildAttachmentForm(recipients, enrollment_data, starting_child_num=5)
            for i, child in enumerate(form.get_attachment_children()):
                # The indexing starts with the 3rd child.
                child_index = i + form.num_children_on_form()
                child_data = enrollment_data['child_coverages'][child_index]
                child.update(dict(
                    coverage=format(Decimal(unicode(child_data['face_value']), 'utf-8'), ',.0f'),
                    premium=format(Decimal(unicode(child_data['premium']), 'utf-8'), '.2f'),
                    soh_questions=enrollment_data.get_child_soh_questions(child_index),
                ))
                child_attachment_form.add_child(child)
            components.append(child_attachment_form)

        # The second part of this statement is meant to restrict this form
        # from showing up when importing enrollments until we implement
        # collecting the Bank Draft data.

        if form.should_include_bank_draft() and (
                    show_all_documents or
                    not self.should_suppress_bank_draft(enrollment_data)):
            components.append(FPPBankDraftFormTemplate(recipients, enrollment_data, should_use_docusign_renderer))

        return components

    def should_suppress_bank_draft(self, enrollment_data):
        # Don't include the bank draft form in these situations.
        return enrollment_data.is_import() or enrollment_data.requires_paylogix_export()

    def create_static_benefit_components(self, enrollment_data, recipients, should_use_docusign_renderer,
                                         enrollment_application, show_all_documents=False):
        components = list()

        from taa.services.docusign.templates.static_benefit import StaticBenefitTemplate
        form = StaticBenefitTemplate(recipients, enrollment_data, should_use_docusign_renderer, enrollment_application)
        components.append(form)

        if form.should_include_bank_draft() and (
                    show_all_documents or
                    not self.should_suppress_bank_draft(enrollment_data)):
            from taa.services.docusign.templates.fpp_bank_draft import FPPBankDraftFormTemplate
            components.append(FPPBankDraftFormTemplate(recipients, enrollment_data, should_use_docusign_renderer))

        return components

    def search_envelopes(self, for_user, envelope_status=None):
        enrollment_service = LookupService("EnrollmentApplicationService")

        # Need to get all envelopes that this user is allowed to see.
        agent_service = LookupService("AgentService")
        if agent_service.is_user_agent(for_user):
            # Fetch enrollments that have been created by me.
            agent = agent_service.get_agent_from_user(for_user)

            own_enrollments = enrollment_service.search_enrollments(
                by_agent_id=agent.id,
                by_agent_signing_status=envelope_status,
            )

            # Fetch additional enrollments for partner agents on cases I own.
            owned_case_ids = [case.id for case in agent.owned_cases]

            partner_enrollments = db.session.query(EnrollmentApplication
                                                   ).filter(EnrollmentApplication.is_preview == False
                                                   ).filter(db.or_(EnrollmentApplication.agent_id != agent.id,
                                                                   EnrollmentApplication.agent_id == None,
                                                                   )
                                                            ).filter(EnrollmentApplication.case_id.in_(owned_case_ids)
                                                                     ).options(db.joinedload('case')
                                                                               # ).options(db.joinedload('coverages').joinedload('product')
                                                                               ).options(db.joinedload('census_record'))
            enrollments = list(own_enrollments) + list(partner_enrollments)

        else:
            # Allow home office and admin to see all for now.
            enrollments = enrollment_service.search_enrollments(
                by_agent_ids=None,
                by_agent_signing_status=envelope_status,
            ).options(db.eagerload('coverages', 'product'))

        print("LOADED ALL ENROLLMENTS")
        return [FakeDocusignEnvelope(enrollment.docusign_envelope_id, enrollment)
                for enrollment in enrollments
                # Include only self-enroll envelopes
                if enrollment.method == EnrollmentApplication.METHOD_SELF_EMAIL and
                enrollment.signature_method == EnrollmentApplication.SIGNATURE_METHOD_WIZARD]

    def sign_enrollment(self, for_user, enrollment_record):
        errors = []
        
        # Now, see if we can sign this enrollment based on who is logged in and the status.
        agent_service = LookupService("AgentService")
        if not agent_service.is_user_agent(for_user) and not agent_service.can_manage_all_cases(for_user):
            raise ValueError("No agent record associated with user {} or agent is not allowed to sign enrollment".format(for_user.href))

        if enrollment_record.is_pending_agent():
            enrollment_record.agent_signing_status = EnrollmentApplication.SIGNING_STATUS_COMPLETE
            enrollment_record.agent_signing_datetime = datetime.now()
            enrollment_record.application_status = EnrollmentApplication.APPLICATION_STATUS_ENROLLED
            
            db.session.commit()
            
            # Create submissions for this enrollment
            submission_service = LookupService('EnrollmentSubmissionService')
            submission_service.create_submissions_for_application(enrollment_record)
        
        return errors
            
        
    def get_envelope_signing_url(self, for_user, envelope_id, callback_url):
        "Must be the agent that the envelope was sent to (this user must be the recipient)"
        errors = []
        envelope_url = '/envelopes/%s' % envelope_id
        enrollment_record = self.get_enrollment_record_from_envelope(envelope_id)
        envelope = DocusignEnvelope(envelope_url, enrollment_record)

        # First, we update our signing status
        envelope.update_enrollment_status()

        # Now, see if we can view this envelope based on who is logged in and the status.
        agent_service = LookupService("AgentService")

        # If not completed, we kick the user out if not an admin or an agent.
        # TODO: Enforce the permissions better here. Might be easier to pass in enrollment ID,
        #  since the caller should have that, then we just need to check if current user has
        #  permissions on that enrollment record.
        if not envelope.is_completed() and not agent_service.is_user_agent(
                for_user) and not agent_service.can_manage_all_cases(for_user):
            raise ValueError("No agent record associated with user {}".format(for_user.href))

        # Only envelopes that have been signed by me.
        # agent = agent_service.get_agent_from_user(for_user)

        # If this has been voided, we return an error.
        if enrollment_record.application_status == EnrollmentApplication.APPLICATION_STATUS_VOIDED:
            return None, [dict(message="This envelope has been voided, and can no longer be viewed or signed.",
                               reason='voided_envelope')]

        # Does the employee need to sign?
        if envelope.is_employee_sig_pending():
            url = envelope.get_employee_signing_url(callback_url)
        elif envelope.is_agent_sig_pending():
            url = envelope.get_agent_signing_url(callback_url)
        else:
            url = envelope.get_completed_view_url(callback_url)

        return url, errors

    def get_enrollment_record_from_envelope(self, envelope_id):
        envelope_url = '/envelopes/%s' % envelope_id
        enrollment_service = LookupService("EnrollmentApplicationService")
        enrollments = enrollment_service.search_enrollments(
            # by_agent_id=agent.id,
            by_envelope_url=envelope_url,
        )
        enrollment_data = enrollments.all()
        if not enrollment_data:
            raise ValueError("No enrollment with envelope id {}".format(envelope_id))
        enrollment_record = enrollment_data[0]
        return enrollment_record

    def build_inbox_callback_url(self, enrollment_record):
        # Build a callback URL for the inbox
        is_ssl = app.config.get('IS_SSL', True)
        hostname = app.config.get('HOSTNAME', '5starenroll.com')
        scheme = 'https://' if is_ssl else 'http://'
        callback_url = ('{scheme}{hostname}/inbox?enrollment={enrollment_id}'.format(
            scheme=scheme,
            hostname=hostname,
            enrollment_id=enrollment_record.id,
        ))
        return callback_url

    def build_census_record_callback_url(self, enrollment_record):
        # Build a callback URL for the inbox
        is_ssl = app.config.get('IS_SSL', True)
        hostname = app.config.get('HOSTNAME', '5starenroll.com')
        scheme = 'https://' if is_ssl else 'http://'
        return ('{scheme}{hostname}/enrollment-case/{case_id}/census/{census_id}'.format(
            scheme=scheme,
            hostname=hostname,
            enrollment_id=enrollment_record.id,
            case_id=enrollment_record.case_id,
            census_id=enrollment_record.census_record_id,
        ))


def create_envelope(email_subject, components):
    docusign_service = LookupService('DocuSignService')
    return docusign_service.create_envelope(email_subject, components)


def create_envelope_recipients(case, enrollment_data):
    docusign_service = LookupService('DocuSignService')
    return docusign_service.create_envelope_recipients(case, enrollment_data)


def fetch_signing_url(in_person_signer, enrollment_data, envelope_result):
    if enrollment_data.should_use_call_center_workflow():
        callback_url = build_callcenter_callback_url(enrollment_data.case)
    else:
        callback_url = build_callback_url(enrollment_data, enrollment_data.get_session_type())

    redirect_url = envelope_result.get_signing_url(
        in_person_signer,
        callback_url=callback_url,
        docusign_transport=get_docusign_transport()
    )
    return redirect_url


class DocusignEnvelope(object):
    def __init__(self, uri, enrollment_record=None, fetch_tabs=False):
        self.uri = uri
        self.enrollment_record = enrollment_record
        self.fetch_tabs = fetch_tabs

        self._cached_recipient_status = None
        self._cached_envelope_status = None
        self._cached_envelope_status_changes = None

    def get_envelope_status_changes(self):
        if self._cached_envelope_status_changes:
            return self._cached_envelope_status_changes

        url = '{}/envelopes?envelopeId={}'.format(self.get_account_base_url(), self.get_envelope_id())
        self._cached_envelope_status_changes = get_docusign_transport().get(url)
        return self._cached_envelope_status_changes

    def get_recipient_status(self):
        if self._cached_recipient_status:
            return self._cached_recipient_status

        docusign_transport = get_docusign_transport()
        self._cached_recipient_status = docusign_transport.get(
            "{}/recipients?include_tabs={}".format(self.get_envelope_base_url(), "true" if self.fetch_tabs else "false")
        )
        return self._cached_recipient_status

    def get_envelope_status(self):
        if self._cached_envelope_status:
            return self._cached_envelope_status

        docusign_transport = get_docusign_transport()
        self._cached_envelope_status = docusign_transport.get(self.get_envelope_base_url())
        return self._cached_envelope_status

    def get_signing_url(self, recipient, callback_url, docusign_transport, clientUserId=None):
        if clientUserId == None:
            clientUserId = recipient.get_client_user_id()

        data = dict(
            authenticationMethod="email",
            email=recipient.email,
            returnUrl=callback_url,
            clientUserId=clientUserId,
            userName=recipient.name,
        )
        view_url = self.get_envelope_base_url() + "/views/recipient"
        result = docusign_transport.post(view_url, data=data)

        return result['url']

    def get_envelope_base_url(self):
        envelope_url = self.get_account_base_url() + self.uri
        return envelope_url

    def get_account_base_url(self):
        url = get_docusign_transport().api_endpoint
        if url.endswith('/'):
            url = url[:-1]
        return url

    def to_json(self):

        if self.enrollment_record.agent_name:
            agent_name = self.enrollment_record.agent_name
        else:
            agent_name = ""

        return dict(
            id=self.get_envelope_id(),
            agent_id=self.enrollment_record.agent_id,
            agent=agent_name,
            employee_signing_status=self.enrollment_record.applicant_signing_status,
            employee_signing_datetime=self.enrollment_record.applicant_signing_datetime,
            agent_signing_datetime=self.enrollment_record.agent_signing_datetime,
            agent_signing_status=self.enrollment_record.agent_signing_status,
            enrollment_record_id=self.enrollment_record.id,
            group=self.enrollment_record.case.company_name,
            timestamp=self.enrollment_record.signature_time,
            employee_first=self.enrollment_record.census_record.employee_first,
            employee_last=self.enrollment_record.census_record.employee_last,
            # TODO: Re-enable after adding server-side pagination to inbox.
            # products=self.get_product_names(),
            # coverage=self.get_coverage_summary(),
            case_id=self.enrollment_record.case_id,
            census_record_id=self.enrollment_record.census_record_id,
            application_status=self.enrollment_record.application_status,
        )

    def get_product_names(self):
        return ', '.join(set([coverage.product.name for coverage in self.enrollment_record.coverages]))

    def get_coverage_summary(self):
        ee_cov = None
        sp_cov = None
        ch_cov = None
        for cov in self.enrollment_record.coverages:
            if cov.applicant_type == EnrollmentApplicationCoverage.APPLICANT_TYPE_EMPLOYEE:
                ee_cov = cov.coverage_face_value
            elif cov.applicant_type == EnrollmentApplicationCoverage.APPLICANT_TYPE_SPOUSE:
                sp_cov = cov.coverage_face_value
            elif cov.applicant_type == EnrollmentApplicationCoverage.APPLICANT_TYPE_CHILD:
                ch_cov = cov.coverage_face_value

        def format_coverage_summary(val):
            if not val:
                return "-"
            if not val.isdigit():
                return val
            import locale
            return locale.currency(int(val), grouping=True)

        return '{} / {} / {}'.format(format_coverage_summary(ee_cov), format_coverage_summary(sp_cov),
                                     format_coverage_summary(ch_cov))

    def get_envelope_id(self):
        return self.uri.replace("/envelopes/", "")

    def update_enrollment_status(self):
        self.update_application_status()
        self.update_recipient_statuses()

        db.session.commit()

    def update_application_status(self):
        DS_ENV_STATUS_DELETED = "deleted"
        DS_ENV_STATUS_VOIDED = "voided"
        DS_ENV_STATUS_DECLINED = "declined"
        DS_ENV_STATUS_COMPLETED = "completed"
        # These statuses are all "pending" full completion.
        DS_ENV_STATUS_SIGNED = "signed"
        DS_ENV_STATUS_DELIVERED = "delivered"
        DS_ENV_STATUS_SENT = "sent"
        DS_ENV_STATUS_CREATED = "created"

        # Based on the signing status and envelope status
        env_status = self.get_envelope_status()['status']
        if env_status in [DS_ENV_STATUS_VOIDED, DS_ENV_STATUS_DELETED]:
            self.void_enrollment()
        elif env_status in [DS_ENV_STATUS_DECLINED]:
            self.decline_enrollment()
        elif env_status in [DS_ENV_STATUS_COMPLETED]:
            self.complete_enrollment()
        else:
            # Anything else is a pending status
            self.mark_enrollment_pending()

    def void_enrollment(self):
        self.enrollment_record.application_status = EnrollmentApplication.APPLICATION_STATUS_VOIDED

    def decline_enrollment(self):
        self.enrollment_record.application_status = EnrollmentApplication.APPLICATION_STATUS_DECLINED

    def complete_enrollment(self):
        self.enrollment_record.application_status = EnrollmentApplication.APPLICATION_STATUS_ENROLLED

    def mark_enrollment_pending(self):
        if self.is_employee_sig_pending():
            self.enrollment_record.application_status = EnrollmentApplication.APPLICATION_STATUS_PENDING_EMPLOYEE
        else:
            self.enrollment_record.application_status = EnrollmentApplication.APPLICATION_STATUS_PENDING_AGENT

    def update_recipient_statuses(self):

        # Get employee if he is a signer.
        emp_signer = self.get_employee_signing_status()
        if emp_signer:
            if emp_signer['status'] == 'sent' or emp_signer['status'] == 'delivered':
                # Not signed
                self.enrollment_record.applicant_signing_status = EnrollmentApplication.SIGNING_STATUS_PENDING
                self.enrollment_record.applicant_signing_datetime = None

            elif emp_signer.get('signedDateTime'):
                # Employee Signed
                self.enrollment_record.applicant_signing_status = EnrollmentApplication.SIGNING_STATUS_COMPLETE
                self.enrollment_record.applicant_signing_datetime = self.parse_signing_date(
                    emp_signer.get('signedDateTime'))

            elif emp_signer['status'] == 'declined':
                self.enrollment_record.applicant_signing_status = EnrollmentApplication.SIGNING_STATUS_DECLINED
                self.enrollment_record.applicant_signing_datetime = None

            else:
                # All other statuses are some form of pending
                self.enrollment_record.applicant_signing_status = EnrollmentApplication.SIGNING_STATUS_PENDING
                self.enrollment_record.applicant_signing_datetime = None

        else:
            # No employee signer on envelope.
            self.enrollment_record.applicant_signing_status = EnrollmentApplication.SIGNING_STATUS_NA

        agent_signer = self.get_agent_signing_status()
        if agent_signer:
            if agent_signer['status'] == 'sent' or agent_signer['status'] == 'delivered':
                # Not signed
                self.enrollment_record.agent_signing_status = EnrollmentApplication.SIGNING_STATUS_PENDING

            elif agent_signer.get('signedDateTime'):
                # Employee Signed
                self.enrollment_record.agent_signing_status = EnrollmentApplication.SIGNING_STATUS_COMPLETE
                self.enrollment_record.agent_signing_datetime = self.parse_signing_date(
                    agent_signer.get('signedDateTime'))
            elif agent_signer['status'] == 'declined':
                self.enrollment_record.agent_signing_status = EnrollmentApplication.SIGNING_STATUS_DECLINED
                self.enrollment_record.agent_signing_datetime = None

    def parse_signing_date(self, val):
        utc_datetime = parse_datetime(val)

        # Docusign datetimes are UTC and include TZ info. We are storing localtimes for now on the server, so convert it.

        from dateutil.tz import tzlocal
        from datetime import datetime

        # First add the local timezone offset to the UTC date.
        local_utc_offset = datetime.now(tzlocal()).utcoffset()
        local_datetime_with_tz = utc_datetime + local_utc_offset

        # Strip off the timezone info by parsing a format without TZ info.
        #  (We don't want to store the TZ info in the database)
        return parse_datetime(local_datetime_with_tz.strftime("%FT%T"))

    def is_employee_sig_pending(self):
        # First, is employee even a signer?
        employee_signing_status = self.get_employee_signing_status()
        if not employee_signing_status:
            return False

        signed_date_time = employee_signing_status.get('signedDateTime')
        return not bool(signed_date_time)

    def is_agent_sig_pending(self):
        # First, is employee even a signer?
        agent_sig_status = self.get_agent_signing_status()
        if not agent_sig_status:
            return False

        signed_date_time = agent_sig_status.get('signedDateTime')
        return not bool(signed_date_time)

    def get_employee_signing_status(self):
        return self.find_recipient_by_role(self.get_recipient_status(), "Employee")

    def get_agent_signing_status(self):
        return self.find_recipient_by_role(self.get_recipient_status(), "Agent")

    def find_recipient_by_role(self, recipient_status, role_name):
        signers = recipient_status.get('signers', [])
        return next(ifilter(lambda r: r.get('roleName') == role_name, signers), None)

    def get_employee_signing_url(self, callback_url):
        ds_recip = self.get_employee_signing_status()
        # recipient = AgentDocuSignRecipient(agent, agent.name(), agent.email)
        recipient = EmployeeDocuSignRecipient(
            name=ds_recip['name'],
            email=ds_recip['email'],
            role_name="Employee",
        )
        return self.get_signing_url(recipient, callback_url, get_docusign_transport(), ds_recip.get('clientUserId'))

    def get_agent_signing_url(self, callback_url):
        ds_recip = self.get_agent_signing_status()
        # recipient = AgentDocuSignRecipient(agent, agent.name(), agent.email)
        # return self.get_signing_url(recipient, callback_url, get_docusign_transport())
        # TODO: should put check here to see if current user is actually agent, or up one level?

        data = dict(
            authenticationMethod="email",
            email=ds_recip['email'],
            returnUrl=callback_url,
            userName=ds_recip['name'],
        )
        if ds_recip.get('clientUserId'):
            data['clientUserId'] = ds_recip.get('clientUserId')

        view_url = self.get_envelope_base_url() + "/views/recipient"
        result = get_docusign_transport().post(view_url, data=data)

        return result['url']

    def get_completed_view_url(self, callback_url):
        if self.get_employee_signing_status():
            return self.get_employee_signing_url(callback_url)
        else:
            # Return any valid recipient view
            return self.get_agent_signing_url(callback_url)

    def get_completed_pdf(self):
        # Return the raw PDF bytes. The PDF will only have all the data if it is completed.
        transport = get_docusign_transport()
        content = transport.get_binary(self.get_envelope_base_url() + '/documents/combined')
        # Can pull out the filename from the header, but let's not do that for now.
        return content

    def is_completed(self):
        """Basically is it all done, enrolled.
        Might add declined to this to allow viewing declines."""
        return self.enrollment_record.application_status == EnrollmentApplication.APPLICATION_STATUS_ENROLLED


class FakeDocusignEnvelope(DocusignEnvelope):
    """
    We no longer use docusign; use this wrapper to keep the old code working for now.
    """
    
    def get_envelope_id(self):
        return self.enrollment_record.id
    
    def is_employee_sig_pending(self):
        return self.get_employee_signing_status() == EnrollmentApplication.SIGNING_STATUS_PENDING
    
    def is_agent_sig_pending(self):
        return self.enrollment_record.agent_signing_status == EnrollmentApplication.SIGNING_STATUS_PENDING
    
    def get_employee_signing_status(self):
        return self.enrollment_record.applicant_signing_status

    def update_enrollment_status(self):
        self.update_application_status()
        self.update_recipient_statuses()

        db.session.commit()
        
    def update_recipient_statuses(self):
        # No external system that needs synchronization.
        pass

    def complete_enrollment(self):
        self.enrollment_record.application_status = EnrollmentApplication.APPLICATION_STATUS_ENROLLED

    def mark_enrollment_pending(self):
        if self.is_employee_sig_pending():
            self.enrollment_record.application_status = EnrollmentApplication.APPLICATION_STATUS_PENDING_EMPLOYEE
        else:
            self.enrollment_record.application_status = EnrollmentApplication.APPLICATION_STATUS_PENDING_AGENT

    
    
    def get_agent_signing_status(self):
        return self.enrollment_record.agent_signing_status
    
    def get_completed_pdf(self):
        raise NotImplementedError
    
    def get_completed_view_url(self, callback_url):
        raise NotImplementedError

    def get_agent_signing_url(self, callback_url):
        raise NotImplementedError
    
    def get_employee_signing_url(self, callback_url):
        raise NotImplementedError

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

        self.last_request = None

    def get(self, url):
        full_url = urljoin(self.api_endpoint, url)

        self.last_request = req = requests.get(full_url, headers=self._make_headers())

        if req.status_code < 200 or req.status_code >= 300:
            self._raise_docusign_error(None, full_url, req)

        return req.json()

    def get_binary(self, url):
        full_url = urljoin(self.api_endpoint, url)

        self.last_request = req = requests.get(full_url, headers=self._make_headers())

        if req.status_code < 200 or req.status_code >= 300:
            self._raise_docusign_error(None, full_url, req)

        return req.content

    def post(self, url, data):
        full_url = urljoin(self.api_endpoint, url)

        from taa.helpers import JSONEncoder
        self.last_request = req = requests.post(
            full_url,
            data=json.dumps(data, cls=JSONEncoder),
            headers=self._make_headers()
        )

        # Useful when we want to get the direct input to docusign for debugging DocuSign errors with requests module.
        # print("posting to docusign: %s"%full_url)
        # print("data: %s"%json.dumps(data))
        # print('headers: %s'%self._make_headers())

        if req.status_code < 200 or req.status_code >= 300:
            self._raise_docusign_error(data, full_url, req)

        return req.json()

    def put(self, url, data):
        full_url = urljoin(self.api_endpoint, url)

        self.last_request = req = requests.put(
            full_url,
            data=json.dumps(data),
            headers=self._make_headers()
        )

        if req.status_code < 200 or req.status_code >= 300:
            self._raise_docusign_error(data, full_url, req)

        return req.json()

    def delete(self, url, data):
        full_url = urljoin(self.api_endpoint, url)

        self.last_request = req = requests.delete(
            full_url,
            data=json.dumps(data),
            headers=self._make_headers()
        )

        if req.status_code < 200 or req.status_code >= 300:
            self._raise_docusign_error(data, full_url, req)

        return req.json()

    def _raise_docusign_error(self, data, full_url, req):
        # Print error to Heroku error logs.
        print(u"""
DOCUSIGN ERROR at URL: %s
posted data: %s
status is: %s
response:
%s""" % (full_url, data, req.status_code, req.text))
        raise Exception("Bad DocuSign Request")

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
    def __init__(self, name, email, cc_only=False, role_name=None, exclude_from_envelope=False,
                 use_embedded_signing=True):
        self.name = name
        self.email = email
        self.cc_only = cc_only
        self.role_name = role_name
        self._use_embedded_signing = use_embedded_signing
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

    def should_use_embedded_signing(self):
        return bool(self._use_embedded_signing)

    def get_client_user_id(self):
        raise NotImplementedError()


class EmployeeDocuSignRecipient(DocuSignRecipient):
    def is_employee(self):
        return True

    def is_required(self):
        return True

    def should_use_embedded_signing(self):
        # Always uses embedded signing process.
        return True

    def get_client_user_id(self):
        return "ee-123456"


class AgentDocuSignRecipient(DocuSignRecipient):
    def __init__(self, agent, name, email, cc_only=False, role_name=None, exclude_from_envelope=False,
                 use_embedded_signing=True):
        super(AgentDocuSignRecipient, self).__init__(name, email, cc_only, role_name, exclude_from_envelope,
                                                     use_embedded_signing)
        self.agent = agent

    def is_employee(self):
        return False

    def is_agent(self):
        return True

    def get_client_user_id(self):
        # Hash our agent id
        import hashlib
        return hashlib.sha256("agent-{}".format(self.agent.id)).hexdigest()


class CarbonCopyRecipient(DocuSignRecipient):
    def is_employee(self):
        return False

    def is_agent(self):
        return False

    def is_carbon_copy(self):
        return True

    def should_use_embedded_signing(self):
        return False

    def get_role_name(self):
        return None


# Tabs
class DocuSignTab(object):
    def __init__(self, x=None, y=None, document_id=None, page_number=None, locked=None, required=None,
                 width=None, height=None, tooltip=None):
        self.x = x
        self.y = y
        self.width = width
        self.height = height

        self.document_id = document_id
        self.page_number = page_number
        self.locked = locked
        self.required = required
        self.tooltip = tooltip

    def build_data(self):
        "Base data that applies to all tabs."

        data = {}

        if self.x is not None:
            data['xPosition'] = int(self.x)
        if self.y is not None:
            data['yPosition'] = int(self.y)
        if self.document_id is not None:
            data['documentId'] = self.document_id
        if self.page_number is not None:
            data['pageNumber'] = self.page_number

        if self.width is not None:
            data['width'] = self.width

        if self.height is not None:
            data['height'] = self.height

        if self.locked is not None:
            data['locked'] = self.locked

        if self.required is not None:
            data['required'] = self.required

        if self.tooltip is not None:
            # They call tooltip 'name' for some reason.
            data['name'] = self.tooltip

        return data


class DocuSignRadioTab(DocuSignTab):
    def __init__(self, group_name, value, is_selected=True, x=None, y=None, document_id=None, page_number=None):

        super(DocuSignRadioTab, self).__init__(x, y, document_id, page_number)

        self.group_name = self.name = group_name
        self.value = value
        self.is_selected = is_selected

    def add_to_tabs(self, tabs):
        if 'radioGroupTabs' not in tabs:
            tabs['radioGroupTabs'] = []

        # Find the radio with this group name if it exists
        radio_group = next((tab for tab in tabs['radioGroupTabs'] if tab['groupName'] == self.group_name), None)
        if not radio_group:
            radio_group = dict(groupName=self.group_name, radios=[], documentID=self.document_id)
            tabs['radioGroupTabs'].append(radio_group)

        # Add this radio
        data = self.build_data()
        data.update(dict(
            value=str(self.value),
            selected=bool(self.is_selected),
        ))
        radio_group['radios'].append(data)


class DocuSignTextTab(DocuSignTab):
    def __init__(self, name, value, x=None, y=None, document_id=None, page_number=None, width=None, height=None,
                 is_bold=None, is_italic=None, is_underline=None, font=None, font_size=None, font_color=None,
                 required=None, tooltip=None):

        super(DocuSignTextTab, self).__init__(x, y, document_id, page_number,
                                              width=width, height=height, required=required, tooltip=tooltip)

        self.name = name
        self.value = value

        self.font = font
        self.font_size = font_size
        self.font_color = font_color

        self.is_bold = is_bold
        self.is_underlined = is_underline
        self.is_italic = is_italic

    def add_to_tabs(self, tabs):
        if 'textTabs' not in tabs:
            tabs['textTabs'] = []

        data = self.build_data()
        text_data = dict(
            tabLabel=self.name,
            value=self.value,
        )
        for attr, docu_attr in [
            ('font', 'font'),
            ('font_size', 'fontSize'),
            ('font_color', 'fontColor'),
            ('is_bold', 'bold'),
            ('is_underlined', 'underline'),
            ('is_italic', 'italic'),
        ]:
            if getattr(self, attr) is not None:
                text_data[docu_attr] = getattr(self, attr)

        data.update(text_data)
        tabs['textTabs'].append(data)


class DocuSignPreSignedTextTab(DocuSignTextTab):
    pass


class DocuSignSigTab(DocuSignTab):
    def __init__(self, x, y, document_id, page_number, name=None):
        super(DocuSignSigTab, self).__init__(x, y, document_id, page_number)
        self.name = name if name else "SignHere"

    def add_to_tabs(self, tabs):
        if 'signHereTabs' not in tabs:
            tabs['signHereTabs'] = []

        tabs['signHereTabs'].append(self.build_data())


class DocuSignSigDateTab(DocuSignTab):
    def __init__(self, x, y, document_id, page_number, name=None):
        super(DocuSignSigDateTab, self).__init__(x, y, document_id, page_number)
        self.name = name if name else 'SigDate'

    def add_to_tabs(self, tabs):
        if 'dateSignedTabs' not in tabs:
            tabs['dateSignedTabs'] = []

        tabs['dateSignedTabs'].append(dict(
            xPosition=int(self.x),
            yPosition=int(self.y),
            documentId=self.document_id,
            pageNumber=self.page_number,
        ))


# Envelope Components - basically, some sort of document or template
#
# Base class
class DocuSignEnvelopeComponent(object):
    tab_repository = RequiredFeature('FormTemplateTabRepository')
    pdf_generator_service = RequiredFeature("ImagedFormGeneratorService")

    # Constants used for determining purpose of tab generation.
    PDF_TABS = u'pdf_tabs'
    DOCUSIGN_TABS = u'docusign_tabs'

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
                recipientId=str(num + 1),
                routingOrder=str(num + 1),
                templateRequired=recipient.is_required(),
            )

            tabs = self.generate_docusign_formatted_tabs(recipient)
            if tabs:
                recip_repr['tabs'] = tabs

            if recipient.get_role_name():
                recip_repr['roleName'] = recipient.get_role_name()

            if recipient.should_use_embedded_signing():
                recip_repr['clientUserId'] = recipient.get_client_user_id()

            if self.is_recipient_signer(recipient):
                output["signers"].append(recip_repr)
            elif recipient.is_carbon_copy():
                output['carbonCopies'].append(recip_repr)

        return dict(**output)

    def generate_docusign_formatted_tabs(self, recipient):
        # Format tabs for docusign
        ds_tabs = {}
        generated_tabs = self.generate_tabs(recipient, self.DOCUSIGN_TABS)

        # Check to see if we are already returning docusign-formatted tab
        # if isinstance(generated_tabs, dict):
        #     ds_tabs.update(generated_tabs)
        #     return ds_tabs

        for tab in generated_tabs:
            tab.add_to_tabs(ds_tabs)

        return ds_tabs

    def generate_tabs(self, recipient, purpose):
        """Returns list of our own internal tab representation"""

        # Inject any signature and date data that has been passed from the enrollment.
        tabs = []

        # Convert call-center employee signatures to voice-auth statements.
        if (purpose == self.PDF_TABS and (self.data.should_use_call_center_workflow() or self.data.did_finish_signing_in_wizard() or self.data.get_agent_esignature())
                and hasattr(self, 'template_id') and self.template_id):
            tab_definitions = self.tab_repository.get_tabs_for_template(self.template_id)
            for tab_def in tab_definitions:
                # The PDF Export code currently expects a name of "{}{}".format(tab_type, recip_type)
                #   In order to match up tab defs to values correctly.
                if tab_def.type_ == "SignHere" and tab_def.recipient_role == "Employee":
                    tabs += [DocuSignTextTab("SignHereEmployee", self.data.get_employee_esignature(),
                                             x=tab_def.x, y=tab_def.y,
                                             document_id=1, page_number=tab_def.page,
                                             )]
                elif tab_def.type_ == "DateSigned" and tab_def.recipient_role == "Employee":
                    tabs.append(DocuSignTextTab("DateSignedEmployee", self.data.get_employee_esignature_date(),
                                                x=tab_def.x,
                                                y=tab_def.y,
                                                document_id=1,
                                                page_number=tab_def.page,
                                                ))
                # All call center enrollments use signing ceremony now, so put the agent sig.
                elif tab_def.type_ == "SignHere" and tab_def.recipient_role == "Agent":
                    tabs += [DocuSignTextTab("SignHereAgent", self.data.get_agent_esignature(),
                                             x=tab_def.x, y=tab_def.y,
                                             document_id=1, page_number=tab_def.page,
                                             )]
                elif tab_def.type_ == "DateSigned" and tab_def.recipient_role == "Agent":
                    tabs.append(DocuSignTextTab("DateSignedAgent", self.data.get_agent_esignature_date(),
                                                x=tab_def.x,
                                                y=tab_def.y,
                                                document_id=1,
                                                page_number=tab_def.page,
                                                ))


        # This is for enrollment import - replace signatures with text when rendering PDF.
        if self.data.get('emp_sig_txt') or self.data.did_employee_sign_in_wizard():
            tabs += [DocuSignPreSignedTextTab("SignHereEmployee", self.data.get_employee_esignature())]
            tabs += [DocuSignPreSignedTextTab("InitialHereEmployee", self.data.get_employee_initials())]
        if self.data.get('agent_sig_txt'):
            tabs += [DocuSignPreSignedTextTab("SignHereAgent", self.data.get_agent_esignature())]
            tabs += [DocuSignPreSignedTextTab("InitialHereAgent", self.data.get_agent_initials())]

        # Dates
        if self.data.get('application_date') or self.data.get('emp_sig_date'):
            employee_signed_date = self.data.get('application_date') if self.data.get(
                'application_date') else self.data.get('emp_sig_date')
            tabs += [DocuSignPreSignedTextTab("DateSignedEmployee", employee_signed_date)]
        elif self.data.did_employee_sign_in_wizard():
            tabs += [DocuSignPreSignedTextTab("DateSignedEmployee", self.data.get_employee_esignature_date())]
            
        if self.data.get('application_date') or self.data.get('agent_sig_date'):
            agent_signed_date = self.data.get('application_date') if self.data.get(
                'application_date') else self.data.get('agent_sig_date')
            tabs += [
                DocuSignPreSignedTextTab("DateSignedAgent", agent_signed_date),
            ]

        return tabs

    def is_recipient_signer(self, recipient):
        raise NotImplementedError("Override")

    def make_inline_doc_repr(self, num_pages, pdf_base64, recipients):
        return dict(
            document=dict(
                name=self.__class__.__name__,
                sequence="1",
                documentId="1",
                pages=str(num_pages),
                fileExtension="pdf",
                documentBase64=pdf_base64,
            ),
            inlineTemplates=[dict(
                sequence="1",
                recipients=recipients,
            )],
        )


# Server-side template base class.
class DocuSignServerTemplate(DocuSignEnvelopeComponent):
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
        pdf_bytes = self.generate_pdf_bytes()
        num_pages = self.get_num_pages(pdf_bytes)
        pdf_base64 = base64.standard_b64encode(pdf_bytes)
        return self.make_inline_doc_repr(
            num_pages=num_pages,
            pdf_base64=pdf_base64,
            recipients=self.generate_recipients()
        )

    def generate_pdf_bytes(self):
        tabs = []
        for recipient in self.recipients:
            tabs += self.generate_tabs(recipient, purpose=self.PDF_TABS)
        pdf_bytes = self.pdf_generator_service.generate_form_pdf(
            self.template_id,
            tabs,
        )
        return pdf_bytes

    def get_num_pages(self, pdf_bytes):
        reader = PdfFileReader(BytesIO(pdf_bytes))
        num_pages = reader.getNumPages()
        return num_pages

    def generate_tabs(self, recipient, purpose):
        tabs = super(DocuSignServerTemplate, self).generate_tabs(recipient, purpose)


        # --> No longer including agent tabs in callcenter mode since everything is generated on our side now, including signature
        #
        # if purpose == self.DOCUSIGN_TABS and self.data.should_use_call_center_workflow():
        #     # Find the tab definition
        #     tab_definitions = self.tab_repository.get_tabs_for_template(self.template_id)
        #     for tab_def in tab_definitions:
        #         if tab_def.type_ == "SignHere" and tab_def.recipient_role == "Agent":
        #             tabs.append(DocuSignSigTab(
        #                 x=tab_def.x,
        #                 y=tab_def.y,
        #                 # Not sure what this is?
        #                 document_id=1,
        #                 page_number=tab_def.page,
        #             ))
        #         elif tab_def.type_ == "DateSigned" and tab_def.recipient_role == "Agent":
        #             tabs.append(DocuSignSigDateTab(
        #                 x=tab_def.x,
        #                 y=tab_def.y,
        #                 # Not sure if we will need to generate this?
        #                 document_id=1,
        #                 page_number=tab_def.page,
        #             ))
        #         # Include all radio button tabs that were not locked, both employee and agent.
        #         elif tab_def.custom_type == "Radio" and tab_def.custom_tab_locked == False:
        #             label = tab_def.label.split('.')[0] if len(tab_def.label.split('.')) > 1 else tab_def.label
        #             tabs.append(DocuSignRadioTab(
        #                 label,
        #                 is_selected=False,
        #                 value=tab_def.name,
        #                 x=tab_def.x,
        #                 y=tab_def.y,
        #                 document_id=1,
        #                 page_number=tab_def.page))
        #
        #         elif tab_def.custom_type == "Text" and tab_def.custom_tab_locked == False:
        #             tabs.append(
        #                 DocuSignTextTab(
        #                     # Tab def label is really the name
        #                     name=tab_def.label,
        #                     value="",
        #                     x=tab_def.x,
        #                     y=tab_def.y,
        #                     width=tab_def.width,
        #                     height=tab_def.height,
        #                     document_id=1,
        #                     page_number=tab_def.page,
        #                     required=tab_def.custom_tab_required,
        #                 ))

        return tabs

    def is_recipient_signer(self, recipient):
        return recipient.is_employee() or recipient.is_agent()


# Custom PDF documents
class BasePDFDoc(DocuSignEnvelopeComponent):
    pdf_generator_service = RequiredFeature("ImagedFormGeneratorService")

    def __init__(self, recipients):
        DocuSignEnvelopeComponent.__init__(self, recipients)

        self.page_width, self.page_height = letter
        self._pdf_data = StringIO.StringIO()
        self._doc = SimpleDocTemplate(self._pdf_data, pagesize=letter)

        # Use this to record the last page before generating document.
        self._num_pages = None

    def get_pdf_bytes(self):
        """
        Generates the PDF and encodes the bytes using base64 encoding.
        """

        # Capture number of pages before saving
        self._num_pages = self._doc.page

        return self._pdf_data.getvalue()

    def generate(self):
        raise NotImplementedError("Override this method to do custom drawing")

    def generate_composite_template(self):

        # Generate the PDF
        pdf_bytes = self.generate_pdf_bytes()

        # Output DocuSign representation
        return self.make_inline_doc_repr(
            num_pages=self.get_num_pages(),
            pdf_base64=base64.standard_b64encode(pdf_bytes),
            recipients=self.generate_recipients()
        )

    def generate_pdf_bytes(self):

        # Do any drawing necessary to create the base PDF
        self.generate()
        pdf_bytes = self.get_pdf_bytes()

        # Add any tabs needed and merge them onto this PDF
        tabs = []
        for r in self.recipients:
            tabs += self.generate_tabs(r, self.PDF_TABS)

        pdf_bytes = self.pdf_generator_service.generate_overlay_pdf_from_tabs(
            tabs,
            # Sig tabs will be auto-generated
            [],
            pdf_bytes,
        )
        return pdf_bytes

    def get_num_pages(self):
        if self._num_pages:
            return self._num_pages
        else:
            # Assume we are still drawing and need the current number of pages.
            return self._doc.page


if __name__ == "__main__":
    # Test drive the code

    # from documents.additional_children import ChildAttachmentForm
    #
    # agent = AgentDocuSignRecipient(name="Test Agent", email="agent@zachmason.com")
    # employee = EmployeeDocuSignRecipient(name="Zach Mason", email="zach@zachmason.com")
    # test_recipients = [
    #     agent,
    #     employee,
    # ]
    #
    # child_attachment_form = ChildAttachmentForm(test_recipients)
    # child_attachment_form.add_child("Joe", "Johnson", child_dob="12/01/2010", child_ssn='123-12-1234', child_soh_answers=[])
    # child_attachment_form.add_child("Susie", "Johnson", child_dob="12/01/2012", child_ssn='123-12-3234', child_soh_answers=[])
    # child_attachment_form.add_child("Christy", "Johnson", child_dob="12/01/2014", child_ssn='223-12-3234', child_soh_answers=[])
    #
    # general_template = DocuSignServerTemplate('666F1F5B-77C6-47CC-AC85-1784B8569C3D', test_recipients)

    transport = get_docusign_transport()
    url = 'https://demo.docusign.net/restapi/v2/accounts/5988eb5b-bee1-4825-a078-dcac445a22ce/envelopes'
    transport.post(url, data={'status': 'sent', 'compositeTemplates': [
        {'document': {'name': 'FPPTemplate',
                      'sequence': '1',
                      'fileExtension': 'pdf',
                      'documentBase64': 'JVBERi0xLjMKMSAwIG9iago8PAovS2lkcyBbIDMgMCBSIDQgMCBSIF0KL1R5cGUgL1BhZ2VzCi9Db3VudCAyCj4+CmVuZG9iagoyIDAgb2JqCjw8Ci9Qcm9kdWNlciAoUHlQREYyKQo+PgplbmRvYmoKMyAwIG9iago8PAovQ29udGVudHMgNiAwIFIKL1JvdGF0ZSAwCi9UcmltQm94IFsgMCAwIDYxMiA3OTIgXQovUmVzb3VyY2VzIDw8Ci9YT2JqZWN0IDw8Ci9GbTAgNyAwIFIKL0ZtMSAxMiAwIFIKL0ZtMiAxNCAwIFIKL0ZtMyAxNiAwIFIKL0ZtNCAxOCAwIFIKL0ZtNSAyMCAwIFIKL0ZtNiAyMiAwIFIKL0ZtNyAyNCAwIFIKL0ZtOCAyNiAwIFIKL0ZtOSAyOCAwIFIKPj4KL0V4dEdTdGF0ZSA8PAovR1MyIDMwIDAgUgovR1MxIDExIDAgUgovR1MwIDMxIDAgUgo+PgovRm9udCA8PAovRjIrMCAzMiAwIFIKL0YzIDM2IDAgUgovVDFfNCAzNyAwIFIKL0YxIDQwIDAgUgovVDFfMiA0MSAwIFIKL1QxXzMgNDQgMCBSCi9UMV8wIDQ3IDAgUgovVDFfMSA1MCAwIFIKL1RUMSA1MyAwIFIKL1RUMCA1NiAwIFIKPj4KL1Byb2NTZXQgWyAvSW1hZ2VDIC9UZXh0IC9QREYgL0ltYWdlSSAvSW1hZ2VCIF0KL0NvbG9yU3BhY2UgPDwKL0NTMiA1OSAwIFIKL0NTMSA2MCAwIFIKL0NTMCA5IDAgUgo+Pgo+PgovQ3JvcEJveCBbIDAgMCA2MTIgNzkyIF0KL0dyb3VwIDYxIDAgUgovUGFyZW50IDEgMCBSCi9NZWRpYUJveCBbIDAgMCA2MTIgNzkyIF0KL0Fubm90cyBbIF0KL1R5cGUgL1BhZ2UKL0JsZWVkQm94IFsgMCAwIDYxMiA3OTIgXQovQXJ0Qm94IFsgMTAuMDQzNTAgNi42Nzc3MyA2MTIgNzc3LjgxMjAwIF0KPj4KZW5kb2JqCjQgMCBvYmoKPDwKL0NvbnRlbnRzIDYzIDAgUgovUm90YXRlIDAKL1RyaW1Cb3ggWyAwIDAgNjEyIDc5MiBdCi9SZXNvdXJjZXMgPDwKL0NvbG9yU3BhY2UgPDwKL0NTMiA2NCAwIFIKL0NTMSA2MCAwIFIKL0NTMCA5IDAgUgo+PgovRXh0R1N0YXRlIDw8Ci9HUzEgMzAgMCBSCi9HUzAgMTEgMCBSCj4+Ci9Gb250IDw8Ci9GMiswIDMyIDAgUgovVDFfMiAzNyAwIFIKL1QxXzAgNDcgMCBSCi9UMV8xIDQxIDAgUgovVFQwIDU2IDAgUgovRjMgMzYgMCBSCi9GMSA0MCAwIFIKPj4KL1Byb2NTZXQgWyAvSW1hZ2VDIC9UZXh0IC9QREYgL0ltYWdlSSAvSW1hZ2VCIF0KPj4KL0Nyb3BCb3ggWyAwIDAgNjEyIDc5MiBdCi9Hcm91cCA2NSAwIFIKL1BhcmVudCAxIDAgUgovTWVkaWFCb3ggWyAwIDAgNjEyIDc5MiBdCi9Bbm5vdHMgWyBdCi9UeXBlIC9QYWdlCi9CbGVlZEJveCBbIDAgMCA2MTIgNzkyIF0KL0FydEJveCBbIDE2LjkwNjMwIDYuNjc3NzMgNTk1LjA5NDAwIDc3My4zODgwMCBdCj4+CmVuZG9iago1IDAgb2JqCjw8Ci9UeXBlIC9DYXRhbG9nCi9QYWdlcyAxIDAgUgo+PgplbmRvYmoKNiAwIG9iago8PAovTGVuZ3RoIDQ3NzI3Cj4+CnN0cmVhbQpxCnEKMCA3OTIgNjEyIC03OTIgcmUKVwpuCnEKL0dTMCBncwovRm0wIERvClEKcQovR1MwIGdzCi9GbTEgRG8KUQpRCkJUCi9DUzAgY3MKMC4wMDEwMCAwLjAwMjAwIDAuMDAyMDAgc2NuCi9HUzEgZ3MKL1QxXzAgMSBUZgo5IDAgMCA5IDUxNy4wNjAxMCAyNy4zOTMxMCBUbQooXDA0NCApIFRqCi9UVDAgMSBUZgooXDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzcpIFRqCi9UMV8wIDEgVGYKLTkuMzIyMDAgMy45NDAwMCBUZAooXDA0NCApIFRqCi9UVDAgMSBUZgooXDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzcpIFRqCi9UMV8wIDEgVGYKMCAtMiBURAooXDA0NCApIFRqCi9UVDAgMSBUZgooXDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzcpIFRqCi9UMV8wIDEgVGYKVCoKKFwwNDQgKSBUagovVFQwIDEgVGYKKFwxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3KSBUagpFVAovQ1MxIGNzCjEgc2NuCjExLjExNzAwIDY0NC42NTAwMCA0MDkuODgzMDAgMTMuODAwMDAgcmUKZioKQlQKL0NTMCBjcwowLjAwMTAwIDAuMDAyMDAgMC4wMDIwMCBzY24KL1QxXzAgMSBUZgoxMCAwIDAgMTAgMTQuMDA4MzAgNjY4LjUwMDAwIFRtCihFbXBsKSBUagotMC4wMTcwMCBUYwowLjAxNzAwIFR3CihveSkgVGoKMCBUYwowIFR3CihlclwwNTdHcm91cCBOYW1lXDA3MiApIFRqCi9UVDAgMSBUZgo4IDAgMCA4IDk5LjE3NjMwIDY2OC41MDAwMCBUbQooXDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzcpIFRqCi9UMV8wIDEgVGYKNDAuNTg5MDAgMCBUZAooICAgICkgVGoKMTAgMCAwIDEwIDQyOS41MTkwMCA2NjguNTAwMDAgVG0KKEdyb3VwIE51bWJlclwwNzIgKSBUago4IDAgMCA4IDQ4NS40MDc3MCA2NjguNTAwMDAgVG0KKCApIFRqCi9UVDAgMSBUZgooXDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3KSBUagovQ1MxIGNzCjEgc2NuCi9UMV8xIDEgVGYKMTggMCAwIDE4IDE3Ni41ODY0MCA3NTUuOTk1NjAgVG0KKDUpIFRqCihTKSBUagoxLjQwOTAwIDAgVGQKKHRhKSBUagoocikgVGoKMS40NzgwMCAwIFRkCiggKSBUagooRikgVGoKMC44MzAwMCAwIFRkCihhbWlseSBQKSBUagoocikgVGoKMy45NTEwMCAwIFRkCihvdGVjdGlvbiBQbGFuKSBUagotMC4wMTAwMCBUYwoxNCAwIDAgMTQgMjAxLjIxNDQwIDczOS4xOTU4MCBUbQooSW5kaXZpZHVhbCBUKSBUagowIFRjCihlcm0gTGkpIFRqCihmKSBUago5LjAzMjAwIDAgVGQKKGUgSW5zdSkgVGoKKHIpIFRqCjMuMzQzMDAgMCBUZAooYW5jZSkgVGoKLTEwLjcxMTAwIC0xLjIwMDAwIFRkCih0bykgVGoKKCApIFRqCjEuMjUyMDAgMCBUZAooQSkgVGoKMC43MjgwMCAwIFRkCihnZSAxMDAgQXBwbGljYXRpb24pIFRqCi9DUzAgY3MKMC4wMDEwMCAwLjAwMjAwIDAuMDAyMDAgc2NuCi9UMV8wIDEgVGYKOCAwIDAgOCAxMC4wNDM1MCA1OC44MjEzMCBUbQooVW5kZXJ3cml0dGVuICkgVGoKKGIpIFRqCjUuNDE0MDAgMCBUZAooeSA1U3RhKSBUagoocikgVGoKMi42NDYwMCAwIFRkCiggTGkpIFRqCihmKSBUagoxLjAyNjAwIDAgVGQKKGUgSW5zdSkgVGoKKHIpIFRqCjIuNDE2MDAgMCBUZAooYW5jZSBDb21wYSkgVGoKKG4pIFRqCjQuOTM3MDAgMCBUZAooeSBcMDUwYSBCYXRvbiBSb3VnZVwwNTQgTG91aXNpYW5hIENvbXBhKSBUagoobikgVGoKMTMuMDY1MDAgMCBUZAooeVwwNTEpIFRqCi0yOS41MDMwMCAtMS4yNTAwMCBUZAooTm90IGF2YWlsYWJsZSBpbiBhbGwgc3RhdGVzICkgVGoKNyAwIDAgNyA4My4wNDk4MCA0OC44MjEzMCBUbQo8N2Y+IFRqCjggMCAwIDggODUuODk4NDAgNDguODIxMzAgVG0KKCApIFRqCihBKSBUagowLjY0OTAwIDAgVGQKKGRtaW4gT2ZmaWNlXDA3MiA3NzcgUmVzZWEpIFRqCihyKSBUago4LjY5MDAwIDAgVGQKKGNoIEQpIFRqCihyKSBUagoxLjgzNTAwIDAgVGQKKFwwNTZcMDU0IExpbmNvbG5cMDU0IE5FIDY4NTIxICkgVGoKNyAwIDAgNyAyMzMuNzc1NDAgNDguODIxMzAgVG0KPDdmPiBUago4IDAgMCA4IDIzNi42MjQwMCA0OC44MjEzMCBUbQooIDFcMDU1ODY2XDA1NTg2M1wwNTU5NzUzKSBUagpFVAovQ1MxIGNzCjEgc2NuCjExLjExNzAwIDY4Ni45MDAwMCA1ODYuODYwMDAgMTMuODAwMDAgcmUKZioKQlQKL0NTMCBjcwoxIDEgMSBzY24KL1QxXzIgMSBUZgo5LjUwMDAwIDAgMCA5LjUwMDAwIDE0LjAwODMwIDY5MSBUbQooU2VjdGlvbiAxIFwwNTUgRW1wbCkgVGoKLTAuMDE5MDAgVGMKMC4wMTkwMCBUdwoob3kpIFRqCjAgVGMKMCBUdwooZSkgVGoKKHIpIFRqCjguMTE0MDAgMCBUZAooIEkpIFRqCihuKSBUagowLjkwNDAwIDAgVGQKKGYpIFRqCjAuMzMwMDAgMCBUZAoob3JtYXRpb24pIFRqCjAuMDAxMDAgMC4wMDIwMCAwLjAwMjAwIHNjbgovVDFfMCAxIFRmCjEwIDAgMCAxMCAzODYuMzgzMzAgNzA3IFRtCihJbnN1cmFuY2UgUmVwcmVzZW50YXRpdmUgQXNzaXN0ZWRcMDcyKSBUago4IDAgMCA4IDUxMy43NzgzMCA3MDcgVG0KKCAgKSBUagoxMCAwIDAgMTAgNTMwLjM4MTMwIDcwNyBUbQooU2VsZiBDb21wbGV0ZWRcMDcyICkgVGoKLTUxLjYzNzAwIC04LjAyNzAwIFRkCihFbXBsKSBUagotMC4wMTcwMCBUYwowLjAxNzAwIFR3CihveSkgVGoKMCBUYwowIFR3CihlZVwwNTdPd25lclwwNzIpIFRqCjggMCAwIDggNzcuMzY2NzAgNjI2LjcyNzEwIFRtCiggKSBUagovVFQwIDEgVGYKKFwxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3KSBUagovVDFfMCAxIFRmCjEwIDAgMCAxMCAyMzkuMzA2MjAgNjI2LjcyNzEwIFRtCiggICAgIFNTTlwwNzIpIFRqCjggMCAwIDggMjY0LjI1NTQwIDYyNi43MjcxMCBUbQooICApIFRqCi9UVDAgMSBUZgooXDEzN1wxMzdcMTM3XDEzN1wxMzcgXDA1NSBcMTM3XDEzN1wxMzdcMTM3XDEzNyBcMDU1ICBcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3KSBUagovVDFfMCAxIFRmCjkuNzkxMDAgMCBUZAooICAgICApIFRqCjEwIDAgMCAxMCAzNDkuNjIyNjAgNjI2LjcyNzEwIFRtCihHZW5kZXJcMDcyICkgVGoKOCAwIDAgOCAzNzkuNDExNjAgNjI2LjcyNzEwIFRtCiggICAgICkgVGoKMTAgMCAwIDEwIDM4Ni40NTE3MCA2MjYuNzI3MTAgVG0KKCBNICAgICAgRikgVGoKL1RUMCAxIFRmCjggMCAwIDggNDA5LjA1MTMwIDYyNi43MjcxMCBUbQooICApIFRqCi9UMV8wIDEgVGYKKCAgICApIFRqCjEwIDAgMCAxMCAxNC4wMDgzMCA2MDguNzI3MTAgVG0KKEJpcnRoIERhdGVcMDcyICApIFRqCi9UVDAgMSBUZgo4IDAgMCA4IDU1Ljg1NjkwIDYwOC43MjcxMCBUbQooXDEzN1wxMzdcMTM3XDEzNyBcMDU3XDEzN1wxMzdcMTM3XDEzNyBcMDU3XDEzN1wxMzdcMTM3XDEzNykgVGoKL1QxXzAgMSBUZgoxMCAwIDAgMTAgMTA2LjkzNTEwIDYwOC43MjcxMCBUbQooICAgIEFyZSB5b3UgYWN0aXZlbHkgYXQgd29ya1wwNzdcMDUyICAgICAgIFkgICAgICBOICAgICBEYXRlIG9mIEhpcmVcMDcyICkgVGoKL1RUMSAxIFRmCiggKSBUagovVFQwIDEgVGYKOCAwIDAgOCAyOTkuMDM4NjAgNjA4LjcyNzEwIFRtCihcMTM3XDEzN1wxMzdcMTM3IFwwNTdcMTM3XDEzN1wxMzdcMTM3IFwwNTdcMTM3XDEzN1wxMzdcMTM3ICkgVGoKL1QxXzAgMSBUZgoxMCAwIDAgMTAgMTQuMDA4MzAgNTkwLjcyNzEwIFRtCihNYWlsaW5nIEFkZHJlc3NcMDcyKSBUago4IDAgMCA4IDcyLjYxNzIwIDU5MC43MjcxMCBUbQooICApIFRqCi9UVDAgMSBUZgooXDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzNykgVGoKL1QxXzAgMSBUZgozNS4wMTIwMCAwIFRkCiggKSBUagoxMCAwIDAgMTAgMTQuMDA4MzAgNTcyLjcyNzEwIFRtCihDaXR5XDA3MiApIFRqCi9UVDAgMSBUZgo4IDAgMCA4IDMxLjcwODAwIDU3Mi43MjcxMCBUbQooXDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzcpIFRqCi9UMV8wIDEgVGYKMTAgMCAwIDEwIDE5Mi4yMzkzMCA1NzIuNzI3MTAgVG0KKCAgICAgIFN0YXRlXDA3MiApIFRqCi9UVDAgMSBUZgo4IDAgMCA4IDIyNi4yMDgwMCA1NzIuNzI3MTAgVG0KKFwxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzcpIFRqCjEwIDAgMCAxMCAyNTEuNzQ3MTAgNTcyLjcyNzEwIFRtCiggKSBUagovVDFfMCAxIFRmCiggICAgICBaaXAgQ29kZVwwNzIgICkgVGoKL1RUMCAxIFRmCjggMCAwIDggMzAxLjM1NjAwIDU3Mi43MjcxMCBUbQooXDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzcgICkgVGoKL1QxXzAgMSBUZgoxMCAwIDAgMTAgMTQuMDA4MzAgNTU0LjcyNzEwIFRtCihFbWFpbCBBZGRyZXNzXDA3MiApIFRqCi9UVDAgMSBUZgo4IDAgMCA4IDY4LjIxNzMwIDU1NC43MjcxMCBUbQooXDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzcpIFRqCjEwIDAgMCAxMCAyMzkuNjkzODAgNTU0LjcyNzEwIFRtCiggXDEwMCApIFRqCjggMCAwIDggMjUyLjU3NDcwIDU1NC43MjcxMCBUbQooXDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzcpIFRqCjEgMSAxIHNjbgovVDFfMiAxIFRmCjkuNTAwMDAgMCAwIDkuNTAwMDAgMTQuMDA4MzAgNjQ4LjUxNzEwIFRtCihTZWN0aW9uIDIgXDA1NSApIFRqCihFbXBsKSBUagoobykgVGoKNi44NTEwMCAwIFRkCih5KSBUagowLjQ2MTAwIDAgVGQKKGVlICkgVGoKRVQKcQowIDc5MiA2MTIgLTc5MiByZQpXCm4KMC44MjIwMCAwLjA3MDAwIDAuMjU3MDAgc2NuCi9HUzIgZ3MKcQoxIDAgMCAxIDM3LjM4NzUwIDc3MC4wMjEwMCBjbQowIDAgbQotMS40MTMwMCAwIC0yLjk5NTAwIDAgLTQuODI1MDAgMC4xMDAwMCBjCi05LjY0OTAwIC0xNy42NzUwMCBsCi02LjU3MjAwIC0xNC44NzUwMCAtMy40MTAwMCAtMTMuNDU5MDAgMCAtMTMuNDU5MDAgYwo1Ljk3ODAwIC0xMy40NTkwMCA5Ljg1ODAwIC0xOC4xNzIwMCA5Ljg1ODAwIC0yNC45MzAwMCBjCjkuODU4MDAgLTMwLjQ5NzAwIDcuMjk5MDAgLTM2LjM2MjAwIDIuNzQ1MDAgLTQwLjkzNDAwIGMKLTEuNDEzMDAgLTQ1LjAwOTAwIC02LjI0MDAwIC00Ny4wOTQwMCAtMTEuMjI5MDAgLTQ3LjA5NDAwIGMKLTEzLjIyNjAwIC00Ny4wOTQwMCAtMTQuOTczMDAgLTQ2Ljc5NzAwIC0xNi4yMjEwMCAtNDYuMTAyMDAgYwotMTYuMzg3MDAgLTQ1LjUwNjAwIGwKLTE2LjA1NDAwIC00NC44MDkwMCBsCi0xNS43MjMwMCAtNDQuNjEwMDAgbAotMTQuMjI1MDAgLTQ1LjIwODAwIC0xMi43MjgwMCAtNDUuNTA2MDAgLTExLjE0NzAwIC00NS41MDYwMCBjCi04LjA2OTAwIC00NS41MDYwMCAtNC43NDMwMCAtNDMuOTE1MDAgLTEuODMxMDAgLTQxLjAzMzAwIGMKMi4yNDcwMCAtMzYuODU4MDAgNC42NTkwMCAtMzAuNzk2MDAgNC42NTkwMCAtMjQuOTMwMDAgYwo0LjY1OTAwIC0xOS4zNjUwMCAxLjkxMzAwIC0xNS44ODcwMCAtMS45OTUwMCAtMTUuODg3MDAgYwotNi43MzcwMCAtMTUuODg3MDAgLTkuODE2MDAgLTIxLjE1NDAwIC0xMS4zMTMwMCAtMjEuMTU0MDAgYwotMTEuNjQ2MDAgLTIxLjE1NDAwIC0xMS44OTUwMCAtMjAuODU2MDAgLTExLjg5NTAwIC0yMC4zNTgwMCBjCi0xMS44OTUwMCAtMjAuMDYxMDAgLTExLjgxMjAwIC0xOS43NjQwMCAtMTEuNzI5MDAgLTE5LjI2NjAwIGMKLTUuNDA5MDAgNC4xOTYwMCBsCi01LjA3MzAwIDUuNTk0MDAgLTQuNTc1MDAgNS42OTMwMCAtMy40MTAwMCA1Ljc5NDAwIGMKOS4xMTQwMCA1Ljk5NDAwIGwKMTAuNTI1MDAgNi4wOTMwMCAxMS43NzEwMCA2LjY5MjAwIDEyLjc2OTAwIDcuNzkxMDAgYwoxMy4xODQwMCA3LjU5MjAwIGwKMTIuMjcwMDAgMC40OTkwMCA4LjIwNzAwIDAgMCAwIGMKZgpRCi9DUzIgY3MKMSBzY24KcQoxIDAgMCAxIDM3Ljc3NTYwIDc1MC43NTE0MCBjbQowIDAgbQoxLjA5MDAwIDAgbAoxLjA2NTAwIC0wLjc4MjAwIDEuMDUzMDAgLTEuNTU2MDAgMS4wNTMwMCAtMi4zMjUwMCBjCjEuMDkwMDAgLTMuNDIzMDAgbAoxLjk0MjAwIC0zLjk4OTAwIDIuNzkwMDAgLTQuMzk5MDAgMy42MzcwMCAtNC42NTEwMCBjCjQuNDgzMDAgLTQuOTAzMDAgNS4zODgwMCAtNS4wMjgwMCA2LjM1MjAwIC01LjAyODAwIGMKOC4zMzIwMCAtNS4wMjgwMCA5LjkzOTAwIC00LjQ5ODAwIDExLjE3MzAwIC0zLjQzODAwIGMKMTIuNDA2MDAgLTIuMzc4MDAgMTMuMDI0MDAgLTEuMDU0MDAgMTMuMDI0MDAgMC41MzQwMCBjCjEzLjAyNDAwIDEuNzExMDAgMTIuNjgxMDAgMi41NzMwMCAxMS45OTQwMCAzLjEyMDAwIGMKMTEuMjk0MDAgMy42NzYwMCAxMC4wNzAwMCA0LjExNzAwIDguMzIxMDAgNC40MzkwMCBjCjYuMzA5MDAgNC44MjkwMCA0LjkwMTAwIDUuMjQ4MDAgNC4wOTYwMCA1LjY5NDAwIGMKMy4yODkwMCA2LjE0MTAwIDIuNjY4MDAgNi43MzgwMCAyLjIzMTAwIDcuNDg3MDAgYwoxLjc5NDAwIDguMjM1MDAgMS41NzUwMCA5LjEzMjAwIDEuNTc1MDAgMTAuMTc1MDAgYwoxLjU3NTAwIDEyLjQ1MzAwIDIuNTcyMDAgMTQuMzY1MDAgNC41NjYwMCAxNS45MTMwMCBjCjYuNTU5MDAgMTcuNDYwMDAgOS4wMzcwMCAxOC4yMzQwMCAxMS45OTUwMCAxOC4yMzQwMCBjCjEyLjk5OTAwIDE4LjIzNDAwIDEzLjg3OTAwIDE4LjE1MTAwIDE0LjYzODAwIDE3Ljk4ODAwIGMKMTUuMzk3MDAgMTcuODI1MDAgMTYuMTcyMDAgMTcuNTM3MDAgMTYuOTYyMDAgMTcuMTI1MDAgYwoxNi40ODAwMCAxNS42NDQwMCAxNi4xNTcwMCAxNC4wOTYwMCAxNS45OTMwMCAxMi40NzkwMCBjCjE0LjkwMjAwIDEyLjQ3OTAwIGwKMTQuOTIwMDAgMTMuNzYzMDAgbAoxNC45MDIwMCAxNS4wNjMwMCBsCjE0LjIzOTAwIDE1LjY5NTAwIDEzLjU4ODAwIDE2LjEyOTAwIDEyLjk1MDAwIDE2LjM2NjAwIGMKMTIuMzEyMDAgMTYuNjAwMDAgMTEuNTIyMDAgMTYuNzIwMDAgMTAuNTg0MDAgMTYuNzIwMDAgYwo4LjcyMDAwIDE2LjcyMDAwIDcuMjM2MDAgMTYuMjMxMDAgNi4xMzUwMCAxNS4yNTQwMCBjCjUuMDMzMDAgMTQuMjc4MDAgNC40ODMwMCAxMy4wMDQwMCA0LjQ4MzAwIDExLjQzNjAwIGMKNC40ODMwMCAxMC41OTAwMCA0LjYzODAwIDkuOTE4MDAgNC45NTAwMCA5LjQyMDAwIGMKNS4yNjIwMCA4LjkyMjAwIDUuNzUxMDAgOC41MDIwMCA2LjQxOTAwIDguMTU5MDAgYwo3LjA4NTAwIDcuODE3MDAgOC4zMDQwMCA3LjQ2NjAwIDEwLjA3NDAwIDcuMTA1MDAgYwoxMS45MzEwMCA2LjczMDAwIDEzLjIwNjAwIDYuMzUzMDAgMTMuODk4MDAgNS45NzgwMCBjCjE0LjU4OTAwIDUuNjA0MDAgMTUuMTMxMDAgNS4wNzAwMCAxNS41MjQwMCA0LjM3NTAwIGMKMTUuOTE2MDAgMy42ODAwMCAxNi4xMTQwMCAyLjg3MDAwIDE2LjExNDAwIDEuOTQ0MDAgYwoxNi4xMTQwMCAtMC4yNzEwMCAxNS4wMjAwMCAtMi4yNjgwMCAxMi44MzIwMCAtNC4wNTEwMCBjCjEwLjY0NDAwIC01LjgzNDAwIDguMDg3MDAgLTYuNzI0MDAgNS4xNjEwMCAtNi43MjQwMCBjCjQuMDYzMDAgLTYuNzI0MDAgMy4wMDIwMCAtNi42MDEwMCAxLjk4MzAwIC02LjM1NzAwIGMKMC45NjUwMCAtNi4xMTEwMCAtMC4wMjAwMCAtNS43NDkwMCAtMC45NjkwMCAtNS4yNzAwMCBjCi0wLjU1OTAwIC0zLjY5NzAwIC0wLjIzNzAwIC0xLjk0MDAwIDAgMCBjCmYKUQpxCjEgMCAwIDEgNTQuOTQ2NzAgNzQ0LjM5MDYwIGNtCjAgMCBtCjAgMC45NjkwMCBsCjEuNDQyMDAgMS4wNDMwMCBsCjIuMzY4MDAgMS4xMDYwMCAyLjk0MDAwIDEuMjEwMDAgMy4xNjAwMCAxLjM1NDAwIGMKMy4zNzcwMCAxLjQ5NzAwIDMuNTQ0MDAgMS43MjcwMCAzLjY1NjAwIDIuMDM5MDAgYwozLjkzMDAwIDIuNzM5MDAgNC41MzAwMCA1LjY5ODAwIDUuNDU2MDAgMTAuOTE2MDAgYwo2LjE3MzAwIDE1LjAwNjAwIDYuODE1MDAgMTguOTYxMDAgNy4zNzcwMCAyMi43NzgwMCBjCjMuNjk5MDAgMjIuNzQwMDAgMS42MDcwMCAyMi42OTIwMCAxLjEwMjAwIDIyLjYzNjAwIGMKMC41OTQwMCAyMi41NzkwMCAwLjIyOTAwIDIyLjQzNzAwIDAuMDA0MDAgMjIuMjEwMDAgYwotMC4zMzUwMCAyMS45MDcwMCAtMC42NDgwMCAyMS4xNjgwMCAtMC45MzUwMCAxOS45OTcwMCBjCi0xLjMxMDAwIDE4LjU5ODAwIGwKLTIuNDAwMDAgMTguNTk4MDAgbAotMS44OTUwMCAyMi40MjIwMCBsCi0xLjg1NzAwIDIyLjg5OTAwIC0xLjgzMTAwIDIzLjUwMzAwIC0xLjgxNzAwIDI0LjIzMjAwIGMKMi4yNTUwMCAyNC4xNTAwMCA1LjcwMjAwIDI0LjExMDAwIDguNTI1MDAgMjQuMTEwMDAgYwoxMS4yMjIwMCAyNC4xMTAwMCAxNS4wMzEwMCAyNC4xNTAwMCAxOS45NTMwMCAyNC4yMzIwMCBjCjE5LjU3MjAwIDIyLjc0OTAwIDE5LjIyODAwIDIwLjg3MTAwIDE4LjkyMzAwIDE4LjU5ODAwIGMKMTcuODMzMDAgMTguNTk4MDAgbAoxNy45NjUwMCAyMS4zNTgwMCBsCjE3Ljk2NTAwIDIxLjkyNjAwIDE3LjcyNTAwIDIyLjI5MTAwIDE3LjI0ODAwIDIyLjQ1NjAwIGMKMTYuNzY5MDAgMjIuNjE5MDAgMTQuNTQ2MDAgMjIuNzI2MDAgMTAuNTc3MDAgMjIuNzc4MDAgYwo4LjE1NTAwIDEwLjEwMjAwIDYuOTQ5MDAgMy4yMjAwMCA2Ljk1NTAwIDIuMTMyMDAgYwo2Ljk1NTAwIDEuNjgxMDAgNy4xMTIwMCAxLjM4ODAwIDcuNDI1MDAgMS4yNTAwMCBjCjcuNzM3MDAgMS4xMTEwMCA4LjgzMTAwIDEuMDE5MDAgMTAuNzA3MDAgMC45NjkwMCBjCjEwLjU1NjAwIDAgbAo4Ljc4MDAwIDAuMDgxMDAgNy4wMDYwMCAwLjEyMTAwIDUuMjMxMDAgMC4xMjEwMCBjCjMuMzQzMDAgMC4xMjEwMCAxLjYwMDAwIDAuMDgxMDAgMCAwIGMKZgpRCnEKMSAwIDAgMSA4Mi4zMjE2MCA3NjguOTg1NjAgY20KMCAwIG0KLTEzLjY1NTAwIC0yMS4wMDgwMCBsCi0xNC42MjkwMCAtMjIuNDYzMDAgLTE1LjIyODAwIC0yMy4yNTMwMCAtMTUuNDUzMDAgLTIzLjM4MDAwIGMKLTE1LjY3OTAwIC0yMy41MDYwMCAtMTYuMzAyMDAgLTIzLjU4ODAwIC0xNy4zMjUwMCAtMjMuNjI2MDAgYwotMTcuMzI1MDAgLTI0LjU5NTAwIGwKLTE1LjkyOTAwIC0yNC41MTQwMCAtMTQuNjMzMDAgLTI0LjQ3NDAwIC0xMy40MzUwMCAtMjQuNDc0MDAgYwotMTIuMzUwMDAgLTI0LjQ3NDAwIC0xMC44MjAwMCAtMjQuNTE0MDAgLTguODQ0MDAgLTI0LjU5NTAwIGMKLTguODQ0MDAgLTIzLjYyNjAwIGwKLTEwLjUyMDAwIC0yMy41MjUwMCAtMTEuNDk5MDAgLTIzLjQxNzAwIC0xMS43ODAwMCAtMjMuMzA0MDAgYwotMTIuMDYxMDAgLTIzLjE5MDAwIC0xMi4yMDEwMCAtMjMuMDIwMDAgLTEyLjIwMTAwIC0yMi43OTEwMCBjCi0xMi4yMDEwMCAtMjIuNTg4MDAgLTEyLjA5NTAwIC0yMi4yNzIwMCAtMTEuODgzMDAgLTIxLjg0MjAwIGMKLTExLjYwODAwIC0yMS4yNjEwMCAtMTAuNzA0MDAgLTE5LjczNTAwIC05LjE3MzAwIC0xNy4yNjkwMCBjCi03LjYyOTAwIC0xNC43ODEwMCBsCjEuOTQwMDAgLTE0Ljc4MTAwIGwKMy4wNDIwMCAtMjAuNTg5MDAgbAozLjIzMDAwIC0yMS41NzcwMCAzLjMyMzAwIC0yMi4yNDIwMCAzLjMyMzAwIC0yMi41ODMwMCBjCjMuMzIzMDAgLTIyLjkzNzAwIDMuMTk4MDAgLTIzLjE5NDAwIDIuOTQ4MDAgLTIzLjM1MTAwIGMKMi42OTkwMCAtMjMuNTEwMDAgMi4xNDgwMCAtMjMuNTk1MDAgMS4yOTkwMCAtMjMuNjA3MDAgYwowLjAyMzAwIC0yMy42MjYwMCBsCjAuMDIzMDAgLTI0LjU5NTAwIGwKMS40MTAwMCAtMjQuNTE0MDAgMi45NzMwMCAtMjQuNDc0MDAgNC43MTAwMCAtMjQuNDc0MDAgYwo2LjQ5NjAwIC0yNC40NzQwMCA4LjA3NjAwIC0yNC41MTQwMCA5LjQ1MDAwIC0yNC41OTUwMCBjCjkuNDUwMDAgLTIzLjYyNjAwIGwKOC4wNzgwMCAtMjMuNTI1MDAgNy4yNzkwMCAtMjMuNDI3MDAgNy4wNTMwMCAtMjMuMzMyMDAgYwo2LjgyNjAwIC0yMy4yMzcwMCA2LjY3NjAwIC0yMy4xMDcwMCA2LjYwMDAwIC0yMi45NDIwMCBjCjYuNDYyMDAgLTIyLjY2NDAwIDYuMjM1MDAgLTIxLjc1MzAwIDUuOTIxMDAgLTIwLjIxMTAwIGMKMS44MTcwMCAwIGwKaAoxLjY4OTAwIC0xMy40NDgwMCBtCjAuMTcwMDAgLTEzLjUzMDAwIC0xLjA5MTAwIC0xMy41NzAwMCAtMi4wOTUwMCAtMTMuNTcwMDAgYwotMy4xODcwMCAtMTMuNTcwMDAgLTQuNzQzMDAgLTEzLjUzMDAwIC02Ljc2MzAwIC0xMy40NDgwMCBjCi0wLjI5NDAwIC0zLjM5NjAwIGwKaApmClEKcQoxIDAgMCAxIDg4LjczNDMwIDc0NC4zOTA2MCBjbQowIDAgbQowIDAuOTY5MDAgbAoxLjE4MzAwIDEuMDA3MDAgbAoyLjA2OTAwIDEuMDQzMDAgMi42MTYwMCAxLjEzODAwIDIuODIzMDAgMS4yODcwMCBjCjMuMDI5MDAgMS40MzcwMCAzLjIxNDAwIDEuNzk1MDAgMy4zNzcwMCAyLjM1NzAwIGMKMy42NjMwMCAzLjQyMTAwIDQuMzgxMDAgNi45MjkwMCA1LjUzMDAwIDEyLjg4NDAwIGMKNi40ODAwMCAxNy45MjYwMCA2Ljk1NTAwIDIwLjkyODAwIDYuOTU1MDAgMjEuODkyMDAgYwo2Ljk1NTAwIDIyLjM2NzAwIDYuNzkwMDAgMjIuNjkyMDAgNi40NTkwMCAyMi44NjcwMCBjCjYuMTI4MDAgMjMuMDQzMDAgNS4xMTEwMCAyMy4xNzMwMCAzLjQxMTAwIDIzLjI2MjAwIGMKMy40MTEwMCAyNC4yMzIwMCBsCjYuMDg2MDAgMjQuMTUwMDAgOC4wMDYwMCAyNC4xMTAwMCA5LjE3MDAwIDI0LjExMDAwIGMKMTAuMDk2MDAgMjQuMTEwMDAgMTEuMjc5MDAgMjQuMTM3MDAgMTIuNzE4MDAgMjQuMTkwMDAgYwoxMy43MDYwMCAyNC4yMTYwMCAxNC41MTIwMCAyNC4yMzIwMCAxNS4xMzkwMCAyNC4yMzIwMCBjCjE3LjQ0MTAwIDI0LjIzMjAwIDE5LjEzNzAwIDIzLjgwMDAwIDIwLjIyNjAwIDIyLjk0MDAwIGMKMjEuMzE0MDAgMjIuMDgxMDAgMjEuODU3MDAgMjAuOTI4MDAgMjEuODU3MDAgMTkuNDg0MDAgYwoyMS44NTcwMCAxOC41MjQwMCAyMS41NDkwMCAxNy41MzkwMCAyMC45MzQwMCAxNi41MzAwMCBjCjIwLjMxNjAwIDE1LjUyMTAwIDE5LjQ4MDAwIDE0LjY4MzAwIDE4LjQyMDAwIDE0LjAxNjAwIGMKMTcuMzU5MDAgMTMuMzUwMDAgMTUuODkyMDAgMTIuODE2MDAgMTQuMDE4MDAgMTIuNDE5MDAgYwoxNi4xMTIwMCA5LjM3ODAwIDE4LjEwMDAwIDYuNjMyMDAgMTkuOTgzMDAgNC4xNzgwMCBjCjIxLjI2MzAwIDIuNDg5MDAgMjIuMDgxMDAgMS41MzcwMCAyMi40MzkwMCAxLjMyNTAwIGMKMjIuNzIxMDAgMS4xNDkwMCAyMy4yNzMwMCAxLjAzMjAwIDI0LjA5OTAwIDAuOTY5MDAgYwoyNC4wOTkwMCAwIGwKMjIuOTc4MDAgMC4wODEwMCAyMi4wMzcwMCAwLjEyMTAwIDIxLjI3NjAwIDAuMTIxMDAgYwoyMC41NjYwMCAwLjEyMTAwIDE5LjgxMzAwIDAuMDgxMDAgMTkuMDE2MDAgMCBjCjE3Ljg5MzAwIDEuODg4MDAgMTYuNDM5MDAgNC4wOTcwMCAxNC42NTAwMCA2LjYyODAwIGMKMTIuODYyMDAgOS4xNTkwMCAxMS4yNzcwMCAxMS4yNTEwMCA5Ljg5MzAwIDEyLjkwMTAwIGMKOS44OTMwMCAxMy4yNzYwMCBsCjEwLjY4NTAwIDEzLjE4OTAwIDExLjI1MzAwIDEzLjE0NjAwIDExLjU5OTAwIDEzLjE0NjAwIGMKMTIuNzU5MDAgMTMuMTQ2MDAgMTMuOTE2MDAgMTMuNDE2MDAgMTUuMDY1MDAgMTMuOTYyMDAgYwoxNi4yMTQwMCAxNC41MDUwMCAxNy4wODkwMCAxNS4yMjQwMCAxNy42ODcwMCAxNi4xMjEwMCBjCjE4LjI4NzAwIDE3LjAxNzAwIDE4LjU4NjAwIDE3Ljk3MTAwIDE4LjU4NjAwIDE4Ljk4NjAwIGMKMTguNTg2MDAgMjAuMjIyMDAgMTguMTc1MDAgMjEuMTg0MDAgMTcuMzUyMDAgMjEuODY5MDAgYwoxNi41MzAwMCAyMi41NTQwMCAxNS4yNzcwMCAyMi44OTkwMCAxMy41OTYwMCAyMi44OTkwMCBjCjEyLjY5MzAwIDIyLjg5OTAwIDExLjY1NDAwIDIyLjc5NjAwIDEwLjQ3ODAwIDIyLjU5NjAwIGMKOS42NzQwMCAxOC44NDkwMCA4LjY5NTAwIDEzLjc5MTAwIDcuNTQ0MDAgNy40MjUwMCBjCjYuOTkxMDAgNC40NzIwMCA2LjcxNTAwIDIuNjgyMDAgNi43MTUwMCAyLjA1ODAwIGMKNi43MTUwMCAxLjYzMjAwIDYuOTAwMDAgMS4zNTgwMCA3LjI3MzAwIDEuMjMzMDAgYwo3LjQ3ODAwIDEuMTY4MDAgOC40MjQwMCAxLjA4MTAwIDEwLjExMzAwIDAuOTY5MDAgYwo5Ljk2MzAwIDAgbAo4LjIzNTAwIDAuMDgxMDAgNi43NjAwMCAwLjEyMTAwIDUuNTQxMDAgMC4xMjEwMCBjCjQuMjU5MDAgMC4xMjEwMCAyLjQxNDAwIDAuMDgxMDAgMCAwIGMKZgpRCnEKMSAwIDAgMSA0OS4wNzIwMCA3NDAuMzQxNTAgY20KMCAwIG0KMS4zNTQwMCAwIGwKMC4zNzYwMCAtNS4xNTUwMCBsCjIuODEyMDAgLTUuMTU1MDAgbAoyLjU4NjAwIC02LjM0MDAwIGwKLTEuMjA0MDAgLTYuMzQwMDAgbApoCmYKUQpxCjEgMCAwIDEgNTQuODYzOTAgNzQwLjM0MTUwIGNtCjAgMCBtCi0xLjIwNDAwIC02LjM0MDAwIGwKLTIuNTU4MDAgLTYuMzQwMDAgbAotMS4zNTUwMCAwIGwKaApmClEKcQoxIDAgMCAxIDU2LjAwMDgwIDc0MC4zNDE1MCBjbQowIDAgbQozLjcxNTAwIDAgbAozLjQ4OTAwIC0xLjE1ODAwIGwKMS4xMzgwMCAtMS4xNTgwMCBsCjAuODQ2MDAgLTIuNjQ0MDAgbAozLjA1NzAwIC0yLjY0NDAwIGwKMi44MzEwMCAtMy43ODIwMCBsCjAuNjMwMDAgLTMuNzgyMDAgbAowLjE1MDAwIC02LjM0MDAwIGwKLTEuMjA0MDAgLTYuMzQwMDAgbApoCmYKUQpxCjEgMCAwIDEgNjMuNjA4MDAgNzM2LjcwMTEwIGNtCjAgMCBtCi0yLjI0ODAwIDAgbAotMi41NDAwMCAtMS41NDMwMCBsCi0wLjAwOTAwIC0xLjU0MzAwIGwKLTAuMjM1MDAgLTIuNjk5MDAgbAotNC4xMjAwMCAtMi42OTkwMCBsCi0yLjkxNjAwIDMuNjQwMDAgbAowLjgzNzAwIDMuNjQwMDAgbAowLjYxMjAwIDIuNDgzMDAgbAotMS43NzgwMCAyLjQ4MzAwIGwKLTIuMDQxMDAgMS4xMzgwMCBsCjAuMjI2MDAgMS4xMzgwMCBsCmgKZgpRCnEKMSAwIDAgMSA2OC4zODMzMCA3NDAuMzQxNTAgY20KMCAwIG0KLTEuMjA0MDAgLTYuMzQwMDAgbAotMi41NTgwMCAtNi4zNDAwMCBsCi0xLjM1NDAwIDAgbApoCmYKUQpxCjEgMCAwIDEgNjguMzE2MDAgNzM0LjAwMTYwIGNtCjAgMCBtCjEuMjA0MDAgNi4zNDAwMCBsCjIuNzk0MDAgNi4zNDAwMCBsCjMuNjMwMDAgMy45MjIwMCBsCjMuODg1MDAgMy4xMTMwMCA0LjA1NDAwIDIuNDU0MDAgNC4yMDUwMCAxLjc1OTAwIGMKNC4yMzMwMCAxLjc1OTAwIGwKNC4yNjEwMCAyLjQwODAwIDQuMzU1MDAgMy4xMDQwMCA0LjUyNDAwIDQuMDM1MDAgYwo0Ljk1NzAwIDYuMzQwMDAgbAo2LjIyNzAwIDYuMzQwMDAgbAo1LjAyMzAwIDAgbAozLjYyMTAwIDAgbAoyLjczNzAwIDIuNTMwMDAgbAoyLjQ1NTAwIDMuMzk1MDAgMi4yODUwMCAzLjk2OTAwIDIuMTE2MDAgNC43NDAwMCBjCjIuMDg4MDAgNC43NDAwMCBsCjIuMDIyMDAgNC4xMTkwMCAxLjg3MjAwIDMuMjI2MDAgMS42ODQwMCAyLjIxOTAwIGMKMS4yNjAwMCAwIGwKaApmClEKcQoxIDAgMCAxIDc0Ljc0NjgwIDczNS40Njg1MCBjbQowIDAgbQowLjM3NjAwIC0wLjIyNTAwIDAuOTAzMDAgLTAuNDA0MDAgMS40OTYwMCAtMC40MDQwMCBjCjIuMDEzMDAgLTAuNDA0MDAgMi40OTMwMCAtMC4xNjAwMCAyLjQ5MzAwIDAuMzMwMDAgYwoyLjQ5MzAwIDAuNjk2MDAgMi4yMjAwMCAwLjkyMjAwIDEuNjM3MDAgMS4yMzMwMCBjCjAuOTY5MDAgMS41ODkwMCAwLjMyOTAwIDIuMDk4MDAgMC4zMjkwMCAyLjkxNjAwIGMKMC4zMjkwMCA0LjE5NTAwIDEuNDM5MDAgNC45NzYwMCAyLjgyMjAwIDQuOTc2MDAgYwozLjU4NDAwIDQuOTc2MDAgNC4wMzUwMCA0LjgwNzAwIDQuMzA4MDAgNC42NjYwMCBjCjMuODg1MDAgMy41MzcwMCBsCjMuNjc4MDAgMy42NTAwMCAzLjIzNjAwIDMuODE5MDAgMi43MDkwMCAzLjgxMDAwIGMKMi4wNzkwMCAzLjgxMDAwIDEuNzUwMDAgMy41MDAwMCAxLjc1MDAwIDMuMTUxMDAgYwoxLjc1MDAwIDIuNzc1MDAgMi4xMzUwMCAyLjU0OTAwIDIuNjcxMDAgMi4yNDgwMCBjCjMuNDQzMDAgMS44NDQwMCAzLjkyMzAwIDEuMzI2MDAgMy45MjMwMCAwLjU2NDAwIGMKMy45MjMwMCAtMC44NDYwMCAyLjc1NjAwIC0xLjU3MTAwIDEuMzU0MDAgLTEuNTcxMDAgYwowLjQ3OTAwIC0xLjU2MjAwIC0wLjE2MDAwIC0xLjMzNTAwIC0wLjQ1MTAwIC0xLjEyMDAwIGMKaApmClEKcQoxIDAgMCAxIDgxLjY3NjkwIDc0MC4zNDE1MCBjbQowIDAgbQotMC42ODcwMCAtMy42NDAwMCBsCi0wLjczNDAwIC0zLjg2NzAwIC0wLjc2MjAwIC00LjEyMDAwIC0wLjc2MjAwIC00LjM5MzAwIGMKLTAuNzYyMDAgLTQuOTExMDAgLTAuNDQyMDAgLTUuMzA1MDAgMC4xNjAwMCAtNS4zMDUwMCBjCjAuODc1MDAgLTUuMzA1MDAgMS4zMzYwMCAtNC44MzUwMCAxLjU2MTAwIC0zLjY1OTAwIGMKMi4yNTgwMCAwIGwKMy42MTIwMCAwIGwKMi45MjUwMCAtMy42MDMwMCBsCjIuNTU4MDAgLTUuNTIyMDAgMS43MjEwMCAtNi40NDQwMCAtMC4wMDkwMCAtNi40NDQwMCBjCi0xLjMxNzAwIC02LjQ1MzAwIC0yLjExNjAwIC01Ljc2NjAwIC0yLjExNjAwIC00LjQyMTAwIGMKLTIuMTE2MDAgLTQuMTQ5MDAgLTIuMDc5MDAgLTMuODM4MDAgLTIuMDIyMDAgLTMuNTM3MDAgYwotMS4zNTQwMCAwIGwKaApmClEKcQoxIDAgMCAxIDg3LjIxNDcwIDczNy41MTg3MCBjbQowIDAgbQowLjU1NTAwIDAgbAoxLjI4OTAwIDAgMS44MjUwMCAwLjQ0MjAwIDEuODI1MDAgMS4wNjMwMCBjCjEuODI1MDAgMS41NzEwMCAxLjQwMTAwIDEuNzk3MDAgMC44NTYwMCAxLjc5NzAwIGMKMC42MDIwMCAxLjc5NzAwIDAuNDQyMDAgMS43NzkwMCAwLjMyOTAwIDEuNzUwMDAgYwpoCi0wLjgyODAwIDIuNzAwMDAgbQotMC40MDUwMCAyLjgwNDAwIDAuMjM1MDAgMi44NjAwMCAwLjg2NTAwIDIuODYwMDAgYwoxLjQ2NzAwIDIuODYwMDAgMi4wODgwMCAyLjc3NjAwIDIuNTMwMDAgMi40OTMwMCBjCjIuOTQ0MDAgMi4yNDgwMCAzLjIzNjAwIDEuODUzMDAgMy4yMzYwMCAxLjI2MTAwIGMKMy4yMzYwMCAwLjMyMDAwIDIuNjE1MDAgLTAuMjYzMDAgMS43OTcwMCAtMC41MjYwMCBjCjEuNzk3MDAgLTAuNTU1MDAgbAoyLjE3MzAwIC0wLjcyNDAwIDIuMzQyMDAgLTEuMTM4MDAgMi4zOTkwMCAtMS43MTEwMCBjCjIuNDgzMDAgLTIuNDI2MDAgMi41MzAwMCAtMy4yNTQwMCAyLjY0MzAwIC0zLjUxNzAwIGMKMS4yNTEwMCAtMy41MTcwMCBsCjEuMTk1MDAgLTMuMzQ4MDAgMS4xMjkwMCAtMi44MDIwMCAxLjA3MjAwIC0yLjAyMjAwIGMKMS4wMDYwMCAtMS4yNTAwMCAwLjc0MzAwIC0xLjAxNTAwIDAuMjA3MDAgLTEuMDE1MDAgYwotMC4yMDcwMCAtMS4wMTUwMCBsCi0wLjY3NzAwIC0zLjUxNzAwIGwKLTIuMDEzMDAgLTMuNTE3MDAgbApoCmYKUQpxCjEgMCAwIDEgOTQuMTcyMjAgNzM2LjY4MjEwIGNtCjAgMCBtCi0wLjEyMjAwIDEuMzY0MDAgbAotMC4xNTEwMCAxLjcxMjAwIC0wLjE3OTAwIDIuMjIwMDAgLTAuMjA3MDAgMi42MDYwMCBjCi0wLjIzNTAwIDIuNjA2MDAgbAotMC4zOTUwMCAyLjIyMDAwIC0wLjU3NDAwIDEuNzMwMDAgLTAuNzQzMDAgMS4zNjQwMCBjCi0xLjM2NDAwIDAgbApoCi0xLjc2OTAwIC0xLjAzNTAwIG0KLTIuNTEyMDAgLTIuNjgwMDAgbAotMy45NjAwMCAtMi42ODAwMCBsCi0wLjg2NjAwIDMuNjU5MDAgbAowLjg3NTAwIDMuNjU5MDAgbAoxLjYwODAwIC0yLjY4MDAwIGwKMC4xODgwMCAtMi42ODAwMCBsCjAuMDU2MDAgLTEuMDM1MDAgbApoCmYKUQpxCjEgMCAwIDEgOTYuMzUyMTAgNzM0LjAwMTYwIGNtCjAgMCBtCjEuMjA0MDAgNi4zNDAwMCBsCjIuNzkzMDAgNi4zNDAwMCBsCjMuNjMwMDAgMy45MjIwMCBsCjMuODg1MDAgMy4xMTMwMCA0LjA1NDAwIDIuNDU0MDAgNC4yMDQwMCAxLjc1OTAwIGMKNC4yMzIwMCAxLjc1OTAwIGwKNC4yNjEwMCAyLjQwODAwIDQuMzU1MDAgMy4xMDQwMCA0LjUyNDAwIDQuMDM1MDAgYwo0Ljk1NzAwIDYuMzQwMDAgbAo2LjIyNzAwIDYuMzQwMDAgbAo1LjAyMzAwIDAgbAozLjYyMjAwIDAgbAoyLjczNzAwIDIuNTMwMDAgbAoyLjQ1NTAwIDMuMzk1MDAgMi4yODUwMCAzLjk2OTAwIDIuMTE3MDAgNC43NDAwMCBjCjIuMDg4MDAgNC43NDAwMCBsCjIuMDIyMDAgNC4xMTkwMCAxLjg3MjAwIDMuMjI2MDAgMS42ODQwMCAyLjIxOTAwIGMKMS4yNjAwMCAwIGwKaApmClEKcQoxIDAgMCAxIDEwNy4yNTA2MCA3MzQuMTcxMDAgY20KMCAwIG0KLTAuMzM5MDAgLTAuMTUxMDAgLTAuOTAzMDAgLTAuMjc0MDAgLTEuNjY1MDAgLTAuMjc0MDAgYwotMy4zNDkwMCAtMC4yNzQwMCAtNC40NjgwMCAwLjc0MzAwIC00LjQ2ODAwIDIuNDE3MDAgYwotNC40NjgwMCAzLjg1NjAwIC0zLjgyOTAwIDQuOTY3MDAgLTIuODY5MDAgNS42MDUwMCBjCi0yLjIxMTAwIDYuMDU3MDAgLTEuNDM5MDAgNi4yNzMwMCAtMC41NDYwMCA2LjI3MzAwIGMKMC4xNDEwMCA2LjI3MzAwIDAuNjU4MDAgNi4xMTMwMCAwLjg0NjAwIDYuMDEwMDAgYwowLjQ3MDAwIDQuOTEwMDAgbAowLjI5MTAwIDUuMDAzMDAgLTAuMTMyMDAgNS4xMTcwMCAtMC42NzgwMCA1LjExNzAwIGMKLTEuMjIzMDAgNS4xMTcwMCAtMS43NDAwMCA0Ljk0NzAwIC0yLjEyNjAwIDQuNjI4MDAgYwotMi42NTMwMCA0LjE4NTAwIC0zLjAwMTAwIDMuNDYxMDAgLTMuMDAxMDAgMi41NzcwMCBjCi0zLjAwMTAwIDEuNTYxMDAgLTIuNDI3MDAgMC44ODQwMCAtMS4zMTcwMCAwLjg4NDAwIGMKLTAuODY2MDAgMC44ODQwMCAtMC40MjQwMCAwLjk1OTAwIC0wLjExMzAwIDEuMDkwMDAgYwpoCmYKUQpxCjEgMCAwIDEgMTExLjk4OTAwIDczNi43MDExMCBjbQowIDAgbQotMi4yNDgwMCAwIGwKLTIuNTQwMDAgLTEuNTQzMDAgbAotMC4wMTAwMCAtMS41NDMwMCBsCi0wLjIzNTAwIC0yLjY5OTAwIGwKLTQuMTIwMDAgLTIuNjk5MDAgbAotMi45MTYwMCAzLjY0MDAwIGwKMC44MzcwMCAzLjY0MDAwIGwKMC42MTEwMCAyLjQ4MzAwIGwKLTEuNzc4MDAgMi40ODMwMCBsCi0yLjA0MTAwIDEuMTM4MDAgbAowLjIyNjAwIDEuMTM4MDAgbApoCmYKUQpxCjEgMCAwIDEgNTEuMDAwNTAgNzI1LjQ0NzYwIGNtCjAgMCBtCi0wLjMzODAwIC0wLjE1MTAwIC0wLjkwMzAwIC0wLjI3MzAwIC0xLjY2NDAwIC0wLjI3MzAwIGMKLTMuMzQ4MDAgLTAuMjczMDAgLTQuNDY4MDAgMC43NDMwMCAtNC40NjgwMCAyLjQxODAwIGMKLTQuNDY4MDAgMy44NTYwMCAtMy44MjgwMCA0Ljk2NzAwIC0yLjg2ODAwIDUuNjA1MDAgYwotMi4yMTAwMCA2LjA1NzAwIC0xLjQzOTAwIDYuMjc0MDAgLTAuNTQ1MDAgNi4yNzQwMCBjCjAuMTQyMDAgNi4yNzQwMCAwLjY1OTAwIDYuMTE0MDAgMC44NDcwMCA2LjAxMDAwIGMKMC40NzAwMCA0LjkxMDAwIGwKMC4yOTIwMCA1LjAwMzAwIC0wLjEzMjAwIDUuMTE3MDAgLTAuNjc3MDAgNS4xMTcwMCBjCi0xLjIyMjAwIDUuMTE3MDAgLTEuNzQwMDAgNC45NDgwMCAtMi4xMjUwMCA0LjYyODAwIGMKLTIuNjUyMDAgNC4xODYwMCAtMyAzLjQ2MTAwIC0zIDIuNTc3MDAgYwotMyAxLjU2MTAwIC0yLjQyNjAwIDAuODg0MDAgLTEuMzE3MDAgMC44ODQwMCBjCi0wLjg2NTAwIDAuODg0MDAgLTAuNDIzMDAgMC45NTkwMCAtMC4xMTMwMCAxLjA5MDAwIGMKaApmClEKcQoxIDAgMCAxIDU0LjQyMjQwIDcyNi4zMDMzMCBjbQowIDAgbQoxLjA5MTAwIDAgMS44MzQwMCAxLjU0MzAwIDEuODM0MDAgMi43MzcwMCBjCjEuODM0MDAgMy41MjgwMCAxLjU1MjAwIDQuMjg5MDAgMC41NTUwMCA0LjI4OTAwIGMKLTAuNTgzMDAgNC4yODkwMCAtMS4zMjYwMCAyLjc2NjAwIC0xLjMyNjAwIDEuNTYyMDAgYwotMS4zMjYwMCAwLjY2ODAwIC0wLjkyMjAwIDAgLTAuMDEwMDAgMCBjCmgKLTAuMTg4MDAgLTEuMTI5MDAgbQotMS44MzQwMCAtMS4xMjkwMCAtMi43NTYwMCAwLjAzODAwIC0yLjc1NjAwIDEuNTE0MDAgYwotMi43NTYwMCAyLjY1MjAwIC0yLjMzMzAwIDMuNzcyMDAgLTEuNjA4MDAgNC40OTYwMCBjCi0xLjAxNjAwIDUuMDcwMDAgLTAuMTk3MDAgNS40MTgwMCAwLjcyNDAwIDUuNDE4MDAgYwoyLjM5OTAwIDUuNDE4MDAgMy4yODMwMCA0LjI4OTAwIDMuMjgzMDAgMi43NzUwMCBjCjMuMjgzMDAgMS42MjcwMCAyLjg3ODAwIDAuNDk5MDAgMi4xNjQwMCAtMC4yMDcwMCBjCjEuNTgwMDAgLTAuNzkwMDAgMC43NzEwMCAtMS4xMjkwMCAtMC4xNzgwMCAtMS4xMjkwMCBjCmgKZgpRCnEKMSAwIDAgMSA2My4yMDQzMCA3MjUuMjc4MzAgY20KMCAwIG0KMC4zODYwMCAyLjYxNDAwIGwKMC40ODkwMCAzLjMwMTAwIDAuNjMwMDAgNC4xNzYwMCAwLjgwOTAwIDUuMDg5MDAgYwowLjc4MTAwIDUuMDg5MDAgbAowLjQzMzAwIDQuMjcwMDAgMC4wMzgwMCAzLjM5NTAwIC0wLjMxMDAwIDIuNjk5MDAgYwotMS42MDgwMCAwLjEwMzAwIGwKLTIuNjYyMDAgMC4xMDMwMCBsCi0yLjg4NzAwIDIuNjcxMDAgbAotMi45NDQwMCAzLjM2NzAwIC0yLjk5MTAwIDQuMjQyMDAgLTMuMDI5MDAgNS4wODkwMCBjCi0zLjA0NzAwIDUuMDg5MDAgbAotMy4yMjYwMCA0LjI1MjAwIC0zLjQxNDAwIDMuMjkyMDAgLTMuNTc0MDAgMi42MTQwMCBjCi00LjIwNTAwIDAgbAotNS40NDYwMCAwIGwKLTMuODU2MDAgNi4zNDAwMCBsCi0yLjA0MTAwIDYuMzQwMDAgbAotMS44NDMwMCAzLjkyMjAwIGwKLTEuODA2MDAgMy4zMzAwMCAtMS43NDAwMCAyLjYxNDAwIC0xLjczMTAwIDEuOTA5MDAgYwotMS42OTMwMCAxLjkwOTAwIGwKLTEuNDQ4MDAgMi42MTQwMCAtMS4xMTAwMCAzLjM1NzAwIC0wLjgzNzAwIDMuOTMyMDAgYwowLjMzOTAwIDYuMzQwMDAgbAoyLjE3MzAwIDYuMzQwMDAgbAoxLjMxNzAwIDAgbApoCmYKUQpxCjEgMCAwIDEgNjcuNDgwNzAgNzI4LjY2NDAwIGNtCjAgMCBtCjAuMTUxMDAgLTAuMDI4MDAgMC4yOTIwMCAtMC4wNDYwMCAwLjUyNjAwIC0wLjA0NjAwIGMKMS4zMzYwMCAtMC4wNDYwMCAxLjg3MjAwIDAuNDcwMDAgMS44NzIwMCAxLjExMDAwIGMKMS44NzIwMCAxLjcxMjAwIDEuNDM5MDAgMS45MTAwMCAwLjkxMjAwIDEuOTEwMDAgYwowLjY1ODAwIDEuOTEwMDAgMC40NzAwMCAxLjg5MTAwIDAuMzU3MDAgMS44NzIwMCBjCmgKLTAuNzgxMDAgMi44MzEwMCBtCi0wLjM3NzAwIDIuOTM1MDAgMC4yNTQwMCAyLjk5MTAwIDAuODY1MDAgMi45OTEwMCBjCjEuNDM5MDAgMi45OTEwMCAyLjA4ODAwIDIuODk3MDAgMi41NDkwMCAyLjU2ODAwIGMKMi45ODIwMCAyLjI4NjAwIDMuMjM1MDAgMS44MzQwMCAzLjIzNTAwIDEuMjIzMDAgYwozLjIzNTAwIDAuNDMyMDAgMi44NjkwMCAtMC4xNzkwMCAyLjM4MDAwIC0wLjU1NTAwIGMKMS44NjIwMCAtMC45NTkwMCAxLjExOTAwIC0xLjE0NzAwIDAuMzQ4MDAgLTEuMTQ3MDAgYwowLjEyMjAwIC0xLjE0NzAwIC0wLjA2NjAwIC0xLjExOTAwIC0wLjIwNzAwIC0xLjEwOTAwIGMKLTAuNjQwMDAgLTMuMzg2MDAgbAotMS45NjYwMCAtMy4zODYwMCBsCmgKZgpRCnEKMSAwIDAgMSA3My42MzA1MCA3MjcuOTU4NzAgY20KMCAwIG0KLTAuMTIyMDAgMS4zNjQwMCBsCi0wLjE1MTAwIDEuNzEyMDAgLTAuMTc5MDAgMi4yMjAwMCAtMC4yMDcwMCAyLjYwNjAwIGMKLTAuMjM1MDAgMi42MDYwMCBsCi0wLjM5NTAwIDIuMjIwMDAgLTAuNTc0MDAgMS43MzAwMCAtMC43NDMwMCAxLjM2NDAwIGMKLTEuMzY0MDAgMCBsCmgKLTEuNzY5MDAgLTEuMDM0MDAgbQotMi41MTIwMCAtMi42ODAwMCBsCi0zLjk2MDAwIC0yLjY4MDAwIGwKLTAuODY2MDAgMy42NTkwMCBsCjAuODc1MDAgMy42NTkwMCBsCjEuNjA4MDAgLTIuNjgwMDAgbAowLjE4ODAwIC0yLjY4MDAwIGwKMC4wNTYwMCAtMS4wMzQwMCBsCmgKZgpRCnEKMSAwIDAgMSA3NS44MDk5MCA3MjUuMjc4MzAgY20KMCAwIG0KMS4yMDQwMCA2LjM0MDAwIGwKMi43OTQwMCA2LjM0MDAwIGwKMy42MzAwMCAzLjkyMjAwIGwKMy44ODUwMCAzLjExMzAwIDQuMDU0MDAgMi40NTQwMCA0LjIwNDAwIDEuNzU5MDAgYwo0LjIzMjAwIDEuNzU5MDAgbAo0LjI2MTAwIDIuNDA4MDAgNC4zNTUwMCAzLjEwNDAwIDQuNTI0MDAgNC4wMzUwMCBjCjQuOTU3MDAgNi4zNDAwMCBsCjYuMjI3MDAgNi4zNDAwMCBsCjUuMDIzMDAgMCBsCjMuNjIxMDAgMCBsCjIuNzM3MDAgMi41MzAwMCBsCjIuNDU1MDAgMy4zOTUwMCAyLjI4NTAwIDMuOTY5MDAgMi4xMTYwMCA0Ljc0MDAwIGMKMi4wODgwMCA0Ljc0MDAwIGwKMi4wMjIwMCA0LjExOTAwIDEuODcyMDAgMy4yMjYwMCAxLjY4MzAwIDIuMjIwMDAgYwoxLjI2MDAwIDAgbApoCmYKUQpxCjEgMCAwIDEgODMuMjAwMDAgNzI1LjI3ODMwIGNtCjAgMCBtCjAuNDg5MDAgMi41ODcwMCBsCi0wLjc1MzAwIDYuMzQwMDAgbAowLjY5NjAwIDYuMzQwMDAgbAoxLjEwMTAwIDQuNzMxMDAgbAoxLjIyMzAwIDQuMjA0MDAgMS4yODkwMCAzLjkwNDAwIDEuMzQ1MDAgMy41ODQwMCBjCjEuMzY0MDAgMy41ODQwMCBsCjEuNTUyMDAgMy45MjIwMCAxLjc1MDAwIDQuMjc5MDAgMi4wMTMwMCA0Ljc0MDAwIGMKMi45NjMwMCA2LjM0MDAwIGwKNC41ODEwMCA2LjM0MDAwIGwKMS44NDQwMCAyLjYxNDAwIGwKMS4zNTQwMCAwIGwKaApmClEKUQpCVAovQ1MxIGNzCjEgc2NuCjYuODU4MjAgMCAwIDYuODU4MjAgNTAxLjM2ODIwIDc2OS44NzI2MCBUbQooQSkgVGoKMC41MDAwMCAwIFRkCihnZW50IHVzZSBvbmx5XDIyNykgVGoKKEEpIFRqCjYuODMwMDAgMCBUZAooZ2VudFwwNDMpIFRqCkVUCnEKMCA3OTIgNjEyIC03OTIgcmUKVwpuCi9DUzEgQ1MKMSBTQ04KNCBNCjQ3NC4wOTgwMCA3MjcuNzkyMDAgMTIxLjQwMjAwIDQ4LjkzOTAwIHJlClMKUQpCVAovVDFfMCAxIFRmCjYuODU4MjAgMCAwIDYuODU4MjAgNTAwLjg1MzUwIDcyNS43NjM3MCBUbQooICkgVGoKLTAuOTIxMDAgMi41MDkwMCBUZAooU2VsZWN0IG9ubHkgb25lIHApIFRqCihyKSBUago2LjQ0NjAwIDAgVGQKKG9kdWN0IHBlKSBUagoocikgVGoKMy41MDUwMCAwIFRkCiggYXBwXDA3MikgVGoKRVQKcQowIDc5MiA2MTIgLTc5MiByZQpXCm4KL0NTMSBDUwoxIFNDTgowLjc1MDAwIHcKNTIyLjAxODAwIDc1MS41NDYwMCAxMS41MDcwMCAxNC4xNzQwMCByZQpTCjUwNy43MzAwMCA3NTEuNTQ2MDAgMTEuNTA3MDAgMTQuMTc0MDAgcmUKUwo0NzkuMTU0MDAgNzUxLjU0NjAwIDExLjUwNzAwIDE0LjE3NDAwIHJlClMKNDkzLjQ0MjAwIDc1MS41NDYwMCAxMS41MDcwMCAxNC4xNzQwMCByZQpTCjU3OS4xNjkwMCA3NTEuNTQ2MDAgMTEuNTA3MDAgMTQuMTc0MDAgcmUKUwo1NjQuODgxMDAgNzUxLjU0NjAwIDExLjUwNzAwIDE0LjE3NDAwIHJlClMKNTM2LjMwNTAwIDc1MS41NDYwMCAxMS41MDcwMCAxNC4xNzQwMCByZQpTCjU1MC41OTMwMCA3NTEuNTQ2MDAgMTEuNTA3MDAgMTQuMTc0MDAgcmUKUwpRCkJUCi9UMV8xIDEgVGYKMTAuNjY4MzAgMCAwIDEwLjY2ODMwIDU0MS45Nzk1MCA3MzguNDAwOTAgVG0KKCApIFRqCi9UMV8wIDEgVGYKNi44NTgyMCAwIDAgNi44NTgyMCA1MDEuMDgyMDAgNzMzLjQ3MTIwIFRtCihGUFBcMDU1Q0kpIFRqCjIuMzQ4MDAgMC4wMDgwMCBUZAooICkgVGoKMy44NzQwMCAtMC4wMDgwMCBUZAooRlBQXDA1NVRJKSBUagpFVApxCjAgNzkyIDYxMiAtNzkyIHJlClcKbgovR1MyIGdzCnEKMSAwIDAgMSA1MjUuNjM1MTAgNzM5LjE0MjEwIGNtCjAgMCBtCjAgLTYuNDM5MDAgbAotMC41NzUwMCAtNi45OTMwMCBsCi02Ljg3MDAwIC02Ljk5MzAwIGwKLTYuODcwMDAgLTAuNTEzMDAgbAotNi4zMzYwMCAwIGwKaAotNi40MzgwMCAtNi41NDIwMCA1LjY3OTAwIDUuODMzMDAgcmUKZgpRCnEKMSAwIDAgMSA1NjguMTM4NTAgNzM5LjE0MjEwIGNtCjAgMCBtCjAgLTYuNDM5MDAgbAotMC41NzUwMCAtNi45OTMwMCBsCi02Ljg3MDAwIC02Ljk5MzAwIGwKLTYuODcwMDAgLTAuNTEzMDAgbAotNi4zMzYwMCAwIGwKaAotNi40MzgwMCAtNi41NDIwMCA1LjY3OTAwIDUuODMzMDAgcmUKZgpRClEKQlQKL0NTMCBjcwowLjAwMTAwIDAuMDAyMDAgMC4wMDIwMCBzY24KMTAgMCAwIDEwIDM2Ny4zMDA4MCA2MDguMzYzODAgVG0KKFdlZWtseSApIFRqCjAgLTEuMzAwMDAgVEQKKEJpXDA1NVdlZWtseSkgVGoKVCoKKFNlbWkgTW9udGhseSkgVGoKVCoKKE1vbnRobHkpIFRqCkVUCnEKMCA3OTIgNjEyIC03OTIgcmUKVwpuCnEKMCBnCjAuNzUwMDAgdwovR1MwIGdzCi9GbTIgRG8KUQpxCjAgZwowLjc1MDAwIHcKL0dTMCBncwovRm0zIERvClEKL0NTMSBjcwoxIHNjbgo1MTMuMzUzMDAgNjQ0LjY1MDAwIDg1LjM0NzAwIDEzLjgwMDAwIHJlCmYqCjQyNC40MzYwMCA2NDQuNjUwMDAgODUuMzQ3MDAgMTMuODAwMDAgcmUKZioKUQpCVAoxIDEgMSBzY24KL1QxXzIgMSBUZgo5LjUwMDAwIDAgMCA5LjUwMDAwIDQzMy43ODg2MCA2NDguNTE3MTAgVG0KKENvdmVyYWdlIEFtb3VudCkgVGoKMTEuMTAyMDAgMCBUZAooUHJlbWl1bSkgVGoKMC4wMDEwMCAwLjAwMjAwIDAuMDAyMDAgc2NuCi9UMV8wIDEgVGYKOSAwIDAgOSA1MjQuNjE4MjAgNjI1LjcyNjYwIFRtCihcMDQ0ICkgVGoKL1RUMCAxIFRmCihcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzNykgVGoKL1QxXzAgMSBUZgoxMCAwIDAgMTAgNDMyLjIyMTIwIDU4OS41MjE1MCBUbQooRGlzYWJpbGl0eSBXYWl2ZXIgb2YgUHJlbWl1bSBcMDUwV1BcMDUxKSBUagpUKgooQXV0byBJbmNyZWFzZSBSaWRlciBcMDUwQUlSXDA1MSApIFRqCi0wLjAwNTAwIFRjClQqCihDaHJvbmljIElsbG5lc3MgUmlkZXIgXDA1MENIUlwwNTEgKSBUagovVDFfMyAxIFRmCi0wLjAxMDAwIFRjCihcMDUwRlBQXDA1NVRJIG9ubHlcMDUxKSBUagovVDFfMCAxIFRmCjAgVGMKVCoKKE90aGVyXDA3MiApIFRqCiggKSBUagovVFQwIDEgVGYKKFwxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzNykgVGoKL1QxXzAgMSBUZgoxMy40OTkwMCAwIFRkCiggKSBUagpFVApxCjAgNzkyIDYxMiAtNzkyIHJlClcKbgowLjEzNzAwIDAuMTIzMDAgMC4xMjYwMCBzY24KL0dTMiBncwpxCjEgMCAwIDEgNDI5Ljk0NDQwIDU5NS42NjM1MCBjbQowIDAgbQowIC01LjAyMDAwIGwKLTAuNDQ4MDAgLTUuNDUxMDAgbAotNS4zNTYwMCAtNS40NTEwMCBsCi01LjM1NjAwIC0wLjQwMDAwIGwKLTQuOTM5MDAgMCBsCmgKLTUuMDE5MDAgLTUuMTAwMDAgNC40MjcwMCA0LjU0NzAwIHJlCmYKUQpxCjEgMCAwIDEgNDI5Ljk0NDQwIDU1Ni45MDc3MCBjbQowIDAgbQowIC01LjAyMDAwIGwKLTAuNDQ4MDAgLTUuNDUxMDAgbAotNS4zNTYwMCAtNS40NTEwMCBsCi01LjM1NjAwIC0wLjQwMDAwIGwKLTQuOTM5MDAgMCBsCmgKLTUuMDE5MDAgLTUuMTAwMDAgNC40MjcwMCA0LjU0NzAwIHJlCmYKUQpxCjEgMCAwIDEgNDI5Ljk0NDQwIDU4My4xMjY5MCBjbQowIDAgbQowIC01LjAyMDAwIGwKLTAuNDQ4MDAgLTUuNDUxMDAgbAotNS4zNTYwMCAtNS40NTEwMCBsCi01LjM1NjAwIC0wLjQwMDAwIGwKLTQuOTM5MDAgMCBsCmgKLTUuMDE5MDAgLTUuMTAwMDAgNC40MjcwMCA0LjU0NzAwIHJlCmYKUQpxCjEgMCAwIDEgNDI5Ljk0NDQwIDU2OS4xODA1MCBjbQowIDAgbQowIC01LjAyMDAwIGwKLTAuNDQ4MDAgLTUuNDUxMDAgbAotNS4zNTYwMCAtNS40NTEwMCBsCi01LjM1NjAwIC0wLjQwMDAwIGwKLTQuOTM5MDAgMCBsCmgKLTUuMDE5MDAgLTUuMTAwMDAgNC40MjcwMCA0LjU0NzAwIHJlCmYKUQpRCkJUCi9UMV8yIDEgVGYKMTAgMCAwIDEwIDQyNC41OTA4MCA2MDAuMjE5MjAgVG0KKFJpZGVycyB0byBiZSBhZGRlZCkgVGoKL1QxXzAgMSBUZgoxLjE0MjAwIDIuNTUxMDAgVGQKKFwwNDQpIFRqCjkgMCAwIDkgNDQwLjE2MzYwIDYyNS43MjcxMCBUbQooICkgVGoKL1RUMCAxIFRmCihcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzNykgVGoKRVQKcQowIDc5MiA2MTIgLTc5MiByZQpXCm4KMC4xMzcwMCAwLjEyMzAwIDAuMTI2MDAgc2NuCi9HUzIgZ3MKcQoxIDAgMCAxIDM2NS40Mjk0MCA2MTQuNDkxNjAgY20KMCAwIG0KMCAtNS4wMjAwMCBsCi0wLjQ0ODAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTUuNDUxMDAgbAotNS4zNTYwMCAtMC40MDAwMCBsCi00LjkzOTAwIDAgbApoCi01LjAxOTAwIC01LjEwMDAwIDQuNDI3MDAgNC41NDcwMCByZQpmClEKcQoxIDAgMCAxIDM2NS40Mjk0MCA2MDEuOTY5NjAgY20KMCAwIG0KMCAtNS4wMjAwMCBsCi0wLjQ0ODAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTUuNDUxMDAgbAotNS4zNTYwMCAtMC40MDAwMCBsCi00LjkzOTAwIDAgbApoCi01LjAxOTAwIC01LjEwMDAwIDQuNDI3MDAgNC41NDcwMCByZQpmClEKcQoxIDAgMCAxIDM2NS40Mjk0MCA1NzUuMTQxMjAgY20KMCAwIG0KMCAtNS4wMjAwMCBsCi0wLjQ0ODAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTUuNDUxMDAgbAotNS4zNTYwMCAtMC40MDAwMCBsCi00LjkzOTAwIDAgbApoCi01LjAxOTAwIC01LjEwMDAwIDQuNDI3MDAgNC41NDcwMCByZQpmClEKcQoxIDAgMCAxIDM2NS40Mjk0MCA1ODguNTc4MzAgY20KMCAwIG0KMCAtNS4wMjAwMCBsCi0wLjQ0ODAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTUuNDUxMDAgbAotNS4zNTYwMCAtMC40MDAwMCBsCi00LjkzOTAwIDAgbApoCi01LjAxOTAwIC01LjEwMDAwIDQuNDI3MDAgNC41NDcwMCByZQpmClEKL0NTMSBDUwoxIFNDTgoyIHcKNCBNCi9HUzEgZ3MKcQoxIDAgMCAxIDU5OC4xMzkxMCA3NC42MjMxMCBjbQowIDAgbQotNTg3LjQ1MzAwIDAgbApTClEKUQpCVAovVDFfMiAxIFRmCjkuNTAwMDAgMCAwIDkuNTAwMDAgMzI2LjEwODkwIDYxLjc0NDEwIFRtCihUb3RhbCBFbXBsb3llZSBQcmVtaXVtKSBUagowLjkyODAwIC0xLjg5NTAwIFRkCihUb3RhbCBTcG91c2UgUHJlbWl1bSkgVGoKLTAuMzk5MDAgLTEuODk1MDAgVGQKKFRvdGFsIENoaWxkcmVuIFByZW1pdW0pIFRqCjIwLjAyNTAwIDMuODAxMDAgVGQKKFRvdGFsIFByZW1pdW0pIFRqCkVUCnEKMCA3OTIgNjEyIC03OTIgcmUKVwpuCjAuMTM3MDAgMC4xMjMwMCAwLjEyNjAwIHNjbgovR1MyIGdzCnEKMSAwIDAgMSA1MjIuMDE2NjAgNzEyLjczMjcwIGNtCjAgMCBtCjAgLTUuMDIwMDAgbAotMC40NDgwMCAtNS40NTEwMCBsCi01LjM1NjAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTAuNDAwMDAgbAotNC45MzkwMCAwIGwKaAotNS4wMTkwMCAtNS4xMDAwMCA0LjQyNzAwIDQuNTQ3MDAgcmUKZgpRCnEKMSAwIDAgMSA1OTUuNzkwNTAgNzEyLjczMjcwIGNtCjAgMCBtCjAgLTUuMDIwMDAgbAotMC40NDgwMCAtNS40NTEwMCBsCi01LjM1NjAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTAuNDAwMDAgbAotNC45MzkwMCAwIGwKaAotNS4wMTkwMCAtNS4xMDAwMCA0LjQyNzAwIDQuNTQ3MDAgcmUKZgpRCnEKMSAwIDAgMSAzODYuNDI5NDAgNjMzLjIyNTcwIGNtCjAgMCBtCjAgLTUuMDIwMDAgbAotMC40NDgwMCAtNS40NTEwMCBsCi01LjM1NjAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTAuNDAwMDAgbAotNC45MzkwMCAwIGwKaAotNS4wMTkwMCAtNS4xMDAwMCA0LjQyNzAwIDQuNTQ3MDAgcmUKZgpRCnEKMSAwIDAgMSA0MDIuNjU2MDAgNjMzLjIyNTcwIGNtCjAgMCBtCjAgLTUuMDIwMDAgbAotMC40NDgwMCAtNS40NTEwMCBsCi01LjM1NjAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTAuNDAwMDAgbAotNC45MzkwMCAwIGwKaAotNS4wMTkwMCAtNS4xMDAwMCA0LjQyNzAwIDQuNTQ3MDAgcmUKZgpRCnEKMSAwIDAgMSAyMTguNjEwMzAgNjE0Ljg1NDcwIGNtCjAgMCBtCjAgLTUuMDIwMDAgbAotMC40NDgwMCAtNS40NTEwMCBsCi01LjM1NjAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTAuNDAwMDAgbAotNC45MzkwMCAwIGwKaAotNS4wMTkwMCAtNS4xMDAwMCA0LjQyNzAwIDQuNTQ3MDAgcmUKZgpRCnEKMSAwIDAgMSAyMzIuOTY4NzAgNjE0Ljg1NDcwIGNtCjAgMCBtCjAgLTUuMDIwMDAgbAotMC40NDgwMCAtNS40NTEwMCBsCi01LjM1NjAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTAuNDAwMDAgbAotNC45MzkwMCAwIGwKaAotNS4wMTkwMCAtNS4xMDAwMCA0LjQyNzAwIDQuNTQ3MDAgcmUKZgpRCnEKMCBnCjIgdwo0IE0KL0dTMCBncwovRm00IERvClEKUQpCVAowLjE1OTAwIDAuMzM5MDAgMC42NDYwMCBzY24KOS41MDAwMCAwIDAgOS41MDAwMCAxNC4wMDgzMCAxMTUuODE3NDAgVG0KKENoaWxkIDIgKSBUagovVDFfMCAxIFRmCihcMDUwQWRkaXRpb25hbCBDaGlsZHJlbiBjYW4gYmUgc2hvd24gb24gYSBzZXBhcmF0ZSBzaGVldCBvZiA4XDA1NjVcMjI0IHggMTFcMjI0IHBhcGVyXDA1NlwwNTEpIFRqCjAuMDAxMDAgMC4wMDIwMCAwLjAwMjAwIHNjbgoxMCAwIDAgMTAgMTQuMDA4MzAgMTAzLjgxNzQwIFRtCihOYW1lIFwwNTBGaXJzdFwwNTQgTUlcMDU0IExhc3RcMDUxXDA3MikgVGoKOCAwIDAgOCA5MC42MTYyMCAxMDMuODE3NDAgVG0KKCApIFRqCi9UVDAgMSBUZgpbIChcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3KSAtMy4xMDAwMCAoXDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzNykgXSBUSgovVDFfMCAxIFRmCjEwIDAgMCAxMCAxNC4wMDgzMCA4NS44MTc0MCBUbQooU1NOXDA3MikgVGoKOCAwIDAgOCAzMC4xNTc3MCA4NS44MTc0MCBUbQooICApIFRqCi9UVDAgMSBUZgooXDEzN1wxMzdcMTM3XDEzN1wxMzcgXDA1NSBcMTM3XDEzN1wxMzdcMTM3XDEzNyBcMDU1ICBcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3KSBUagovVDFfMCAxIFRmCjkuNzkxMDAgMCBUZAooICAgICApIFRqCjEwIDAgMCAxMCAxMTUuNTI1NDAgODUuODE3NDAgVG0KKEdlbmRlclwwNzIgKSBUago4IDAgMCA4IDE0NS4zMTQ1MCA4NS44MTc0MCBUbQooICAgICApIFRqCjEwIDAgMCAxMCAxNTIuMzU0MDAgODUuODE3NDAgVG0KKCBNICAgICAgRikgVGoKOCAwIDAgOCAxNzQuOTUzNjAgODUuODE3NDAgVG0KKCAgICAgKSBUagoxMCAwIDAgMTAgMTgxLjk5MzcwIDg1LjgxNzQwIFRtCihCaXJ0aCBEYXRlXDA3MiAgKSBUagovVFQwIDEgVGYKOCAwIDAgOCAyMjMuODQyMzAgODUuODE3NDAgVG0KKFwxMzdcMTM3XDEzN1wxMzcgXDA1N1wxMzdcMTM3XDEzN1wxMzcgXDA1N1wxMzdcMTM3XDEzN1wxMzcpIFRqCi9UMV8wIDEgVGYKMTAgMCAwIDEwIDI3NC45MjA0MCA4NS44MTc0MCBUbQooICkgVGoKRVQKcQowIDc5MiA2MTIgLTc5MiByZQpXCm4KcQowIGcKMiB3CjQgTQovR1MwIGdzCi9GbTUgRG8KUQpRCkJUCjkgMCAwIDkgNTE3LjUzNDIwIDEwMS40Mjk3MCBUbQooXDA0NCApIFRqCi9UVDAgMSBUZgooXDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzcpIFRqCkVUCnEKMCA3OTIgNjEyIC03OTIgcmUKVwpuCi9DUzEgY3MKMSBzY24KMTAuNTIzMDAgMTk2LjA0MDAwIDQwOS45NzcwMCAxMy44MDAwMCByZQpmKgpRCkJUCjEgMSAxIHNjbgovVDFfMSAxIFRmCjkgMCAwIDkgMTQuMDA4MzAgMTk5LjkwNTgwIFRtCihTZWN0aW9uIDQgXDA1NSBDaGlsZHJlblwyMjJzIEluZm9ybWF0aW9uIFwwNTBhZ2VzIDE0IGRheXMgXDA1NSAyMyB5ZWFyc1wwNTEgKSBUagowLjE1OTAwIDAuMzM5MDAgMC42NDYwMCBzY24KL1QxXzIgMSBUZgo5LjUwMDAwIDAgMCA5LjUwMDAwIDE0LjAwODMwIDE2OS4xMTYyMCBUbQooQ2hpbGQgMSkgVGoKMC4wMDEwMCAwLjAwMjAwIDAuMDAyMDAgc2NuCi9UMV8wIDEgVGYKMTAgMCAwIDEwIDE0LjAwODMwIDE1Ny4xMTYyMCBUbQooTmFtZSBcMDUwRmlyc3RcMDU0IE1JXDA1NCBMYXN0XDA1MVwwNzIpIFRqCjggMCAwIDggOTAuNjE2MjAgMTU3LjExNjIwIFRtCiggKSBUagovVFQwIDEgVGYKWyAoXDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzNykgLTMuMTAwMDAgKFwxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzcpIF0gVEoKL1QxXzAgMSBUZgoxMCAwIDAgMTAgMTQuMDA4MzAgMTM5LjExNjIwIFRtCihTU05cMDcyKSBUago4IDAgMCA4IDMwLjE1NzcwIDEzOS4xMTYyMCBUbQooICApIFRqCi9UVDAgMSBUZgooXDEzN1wxMzdcMTM3XDEzN1wxMzcgXDA1NSBcMTM3XDEzN1wxMzdcMTM3XDEzNyBcMDU1ICBcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3KSBUagovVDFfMCAxIFRmCjkuNzkxMDAgMCBUZAooICAgICApIFRqCjEwIDAgMCAxMCAxMTUuNTI1NDAgMTM5LjExNjIwIFRtCihHZW5kZXJcMDcyICkgVGoKOCAwIDAgOCAxNDUuMzE0NTAgMTM5LjExNjIwIFRtCiggICAgICkgVGoKMTAgMCAwIDEwIDE1Mi4zNTQwMCAxMzkuMTE2MjAgVG0KKCBNICAgICAgRikgVGoKOCAwIDAgOCAxNzQuOTUzNjAgMTM5LjExNjIwIFRtCiggICAgICkgVGoKMTAgMCAwIDEwIDE4MS45OTM3MCAxMzkuMTE2MjAgVG0KKEJpcnRoIERhdGVcMDcyICApIFRqCi9UVDAgMSBUZgo4IDAgMCA4IDIyMy44NDIzMCAxMzkuMTE2MjAgVG0KKFwxMzdcMTM3XDEzN1wxMzcgXDA1N1wxMzdcMTM3XDEzN1wxMzcgXDA1N1wxMzdcMTM3XDEzN1wxMzcpIFRqCi9UMV8wIDEgVGYKMTAgMCAwIDEwIDI3NC45MjA0MCAxMzkuMTE2MjAgVG0KKCApIFRqCkVUCnEKMCA3OTIgNjEyIC03OTIgcmUKVwpuCi9DUzEgQ1MKMSBTQ04KMC43NTAwMCB3CjQgTQpxCjEgMCAwIDEgNTk3Ljk3NjYwIDEzMC44NTIyMCBjbQowIDAgbQotNTg3LjQ1MzAwIDAgbApTClEKUQpCVAo5IDAgMCA5IDQzMi42MzM4MCAxMDEuNDI5MjAgVG0KKFwwNDQgKSBUagovVFQwIDEgVGYKKFwxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3KSBUagpFVApxCjAgNzkyIDYxMiAtNzkyIHJlClcKbgowLjEzNzAwIDAuMTIzMDAgMC4xMjYwMCBzY24KL0dTMiBncwpxCjEgMCAwIDEgMTUyLjI2MTgwIDE0NS41OTQyMCBjbQowIDAgbQowIC01LjAyMDAwIGwKLTAuNDQ4MDAgLTUuNDUxMDAgbAotNS4zNTYwMCAtNS40NTEwMCBsCi01LjM1NjAwIC0wLjQwMDAwIGwKLTQuOTM5MDAgMCBsCmgKLTUuMDE5MDAgLTUuMTAwMDAgNC40MjcwMCA0LjU0NzAwIHJlCmYKUQpxCjEgMCAwIDEgMTY4LjcwMjYwIDE0NS41OTQyMCBjbQowIDAgbQowIC01LjAyMDAwIGwKLTAuNDQ4MDAgLTUuNDUxMDAgbAotNS4zNTYwMCAtNS40NTEwMCBsCi01LjM1NjAwIC0wLjQwMDAwIGwKLTQuOTM5MDAgMCBsCmgKLTUuMDE5MDAgLTUuMTAwMDAgNC40MjcwMCA0LjU0NzAwIHJlCmYKUQpxCjEgMCAwIDEgMTUyLjI2MTgwIDkyLjI5NDQwIGNtCjAgMCBtCjAgLTUuMDIwMDAgbAotMC40NDgwMCAtNS40NTEwMCBsCi01LjM1NjAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTAuNDAwMDAgbAotNC45MzkwMCAwIGwKaAotNS4wMTkwMCAtNS4xMDAwMCA0LjQyNzAwIDQuNTQ3MDAgcmUKZgpRCnEKMSAwIDAgMSAxNjguNzAyNjAgOTIuMjk0NDAgY20KMCAwIG0KMCAtNS4wMjAwMCBsCi0wLjQ0ODAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTUuNDUxMDAgbAotNS4zNTYwMCAtMC40MDAwMCBsCi00LjkzOTAwIDAgbApoCi01LjAxOTAwIC01LjEwMDAwIDQuNDI3MDAgNC41NDcwMCByZQpmClEKUQpCVAovVDFfMCAxIFRmCjEwIDAgMCAxMCAxNC4wMDgzMCAxODIuMzU1NTAgVG0KKFRoZSBlbXBsb3llZSB3aWxsIGJlIHRoZSBvd25lciBhbmQgdGhlIGJlbmVmaWNpYXJ5IHVubGVzcyBvdGhlcndpc2Ugc3RhdGVkXDA1NikgVGoKRVQKcQowIDc5MiA2MTIgLTc5MiByZQpXCm4KcQowIGcKMC43NTAwMCB3CjQgTQovR1MwIGdzCi9GbTYgRG8KUQpxCjAgZwowLjc1MDAwIHcKNCBNCi9HUzAgZ3MKL0ZtNyBEbwpRCi9DUzEgY3MKMSBzY24KNTEzLjQxNzAwIDE5Ni4wNDAwMCA4NS4zNDcwMCAxMy44MDAwMCByZQpmKgo0MjQuNTAwMDAgMTk2LjA0MDAwIDg1LjM0NzAwIDEzLjgwMDAwIHJlCmYqClEKQlQKMSAxIDEgc2NuCi9UMV8yIDEgVGYKOS41MDAwMCAwIDAgOS41MDAwMCA0MzMuODUyNTAgMTk5LjkwNTgwIFRtCihDb3ZlcmFnZSBBbW91bnQpIFRqCjExLjEwMjAwIDAgVGQKKFByZW1pdW0pIFRqCjAuMDAxMDAgMC4wMDIwMCAwLjAwMjAwIHNjbgovVDFfMCAxIFRmCjkgMCAwIDkgNTI0LjY4MjEwIDE3Ny4xMTYyMCBUbQooXDA0NCApIFRqCi9UVDAgMSBUZgooXDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzcpIFRqCi9UMV8wIDEgVGYKMTAgMCAwIDEwIDQzNi4wNzcxMCAxNzcuMTE2MjAgVG0KKFwwNDQpIFRqCjkgMCAwIDkgNDQwLjIyNzUwIDE3Ny4xMTYyMCBUbQooICkgVGoKL1RUMCAxIFRmCihcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzNykgVGoKRVQKcQowIDc5MiA2MTIgLTc5MiByZQpXCm4KL0NTMSBDUwoxIFNDTgowLjc1MDAwIHcKNCBNCnEKMSAwIDAgMSA1OTcuNzczNDAgNTExLjIwMTAwIGNtCjAgMCBtCi01ODYuNjU2MDAgMCBsClMKUQpRCkJUCjAuMTU5MDAgMC4zMzkwMCAwLjY0NjAwIHNjbgovVDFfMiAxIFRmCjkuNTAwMDAgMCAwIDkuNTAwMDAgMTQuMDA4MzAgNDk4Ljk3MjIwIFRtCihCZW5lZmljaWFyeSkgVGoKMC4wMDEwMCAwLjAwMjAwIDAuMDAyMDAgc2NuCi9UMV8wIDEgVGYKMTAgMCAwIDEwIDE0LjAwODMwIDQ4NS45NzIyMCBUbQooUCkgVGoKMC40MjMwMCAwIFRkCihyaW1hcnlcMDcyKSBUago4IDAgMCA4IDQzLjQ5NzYwIDQ4NS45NzIyMCBUbQooICkgVGoKL1RUMCAxIFRmCihcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzcpIFRqCi9UMV8wIDEgVGYKMjYuMTcxMDAgMCBUZAooICAgICkgVGoKMTAgMCAwIDEwIDI1OC40OTg1MCA0ODUuOTcyMjAgVG0KKFJlbGF0aW9uc2hpcFwwNzIpIFRqCjggMCAwIDggMzA0LjczNjgwIDQ4NS45NzIyMCBUbQooICkgVGoKL1RUMCAxIFRmCiggXDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3KSBUagovVDFfMCAxIFRmCjUuODc3MDAgMCBUZAooICAgICkgVGoKMTAgMCAwIDEwIDM1Ny4zODIzMCA0ODUuOTcyMjAgVG0KKEEpIFRqCjAuNDcyMDAgMCBUZAooZ2VcMDcyKSBUago4IDAgMCA4IDM3My4wMjIwMCA0ODUuOTcyMjAgVG0KKCAgICkgVGoKL1RUMCAxIFRmCihcMTM3XDEzN1wxMzdcMTM3XDEzNykgVGoKL1QxXzAgMSBUZgooICAgICkgVGoKMTAgMCAwIDEwIDQwMS4xMTk2MCA0ODUuOTcyMjAgVG0KKEJpcnRoIERhdGVcMDcyKSBUago4IDAgMCA4IDQzOS40NDg3MCA0ODUuOTcyMjAgVG0KKCAgKSBUagovVFQwIDEgVGYKKFwxMzdcMTM3XDEzN1wxMzcgXDA1N1wxMzdcMTM3XDEzN1wxMzcgXDA1N1wxMzdcMTM3XDEzN1wxMzcpIFRqCi9UMV8wIDEgVGYKNi43MzcwMCAwIFRkCiggICAgKSBUagoxMCAwIDAgMTAgNDk4Ljk3NTEwIDQ4NS45NzIyMCBUbQooU1NOXDA3MiApIFRqCjggMCAwIDggNTE2Ljg4NDMwIDQ4NS45NzIyMCBUbQooICkgVGoKL1RUMCAxIFRmCihcMTM3XDEzN1wxMzdcMTM3XDEzNyBcMDU1IFwxMzdcMTM3XDEzN1wxMzdcMTM3IFwwNTUgIFwxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzcgKSBUagovVDFfMCAxIFRmCjEwIDAgMCAxMCAxNC4wMDgzMCA0NjcuOTcyMjAgVG0KKENvbnRpbmdlbnRcMDcyKSBUago4IDAgMCA4IDU2LjE3NzIwIDQ2Ny45NzIyMCBUbQooICkgVGoKKCApIFRqCi9UVDAgMSBUZgooXDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzcpIFRqCi9UMV8wIDEgVGYKMjQuNTIzMDAgMCBUZAooICAgICkgVGoKMTAgMCAwIDEwIDI1Ny45OTIyMCA0NjcuOTcyMjAgVG0KKFJlbGF0aW9uc2hpcFwwNzIpIFRqCjggMCAwIDggMzA0LjIzMTAwIDQ2Ny45NzIyMCBUbQooICkgVGoKL1RUMCAxIFRmCiggXDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3KSBUagovVDFfMCAxIFRmCjUuODc3MDAgMCBUZAooICAgICkgVGoKMTAgMCAwIDEwIDM1Ni44NzY1MCA0NjcuOTcyMjAgVG0KKEEpIFRqCjAuNDcyMDAgMCBUZAooZ2VcMDcyKSBUago4IDAgMCA4IDM3Mi41MTYxMCA0NjcuOTcyMjAgVG0KKCAgICkgVGoKL1RUMCAxIFRmCihcMTM3XDEzN1wxMzdcMTM3XDEzNykgVGoKL1QxXzAgMSBUZgooICAgICkgVGoKMTAgMCAwIDEwIDQwMC42MTM4MCA0NjcuOTcyMjAgVG0KKEJpcnRoIERhdGVcMDcyKSBUago4IDAgMCA4IDQzOC45NDI5MCA0NjcuOTcyMjAgVG0KKCAgKSBUagovVFQwIDEgVGYKKFwxMzdcMTM3XDEzN1wxMzcgXDA1N1wxMzdcMTM3XDEzN1wxMzcgXDA1N1wxMzdcMTM3XDEzN1wxMzcpIFRqCi9UMV8wIDEgVGYKNi43MzcwMCAwIFRkCiggICAgKSBUagoxMCAwIDAgMTAgNDk4LjQ2ODMwIDQ2Ny45NzIyMCBUbQooU1NOXDA3MiApIFRqCjggMCAwIDggNTE2LjM3NzQwIDQ2Ny45NzIyMCBUbQooICkgVGoKL1RUMCAxIFRmCihcMTM3XDEzN1wxMzdcMTM3XDEzNyBcMDU1IFwxMzdcMTM3XDEzN1wxMzdcMTM3IFwwNTUgIFwxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzcgKSBUagpFVApxCjAgNzkyIDYxMiAtNzkyIHJlClcKbgovQ1MxIGNzCjEgc2NuCjExLjAyMzAwIDQ0NS4wMzAwMCA0MDkuOTc3MDAgMTMuODAwMDAgcmUKZioKUQpCVAoxIDEgMSBzY24KL1QxXzIgMSBUZgo5LjUwMDAwIDAgMCA5LjUwMDAwIDE0LjAwODMwIDQ0OC44OTcwMCBUbQooU2VjdGlvbiAzIFwwNTUgKSBUagooU3BvdXNlKSBUagowLjAwMTAwIDAuMDAyMDAgMC4wMDIwMCBzY24KL1QxXzAgMSBUZgoxMCAwIDAgMTAgMTMuNjAwMTAgNDIwLjEwNjkwIFRtCihTcG91c2VcMjIycyBOYW1lXDA3MikgVGoKOCAwIDAgOCA3MC41ODg5MCA0MjAuMTA2OTAgVG0KKCApIFRqCi9UVDAgMSBUZgooXDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzcpIFRqCi9UMV8wIDEgVGYKMTAgMCAwIDEwIDIzMi41MjgzMCA0MjAuMTA2OTAgVG0KKCAgICAgU1NOXDA3MikgVGoKOCAwIDAgOCAyNTcuNDc3NTAgNDIwLjEwNjkwIFRtCiggICkgVGoKL1RUMCAxIFRmCihcMTM3XDEzN1wxMzdcMTM3XDEzNyBcMDU1IFwxMzdcMTM3XDEzN1wxMzdcMTM3IFwwNTUgIFwxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzcpIFRqCi9UMV8wIDEgVGYKOS43OTEwMCAwIFRkCiggICAgICkgVGoKL1RUMCAxIFRmCiggICkgVGoKL1QxXzAgMSBUZgooICAgICkgVGoKMTAgMCAwIDEwIDEzLjYwMDEwIDQwMi4xMDY5MCBUbQooR2VuZGVyXDA3MiApIFRqCjggMCAwIDggNDMuMzg5MjAgNDAyLjEwNjkwIFRtCiggICAgICkgVGoKMTAgMCAwIDEwIDUwLjQyOTIwIDQwMi4xMDY5MCBUbQooIE0gICAgICBGICAgICBCaXJ0aCBEYXRlXDA3MiAgKSBUagovVFQwIDEgVGYKOCAwIDAgOCAxMjMuNjc2ODAgNDAyLjEwNjkwIFRtCihcMTM3XDEzN1wxMzdcMTM3IFwwNTdcMTM3XDEzN1wxMzdcMTM3IFwwNTdcMTM3XDEzN1wxMzdcMTM3KSBUagovVDFfMCAxIFRmCjEwIDAgMCAxMCAxMy42MDAxMCAzODUuMTA2OTAgVG0KKER1cmluZyB0aGUgcHJpb3IgNiBtb250aHNcMDU0IG90aGVyIHRoYW4gZm9yIHJvdXRpbmUgbWVkaWNhbCBjYXJlXDA1NCBoYXMgeW91ciBzcG91c2UgYmVlbiBkaWFnbm9zZWQgb3IgdHJlYXRlZCBieSApIFRqClQqCihhIG1lbWJlciBvZiB0aGUgbWVkaWNhbCBwcm9mZXNzaW9uIGluIGEgaG9zcGl0YWwgb3IgYW55IG90aGVyIG1lZGljYWwgZmFjaWxpdHlcMDc3KSBUagooICAgICAgWSAgICAgIE4pIFRqCiggKSBUagooICApIFRqCjAgLTEuMjAwMDAgVEQKKFwwNTBJZiB5ZXNcMDU0IGNvbXBsZXRlIHRoZSBxdWVzdGlvbnMgaW4gU2VjdGlvbiA2XDA1MSAgICkgVGoKKCAgKSBUagowIC0xLjcwMDAwIFRECihIYXMgeW91ciBzcG91c2UgIGJlZW4gZGlzYWJsZWRcMDUyXDA1MiBpbiB0aGUgIHByaW9yIDYgbW9udGhzIG9yIHJlY2VpdmVkIGRpc2FiaWxpdHkgcGF5bWVudHNcMDc3ICAgICAgIFkgICAgICBOKSBUagowIC0xLjgwMDAwIFRECihNYWlsaW5nIEFkZHJlc3NcMDcyKSBUago4IDAgMCA4IDcyLjIwOTAwIDMyNS4xMDY5MCBUbQooICApIFRqCi9UVDAgMSBUZgooXDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzNykgVGoKL1QxXzAgMSBUZgozNS4wMTIwMCAwIFRkCiggKSBUagoxMCAwIDAgMTAgMTMuNjAwMTAgMzA3LjEwNjkwIFRtCihDaXR5XDA3MiApIFRqCi9UVDAgMSBUZgo4IDAgMCA4IDMxLjI5OTgwIDMwNy4xMDY5MCBUbQooXDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzcpIFRqCi9UMV8wIDEgVGYKMTAgMCAwIDEwIDE5MS44MzExMCAzMDcuMTA2OTAgVG0KKCAgICAgIFN0YXRlXDA3MiApIFRqCi9UVDAgMSBUZgo4IDAgMCA4IDIyNS43OTk4MCAzMDcuMTA2OTAgVG0KKFwxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzcpIFRqCjEwIDAgMCAxMCAyNTEuMzM4OTAgMzA3LjEwNjkwIFRtCiggKSBUagovVDFfMCAxIFRmCiggICAgICBaaXAgQ29kZVwwNzIgICkgVGoKL1RUMCAxIFRmCjggMCAwIDggMzAwLjk0NzgwIDMwNy4xMDY5MCBUbQooXDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzcgICkgVGoKL1QxXzAgMSBUZgoxMCAwIDAgMTAgMTMuNjAwMTAgMjg5LjEwNjkwIFRtCihFbWFpbCBBZGRyZXNzXDA3MiApIFRqCi9UVDAgMSBUZgo4IDAgMCA4IDY3LjgwOTEwIDI4OS4xMDY5MCBUbQooXDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzcpIFRqCjEwIDAgMCAxMCAyMzkuMjg1NjAgMjg5LjEwNjkwIFRtCiggXDEwMCApIFRqCjggMCAwIDggMjUyLjE2NjUwIDI4OS4xMDY5MCBUbQooXDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzcpIFRqCkVUCnEKMCA3OTIgNjEyIC03OTIgcmUKVwpuCi9DUzEgQ1MKMSBTQ04KMC43NTAwMCB3CjQgTQpxCjEgMCAwIDEgNTk3Ljc3MzQwIDI2MC41MDU2MCBjbQowIDAgbQotNTg2Ljc1MDAwIDAgbApTClEKUQpCVAowLjE1OTAwIDAuMzM5MDAgMC42NDYwMCBzY24KL1QxXzIgMSBUZgo5LjUwMDAwIDAgMCA5LjUwMDAwIDE0LjAwODMwIDI0OC4yNzY5MCBUbQooQmVuZWZpY2lhcnkpIFRqCjAuMDAxMDAgMC4wMDIwMCAwLjAwMjAwIHNjbgovVDFfMCAxIFRmCjEwIDAgMCAxMCAxNC4wMDgzMCAyMzUuMjc2OTAgVG0KKFApIFRqCjAuNDIzMDAgMCBUZAoocmltYXJ5XDA3MikgVGoKOCAwIDAgOCA0My40OTc2MCAyMzUuMjc2OTAgVG0KKCApIFRqCi9UVDAgMSBUZgooXDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3KSBUagovVDFfMCAxIFRmCjI2LjE3MTAwIDAgVGQKKCAgICApIFRqCjEwIDAgMCAxMCAyNTguNDk4NTAgMjM1LjI3NjkwIFRtCihSZWxhdGlvbnNoaXBcMDcyKSBUago4IDAgMCA4IDMwNC43MzY4MCAyMzUuMjc2OTAgVG0KKCApIFRqCi9UVDAgMSBUZgooIFwxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzNykgVGoKL1QxXzAgMSBUZgo1Ljg3NzAwIDAgVGQKKCAgICApIFRqCjEwIDAgMCAxMCAzNTcuMzgyMzAgMjM1LjI3NjkwIFRtCihBKSBUagowLjQ3MjAwIDAgVGQKKGdlXDA3MikgVGoKOCAwIDAgOCAzNzMuMDIyMDAgMjM1LjI3NjkwIFRtCiggICApIFRqCi9UVDAgMSBUZgooXDEzN1wxMzdcMTM3XDEzN1wxMzcpIFRqCi9UMV8wIDEgVGYKKCAgICApIFRqCjEwIDAgMCAxMCA0MDEuMTE5NjAgMjM1LjI3NjkwIFRtCihCaXJ0aCBEYXRlXDA3MikgVGoKOCAwIDAgOCA0MzkuNDQ4NzAgMjM1LjI3NjkwIFRtCiggICkgVGoKL1RUMCAxIFRmCihcMTM3XDEzN1wxMzdcMTM3IFwwNTdcMTM3XDEzN1wxMzdcMTM3IFwwNTdcMTM3XDEzN1wxMzdcMTM3KSBUagovVDFfMCAxIFRmCjYuNzM3MDAgMCBUZAooICAgICkgVGoKMTAgMCAwIDEwIDQ5OC45NzUxMCAyMzUuMjc2OTAgVG0KKFNTTlwwNzIgKSBUago4IDAgMCA4IDUxNi44ODQzMCAyMzUuMjc2OTAgVG0KKCApIFRqCi9UVDAgMSBUZgooXDEzN1wxMzdcMTM3XDEzN1wxMzcgXDA1NSBcMTM3XDEzN1wxMzdcMTM3XDEzNyBcMDU1ICBcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3ICkgVGoKL1QxXzAgMSBUZgoxMCAwIDAgMTAgMTQuMDA4MzAgMjE3LjI3NjkwIFRtCihDb250aW5nZW50XDA3MikgVGoKOCAwIDAgOCA1Ni4xNzcyMCAyMTcuMjc2OTAgVG0KKCApIFRqCiggKSBUagovVFQwIDEgVGYKKFwxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3KSBUagovVDFfMCAxIFRmCjI0LjUyMzAwIDAgVGQKKCAgICApIFRqCjEwIDAgMCAxMCAyNTcuOTkyMjAgMjE3LjI3NjkwIFRtCihSZWxhdGlvbnNoaXBcMDcyKSBUago4IDAgMCA4IDMwNC4yMzEwMCAyMTcuMjc2OTAgVG0KKCApIFRqCi9UVDAgMSBUZgooIFwxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzNykgVGoKL1QxXzAgMSBUZgo1Ljg3NzAwIDAgVGQKKCAgICApIFRqCjEwIDAgMCAxMCAzNTYuODc2NTAgMjE3LjI3NjkwIFRtCihBKSBUagowLjQ3MjAwIDAgVGQKKGdlXDA3MikgVGoKOCAwIDAgOCAzNzIuNTE2MTAgMjE3LjI3NjkwIFRtCiggICApIFRqCi9UVDAgMSBUZgooXDEzN1wxMzdcMTM3XDEzN1wxMzcpIFRqCi9UMV8wIDEgVGYKKCAgICApIFRqCjEwIDAgMCAxMCA0MDAuNjEzODAgMjE3LjI3NjkwIFRtCihCaXJ0aCBEYXRlXDA3MikgVGoKOCAwIDAgOCA0MzguOTQyOTAgMjE3LjI3NjkwIFRtCiggICkgVGoKL1RUMCAxIFRmCihcMTM3XDEzN1wxMzdcMTM3IFwwNTdcMTM3XDEzN1wxMzdcMTM3IFwwNTdcMTM3XDEzN1wxMzdcMTM3KSBUagovVDFfMCAxIFRmCjYuNzM3MDAgMCBUZAooICAgICkgVGoKMTAgMCAwIDEwIDQ5OC40NjgzMCAyMTcuMjc2OTAgVG0KKFNTTlwwNzIgKSBUago4IDAgMCA4IDUxNi4zNzc0MCAyMTcuMjc2OTAgVG0KKCApIFRqCi9UVDAgMSBUZgooXDEzN1wxMzdcMTM3XDEzN1wxMzcgXDA1NSBcMTM3XDEzN1wxMzdcMTM3XDEzNyBcMDU1ICBcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3ICkgVGoKRVQKcQowIDc5MiA2MTIgLTc5MiByZQpXCm4KMC4xMzcwMCAwLjEyMzAwIDAuMTI2MDAgc2NuCi9HUzIgZ3MKcQoxIDAgMCAxIDUxLjE3OTMwIDQwOC41NDMxMCBjbQowIDAgbQowIC01LjAyMDAwIGwKLTAuNDQ4MDAgLTUuNDUxMDAgbAotNS4zNTYwMCAtNS40NTEwMCBsCi01LjM1NjAwIC0wLjQwMDAwIGwKLTQuOTM5MDAgMCBsCmgKLTUuMDE5MDAgLTUuMTAwMDAgNC40MjcwMCA0LjU0NzAwIHJlCmYKUQpxCjEgMCAwIDEgNjcuNTM0OTAgNDA4LjU0MzEwIGNtCjAgMCBtCjAgLTUuMDIwMDAgbAotMC40NDgwMCAtNS40NTEwMCBsCi01LjM1NjAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTAuNDAwMDAgbAotNC45MzkwMCAwIGwKaAotNS4wMTkwMCAtNS4xMDAwMCA0LjQyNzAwIDQuNTQ3MDAgcmUKZgpRCnEKMSAwIDAgMSAzMDUuMjYyMDAgMzc4LjQzMDAwIGNtCjAgMCBtCjAgLTUuMDIwMDAgbAotMC40NDgwMCAtNS40NTEwMCBsCi01LjM1NjAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTAuNDAwMDAgbAotNC45MzkwMCAwIGwKaAotNS4wMTkwMCAtNS4xMDAwMCA0LjQyNzAwIDQuNTQ3MDAgcmUKZgpRCnEKMSAwIDAgMSAzMTkuODg1MjAgMzc4LjQzMDAwIGNtCjAgMCBtCjAgLTUuMDIwMDAgbAotMC40NDgwMCAtNS40NTEwMCBsCi01LjM1NjAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTAuNDAwMDAgbAotNC45MzkwMCAwIGwKaAotNS4wMTkwMCAtNS4xMDAwMCA0LjQyNzAwIDQuNTQ3MDAgcmUKZgpRCnEKMCBnCjAuNzUwMDAgdwo0IE0KL0dTMCBncwovRm04IERvClEKcQowIGcKMC43NTAwMCB3CjQgTQovR1MwIGdzCi9GbTkgRG8KUQovQ1MxIGNzCjEgc2NuCi9HUzEgZ3MKNTEzLjg5NDAwIDQ0NS40MDcwMCA4NS4zNDcwMCAxMy44MDAwMCByZQpmKgo0MjQuOTc4MDAgNDQ1LjQwNzAwIDg1LjM0NzAwIDEzLjgwMDAwIHJlCmYqClEKQlQKMSAxIDEgc2NuCi9UMV8yIDEgVGYKOS41MDAwMCAwIDAgOS41MDAwMCA0MzQuMzMwMTAgNDQ5LjI3MzkwIFRtCihDb3ZlcmFnZSBBbW91bnQpIFRqCjExLjEwMjAwIDAgVGQKKFByZW1pdW0pIFRqCjAuMDAxMDAgMC4wMDIwMCAwLjAwMjAwIHNjbgovVDFfMCAxIFRmCjkgMCAwIDkgNTI1LjE1OTcwIDQyMi40ODQ0MCBUbQooXDA0NCApIFRqCi9UVDAgMSBUZgooXDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzcpIFRqCi9UMV8wIDEgVGYKMTAgMCAwIDEwIDQzNi41NTQ3MCA0MjIuNDgzOTAgVG0KKFwwNDQpIFRqCjkgMCAwIDkgNDQwLjcwNTEwIDQyMi40ODM5MCBUbQooICkgVGoKL1RUMCAxIFRmCihcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzNykgVGoKL1QxXzAgMSBUZgoxMCAwIDAgMTAgNDMyLjc2MjcwIDM3Mi4yNzkzMCBUbQooRGlzYWJpbGl0eSBXYWl2ZXIgb2YgUHJlbWl1bSBcMDUwV1BcMDUxKSBUagowIC0xLjMwMDAwIFRECihBdXRvIEluY3JlYXNlIFJpZGVyIFwwNTBBSVJcMDUxICkgVGoKLTAuMDA1MDAgVGMKVCoKKENocm9uaWMgSWxsbmVzcyBSaWRlciBcMDUwQ0hSXDA1MSApIFRqCi9UMV8zIDEgVGYKLTAuMDEwMDAgVGMKKFwwNTBGUFBcMDU1VEkgb25seVwwNTEpIFRqCi9UMV8wIDEgVGYKMCBUYwpUKgooT3RoZXJcMDcyICkgVGoKKCApIFRqCi9UVDAgMSBUZgooXDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3KSBUagovVDFfMCAxIFRmCjEzLjQ5OTAwIDAgVGQKKCApIFRqCkVUCnEKMCA3OTIgNjEyIC03OTIgcmUKVwpuCjAuMTM3MDAgMC4xMjMwMCAwLjEyNjAwIHNjbgovR1MyIGdzCnEKMSAwIDAgMSA0MzAuNDg2MDAgMzc4LjQyMDgwIGNtCjAgMCBtCjAgLTUuMDIwMDAgbAotMC40NDgwMCAtNS40NTEwMCBsCi01LjM1NjAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTAuNDAwMDAgbAotNC45MzkwMCAwIGwKaAotNS4wMTkwMCAtNS4xMDAwMCA0LjQyNzAwIDQuNTQ3MDAgcmUKZgpRCnEKMSAwIDAgMSA0MzAuNDg2MDAgMzM5LjY2NTEwIGNtCjAgMCBtCjAgLTUuMDIwMDAgbAotMC40NDgwMCAtNS40NTEwMCBsCi01LjM1NjAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTAuNDAwMDAgbAotNC45MzkwMCAwIGwKaAotNS4wMTkwMCAtNS4xMDAwMCA0LjQyNzAwIDQuNTQ3MDAgcmUKZgpRCnEKMSAwIDAgMSA0MzAuNDg2MDAgMzY1Ljg4NDIwIGNtCjAgMCBtCjAgLTUuMDIwMDAgbAotMC40NDgwMCAtNS40NTEwMCBsCi01LjM1NjAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTAuNDAwMDAgbAotNC45MzkwMCAwIGwKaAotNS4wMTkwMCAtNS4xMDAwMCA0LjQyNzAwIDQuNTQ3MDAgcmUKZgpRCnEKMSAwIDAgMSA0MzAuNDg2MDAgMzUxLjkzNzkwIGNtCjAgMCBtCjAgLTUuMDIwMDAgbAotMC40NDgwMCAtNS40NTEwMCBsCi01LjM1NjAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTAuNDAwMDAgbAotNC45MzkwMCAwIGwKaAotNS4wMTkwMCAtNS4xMDAwMCA0LjQyNzAwIDQuNTQ3MDAgcmUKZgpRClEKQlQKL1QxXzIgMSBUZgoxMCAwIDAgMTAgNDI1LjEzMjgwIDM4Mi45NzcxMCBUbQooUmlkZXJzIHRvIGJlIGFkZGVkKSBUagovVDFfMCAxIFRmCi00MS4xNTMwMCA0Ljk4OTAwIFRkCihUaGUgZW1wbG95ZWUgd2lsbCBiZSB0aGUgb3duZXIgdW5sZXNzIG90aGVyd2lzZSBzdGF0ZWRcMDU2KSBUagpFVApxCjAgNzkyIDYxMiAtNzkyIHJlClcKbgowLjEzNzAwIDAuMTIzMDAgMC4xMjYwMCBzY24KL0dTMiBncwpxCjEgMCAwIDEgMzQzLjY0MDQwIDM1MC40OTc3MCBjbQowIDAgbQowIC01LjAyMDAwIGwKLTAuNDQ4MDAgLTUuNDUxMDAgbAotNS4zNTYwMCAtNS40NTEwMCBsCi01LjM1NjAwIC0wLjQwMDAwIGwKLTQuOTM5MDAgMCBsCmgKLTUuMDE5MDAgLTUuMTAwMDAgNC40MjcwMCA0LjU0NzAwIHJlCmYKUQpxCjEgMCAwIDEgMzU4LjI2MzUwIDM1MC40OTc3MCBjbQowIDAgbQowIC01LjAyMDAwIGwKLTAuNDQ4MDAgLTUuNDUxMDAgbAotNS4zNTYwMCAtNS40NTEwMCBsCi01LjM1NjAwIC0wLjQwMDAwIGwKLTQuOTM5MDAgMCBsCmgKLTUuMDE5MDAgLTUuMTAwMDAgNC40MjcwMCA0LjU0NzAwIHJlCmYKUQpRCkJUCi9UMV80IDEgVGYKNyAwIDAgNyAxOS45NTAyMCA4LjQ3MDcwIFRtCihJQ0MxNCBGUFBcMDU1QXBwIFIxMTE0KSBUago3OC4yMzIwMCAwIFRkCiggICAgIDFcMDU3MTUpIFRqCi0zOS4wNzMwMCAwIFRkCihQYWdlIDEpIFRqCi9UMV8wIDEgVGYKOSAwIDAgOSAxMi4yMTczMCA1MzYuMzYyMzAgVG0KWyAoXDA1MiApIDguODAwMDAgKFwyMjRBY3RpdmVseSBhdCBXb3JrXDIyNCBtZWFucyB0aGF0IHlvdSBhcmUgYW4gZWxpZ2libGUgZW1wbG95ZWVcMDU3bWVtYmVyIG9mIHRoZSBlbXBsb3llclwwNTdhZmZpbGlhdGlvbiB0aHJvdWdoIHdoaWNoIHlvdSBhcmUgYXBwbHlpbmcgZm8pIChyIHRoaXMgaW5kaXZpZHVhbCBpbnN1cmFuY2VcMDczIHlvdSBhcmUgYWJsZSB0byAgKSAtMTU2MS4yMDAwMCAoICkgXSBUSgowIC0xLjExMTAwIFREClsgKCApIC0zMjEuMjAwMDAgKHdvcmsgYW5kIHRvIHBlcmZvcm0gdGhlIG5vcm1hbCBhY3Rpdml0aWVzIG9mIGEgcGVyc29uIG9mIGxpa2UgYWdlIGFuZCBnZW5kZXJcMDczIGFuZCB5b3UgYXJlIG5vdCBjb25maW5lZCBpbiBhIGhvc3BpdGFsXDA1NCBhdCBob21lIG8pIChyIGVsc2V3aGVyZSBkdWUgdG8gaW5qdXJ5IG9yIHNpY2tuZXNzIG9uIHRoZSBkYXRlICApIC0zNzMuMzAwMDAgKCApIF0gVEoKVCoKWyAoICkgLTMyMS4yMDAwMCAoeW91IHNpZ25lZCB0aGlzIGFwcGxpY2F0aW9uXDA1NikgXSBUSgowIC0yNi41OTEwMCBURApbIChcMDUyXDA1MiApIC0xNTguNjAwMDAgKFwyMjREaXNhYmxlZFwyMjQgbWVhbnMgdGhhdCBhIHBlcnNvbiBpcyB1bmFibGUgdG8gd29ya1wwNTQgdG8gYXR0ZW5kIHNjaG9vbFwwNTQgb3IgdG8gcGVyZm9ybSB0aGUgbm9ybWFsIGFjdGl2aXRpZXMgb2YgYSBwZXJzb24gb2YgbGlrZSBhZ2UpICggYW5kIGdlbmRlciBvciB0aGF0IGEgcGVyc29uIGlzIGNvbmZpbmVkIGluIGEpIF0gVEoKMCAtMS4xMTEwMCBURApbICggKSAtODE4LjUwMDAwIChob3NwaXRhbFwwNTQgYXQgaG9tZSBvciBlbHNld2hlcmUgZHVlIHRvIGluanVyeSBvciBzaWNrbmVzc1wwNTYpIF0gVEoKRVQKUQpxCjEgMCAwIDEgMCAwIGNtCkJUCi9GMSAxMiBUZgoxNC40MDAwMCBUTApFVAowIDAgMCBSRwowIDAgMCByZwpCVAovRjEgMTIgVGYKMTQuNDAwMDAgVEwKRVQKQlQKMSAwIDAgMSA0OTQgNjY5LjA1MDgwIFRtCi9GMiswIDEwIFRmCjEyIFRMCigxMjMpIFRqClQqCkVUCkJUCjEgMCAwIDEgODkgNjI5Ljg0NTcwIFRtCi9GMiswIDkgVGYKMTAuODAwMDAgVEwKKEpvZSBTbWl0aCkgVGoKVCoKRVQKQlQKMSAwIDAgMSA2MCA2MTAuODQ1NzAgVG0KL0YyKzAgOSBUZgoxMC44MDAwMCBUTAooMTk3OVwwNTUwMVwwNTUwMlQwMFwwNzIwMFwwNzIwMCkgVGoKVCoKRVQKQlQKMSAwIDAgMSAyNzIgNjI5Ljg0NTcwIFRtCi9GMiswIDkgVGYKMTAuODAwMDAgVEwKKDEyM1wwNTUxMlwwNTUxMjM0KSBUagpUKgpFVApCVAoxIDAgMCAxIDgwIDU5My44NDU3MCBUbQovRjIrMCA5IFRmCjEwLjgwMDAwIFRMCigxMjAgU2VzYW1lKSBUagpUKgpFVApCVAoxIDAgMCAxIDM2IDU3Mi44NDU3MCBUbQovRjIrMCA5IFRmCjEwLjgwMDAwIFRMCihDaGljYWdvKSBUagpUKgpFVApCVAoxIDAgMCAxIDIzMiA1NzIuODQ1NzAgVG0KL0YyKzAgOSBUZgoxMC44MDAwMCBUTAooSUwpIFRqClQqCkVUCkJUCjEgMCAwIDEgMzA2IDU3NS44NDU3MCBUbQovRjIrMCA5IFRmCjEwLjgwMDAwIFRMCig0NDM0MykgVGoKVCoKRVQKQlQKMSAwIDAgMSA3NiA1NTYuODQ1NzAgVG0KL0YyKzAgOSBUZgoxMC44MDAwMCBUTAooam9oblwxMDBzbWl0aFwwNTZjb20pIFRqClQqCkVUCkJUCjEgMCAwIDEgMzYxIDU3MC4wNTA4MCBUbQovRjIrMCAxMCBUZgoxMiBUTAooWCkgVGoKVCoKRVQKQlQKMSAwIDAgMSAzMDYgNjExLjg0NTcwIFRtCi9GMiswIDkgVGYKMTAuODAwMDAgVEwKKDE5OTlcMDU1MDFcMDU1MjRUMDBcMDcyMDBcMDcyMDApIFRqClQqCkVUCkJUCjEgMCAwIDEgNDg2IDc1Ny40NjA5MCBUbQovRjIrMCAxMiBUZgoxNC40MDAwMCBUTAooNTUxMTcpIFRqClQqCkVUCkJUCjEgMCAwIDEgMTA3IDY2OS44NDU3MCBUbQovRjIrMCA5IFRmCjEwLjgwMDAwIFRMCihBbm90aGVyIFRlc3QgQ2FzZSkgVGoKVCoKRVQKcQowLjU0NTEwIDAgMCByZwpCVAovRjMgMTAgVGYKMTIgVEwKRVQKQlQKMSAwIDAgMSAyNjQgNDAyIFRtClQqCkVUClEKQlQKMSAwIDAgMSAyMTIgNjEwLjA1MDgwIFRtCi9GMiswIDEwIFRmCjEyIFRMCihYKSBUagpUKgpFVApCVAoxIDAgMCAxIDc5IDQyMS44NDU3MCBUbQovRjIrMCA5IFRmCjEwLjgwMDAwIFRMCihKb2FubmUgU21pdGgpIFRqClQqCkVUCkJUCjEgMCAwIDEgMTI3IDQwMi44NDU3MCBUbQovRjIrMCA5IFRmCjEwLjgwMDAwIFRMCigxOTkwXDA1NTEyXDA1NTEyVDAwXDA3MjAwXDA3MjAwKSBUagpUKgpFVApCVAoxIDAgMCAxIDI2NiA0MjEuODQ1NzAgVG0KL0YyKzAgOSBUZgoxMC44MDAwMCBUTAooNzc3XDA1NTY2XDA1NTk5OTkpIFRqClQqCkVUCkJUCjEgMCAwIDEgODAgMzI3Ljg0NTcwIFRtCi9GMiswIDkgVGYKMTAuODAwMDAgVEwKKDEyMyBTZXNhbWUpIFRqClQqCkVUCkJUCjEgMCAwIDEgMzYgMzA2Ljg0NTcwIFRtCi9GMiswIDkgVGYKMTAuODAwMDAgVEwKKENoaWNhZ28pIFRqClQqCkVUCkJUCjEgMCAwIDEgMjMyIDMwNi44NDU3MCBUbQovRjIrMCA5IFRmCjEwLjgwMDAwIFRMCihJTCkgVGoKVCoKRVQKQlQKMSAwIDAgMSAzMDYgMzA4Ljg0NTcwIFRtCi9GMiswIDkgVGYKMTAuODAwMDAgVEwKKDQ0MzQzKSBUagpUKgpFVApCVAoxIDAgMCAxIDc2IDI4OS44NDU3MCBUbQovRjIrMCA5IFRmCjEwLjgwMDAwIFRMCihqYW5lXDEwMHNtaXRoXDA1NmNvbSkgVGoKVCoKRVQKQlQKMSAwIDAgMSAzMTUgMzczLjA1MDgwIFRtCi9GMiswIDEwIFRmCjEyIFRMCihYKSBUagpUKgpFVApCVAoxIDAgMCAxIDM1MyAzNDUuMDUwODAgVG0KL0YyKzAgMTAgVGYKMTIgVEwKKFgpIFRqClQqCkVUCkJUCjEgMCAwIDEgNDQ4IDYyNy44NDU3MCBUbQovRjIrMCA5IFRmCjEwLjgwMDAwIFRMCigxMFwwNTQwMDApIFRqClQqCkVUCkJUCjEgMCAwIDEgNTM2IDYyNy44NDU3MCBUbQovRjIrMCA5IFRmCjEwLjgwMDAwIFRMCigxXDA1NjIzKSBUagpUKgpFVApCVAoxIDAgMCAxIDQ1MCA0MjQuODQ1NzAgVG0KL0YyKzAgOSBUZgoxMC44MDAwMCBUTAooMTBcMDU0MDAwKSBUagpUKgpFVApCVAoxIDAgMCAxIDUzOSA0MjQuODQ1NzAgVG0KL0YyKzAgOSBUZgoxMC44MDAwMCBUTAooMlwwNTYyMykgVGoKVCoKRVQKQlQKMSAwIDAgMSA0NDcgNjEuMDUwNzggVG0KL0YyKzAgMTAgVGYKMTIgVEwKKDFcMDU2MjMpIFRqClQqCkVUCkJUCjEgMCAwIDEgNDQ3IDQ0LjA1MDc4IFRtCi9GMiswIDEwIFRmCjEyIFRMCigyXDA1NjIzKSBUagpUKgpFVApCVAoxIDAgMCAxIDQ0NyAyNi4wNTA3OCBUbQovRjIrMCAxMCBUZgoxMiBUTAooN1wwNTY1MCkgVGoKVCoKRVQKQlQKMSAwIDAgMSA1MzAgMjYuMDUwNzggVG0KL0YyKzAgMTAgVGYKMTIgVEwKKDEwXDA1Njk2KSBUagpUKgpFVApCVAoxIDAgMCAxIDQ5IDQ4OC4wNTA4MCBUbQovRjIrMCAxMCBUZgoxMiBUTAooUm9iZXJ0IFJvYnNvbikgVGoKVCoKRVQKQlQKMSAwIDAgMSAzODAgNDg4LjA1MDgwIFRtClQqCkVUCkJUCjEgMCAwIDEgMzExIDQ4OC4wNTA4MCBUbQovRjIrMCAxMCBUZgoxMiBUTAooU29uKSBUagpUKgpFVApCVAoxIDAgMCAxIDQ0MyA0ODcuODQ1NzAgVG0KL0YyKzAgOSBUZgoxMC44MDAwMCBUTAooTm9uZSkgVGoKVCoKRVQKQlQKMSAwIDAgMSA1MjEgNDg3Ljg0NTcwIFRtCi9GMiswIDkgVGYKMTAuODAwMDAgVEwKKDMzM1wwNTU1NVwwNTUzOTkwKSBUagpUKgpFVApCVAoxIDAgMCAxIDU0IDIzOS4wNTA4MCBUbQovRjIrMCAxMCBUZgoxMiBUTAooSm9obiBTbWl0aCkgVGoKVCoKRVQKQlQKMSAwIDAgMSAzODEgMjM4LjA1MDgwIFRtClQqCkVUCkJUCjEgMCAwIDEgMzE1IDIzOC4wNTA4MCBUbQovRjIrMCAxMCBUZgoxMiBUTAooU3BvdXNlKSBUagpUKgpFVApCVAoxIDAgMCAxIDQ0NyAyMzguMDUwODAgVG0KL0YyKzAgMTAgVGYKMTIgVEwKKE5vbmUpIFRqClQqCkVUCkJUCjEgMCAwIDEgNTI0IDIzOS4wNTA4MCBUbQovRjIrMCAxMCBUZgoxMiBUTAooMTIzXDA1NTEyXDA1NTEyMzQpIFRqClQqCkVUCkJUCjEgMCAwIDEgOTkgMTU4Ljg0NTcwIFRtCi9GMiswIDkgVGYKMTAuODAwMDAgVEwKKEpvaG5ueSBTbWl0aCkgVGoKVCoKRVQKQlQKMSAwIDAgMSAyMzAgMTQxLjg0NTcwIFRtCi9GMiswIDkgVGYKMTAuODAwMDAgVEwKKDIwMDBcMDU1MDFcMDU1MDFUMDBcMDcyMDBcMDcyMDApIFRqClQqCkVUCkJUCjEgMCAwIDEgMzkgMTQwLjg0NTcwIFRtCi9GMiswIDkgVGYKMTAuODAwMDAgVEwKKDU1NVwwNTU1NVwwNTU1NTU1KSBUagpUKgpFVApCVAoxIDAgMCAxIDQ1MCAxNzcuODQ1NzAgVG0KL0YyKzAgOSBUZgoxMC44MDAwMCBUTAooMjBcMDU0MDAwKSBUagpUKgpFVApCVAoxIDAgMCAxIDUzOCAxNzcuODQ1NzAgVG0KL0YyKzAgOSBUZgoxMC44MDAwMCBUTAooMlwwNTY1MCkgVGoKVCoKRVQKQlQKMSAwIDAgMSAxNDggMTQwLjA1MDgwIFRtCi9GMiswIDEwIFRmCjEyIFRMCihYKSBUagpUKgpFVApCVAoxIDAgMCAxIDk5IDEwNC44NDU3MCBUbQovRjIrMCA5IFRmCjEwLjgwMDAwIFRMCihTdXNpZSBTbWl0aCkgVGoKVCoKRVQKQlQKMSAwIDAgMSAyMjcgODYuODQ1NzAgVG0KL0YyKzAgOSBUZgoxMC44MDAwMCBUTAooMjAwMVwwNTUwMlwwNTUwMlQwMFwwNzIwMFwwNzIwMCkgVGoKVCoKRVQKQlQKMSAwIDAgMSAzNiA4Ny44NDU3MCBUbQovRjIrMCA5IFRmCjEwLjgwMDAwIFRMCigzMzRcMDU1MzNcMDU1MjIyMikgVGoKVCoKRVQKQlQKMSAwIDAgMSA0NDYgMTAxLjg0NTcwIFRtCi9GMiswIDkgVGYKMTAuODAwMDAgVEwKKDIwXDA1NDAwMCkgVGoKVCoKRVQKQlQKMSAwIDAgMSA1MzEgMTAxLjg0NTcwIFRtCi9GMiswIDkgVGYKMTAuODAwMDAgVEwKKDJcMDU2NTApIFRqClQqCkVUCkJUCjEgMCAwIDEgMTY0IDg2LjA1MDc4IFRtCi9GMiswIDEwIFRmCjEyIFRMCihYKSBUagpUKgpFVApxCjAuNTQ1MTAgMCAwIHJnCkJUCi9GMyAxMCBUZgoxMiBUTApFVApCVAoxIDAgMCAxIDM4NSAxNDcgVG0KKFNFRSBBVFRBQ0hFRCBGT1IgQURESVRJT05BTCBDSElMRFJFTikgVGoKVCoKRVQKUQpCVAoxIDAgMCAxIDUxNyA3MTAuMDUwODAgVG0KL0YyKzAgMTAgVGYKMTIgVEwKKFgpIFRqClQqCkVUCkJUCjEgMCAwIDEgNTYxIDczMy4wNTA4MCBUbQovRjIrMCAxMCBUZgoxMiBUTAooWCkgVGoKVCoKRVQKQlQKMSAwIDAgMSAzODIgNjI5LjA1MDgwIFRtCi9GMiswIDEwIFRmCjEyIFRMCihYKSBUagpUKgpFVApCVAoxIDAgMCAxIDYzIDQwNC4wNTA4MCBUbQovRjIrMCAxMCBUZgoxMiBUTAooWCkgVGoKVCoKRVQKUQoKZW5kc3RyZWFtCmVuZG9iago3IDAgb2JqCjw8Ci9CQm94IFsgNTA3Ljk0NTAwIDc0LjYyMzEwIDU4OS40NjcwMCAxOC44Mzg1MCBdCi9SZXNvdXJjZXMgPDwKL0NvbG9yU3BhY2UgPDwKL0NTMCA5IDAgUgo+PgovRXh0R1N0YXRlIDw8Ci9HUzAgMTEgMCBSCj4+Cj4+Ci9MZW5ndGggNzMKL0dyb3VwIDggMCBSCi9NYXRyaXggWyAxIDAgMCAxIDAgMCBdCi9TdWJ0eXBlIC9Gb3JtCj4+CnN0cmVhbQovQ1MwIGNzIDAuODU4IDAuODggMC44OTIgIHNjbgovR1MwIGdzCjUwNy45NDUgMTguODM5IDgxLjUyMyA1NS43ODUgcmUKZioKCmVuZHN0cmVhbQplbmRvYmoKOCAwIG9iago8PAovUyAvVHJhbnNwYXJlbmN5Ci9UeXBlIC9Hcm91cAovSyBmYWxzZQovSSBmYWxzZQo+PgplbmRv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Cjw8Ci9BSVMgZmFsc2UKL0JNIC9Ob3JtYWwKL2NhIDEKL09QTSAxCi9TTWFzayAvTm9uZQovQ0EgMQovVHlwZSAvRXh0R1N0YXRlCi9vcCBmYWxzZQovU0EgdHJ1ZQovT1AgZmFsc2UKPj4KZW5kb2JqCjEyIDAgb2JqCjw8Ci9CQm94IFsgNDI1LjAzMTAwIDc0LjYyMzEwIDUwNi4wODAwMCAxOC44Mzg1MCBdCi9SZXNvdXJjZXMgPDwKL0NvbG9yU3BhY2UgPDwKL0NTMCA5IDAgUgo+PgovRXh0R1N0YXRlIDw8Ci9HUzAgMTEgMCBSCj4+Cj4+Ci9MZW5ndGggNzMKL0dyb3VwIDEzIDAgUgovTWF0cml4IFsgMSAwIDAgMSAwIDAgXQovU3VidHlwZSAvRm9ybQo+PgpzdHJlYW0KL0NTMCBjcyAwLjg1OCAwLjg4IDAuODkyICBzY24KL0dTMCBncwo0MjUuMDMxIDE4LjgzOSA4MS4wNDkgNTUuNzg1IHJlCmYqCgplbmRzdHJlYW0KZW5kb2JqCjEzIDAgb2JqCjw8Ci9TIC9UcmFuc3BhcmVuY3kKL1R5cGUgL0dyb3VwCi9LIGZhbHNlCi9JIGZhbHNlCj4+CmVuZG9iagoxNCAwIG9iago8PAovQkJveCBbIDUxMy4zNTMwMCA2NDQuMDQ1MDAgNTk4LjcwMDAwIDYxMy44NTUwMCBdCi9SZXNvdXJjZXMgPDwKL0NvbG9yU3BhY2UgPDwKL0NTMCA5IDAgUgo+PgovRXh0R1N0YXRlIDw8Ci9HUzAgMTEgMCBSCj4+Cj4+Ci9MZW5ndGggNzMKL0dyb3VwIDE1IDAgUgovTWF0cml4IFsgMSAwIDAgMSAwIDAgXQovU3VidHlwZSAvRm9ybQo+PgpzdHJlYW0KL0NTMCBjcyAwLjg1OCAwLjg4IDAuODkyICBzY24KL0dTMCBncwo1MTMuMzUzIDYxMy44NTUgODUuMzQ3IDMwLjE5IHJlCmYqCgplbmRzdHJlYW0KZW5kb2JqCjE1IDAgb2JqCjw8Ci9TIC9UcmFuc3BhcmVuY3kKL1R5cGUgL0dyb3VwCi9LIGZhbHNlCi9JIGZhbHNlCj4+CmVuZG9iagoxNiAwIG9iago8PAovQkJveCBbIDQyNC40MzYwMCA2NDQuODE3MDAgNTA5Ljc4MzAwIDYxMy44NTUwMCBdCi9SZXNvdXJjZXMgPDwKL0NvbG9yU3BhY2UgPDwKL0NTMCA5IDAgUgo+PgovRXh0R1N0YXRlIDw8Ci9HUzAgMTEgMCBSCj4+Cj4+Ci9MZW5ndGggNzQKL0dyb3VwIDE3IDAgUgovTWF0cml4IFsgMSAwIDAgMSAwIDAgXQovU3VidHlwZSAvRm9ybQo+PgpzdHJlYW0KL0NTMCBjcyAwLjg1OCAwLjg4IDAuODkyICBzY24KL0dTMCBncwo0MjQuNDM2IDYxMy44NTUgODUuMzQ3IDMwLjk2MiByZQpmKgoKZW5kc3RyZWFtCmVuZG9iagoxNyAwIG9iago8PAovUyAvVHJhbnNwYXJlbmN5Ci9UeXBlIC9Hcm91cAovSyBmYWxzZQovSSBmYWxzZQo+PgplbmRvYmoKMTggMCBvYmoKPDwKL0JCb3ggWyA0MjQuNTAwMDAgMTMxLjUxOTAwIDUwNS41NDkwMCA5NS4zNTIyMCBdCi9SZXNvdXJjZXMgPDwKL0NvbG9yU3BhY2UgPDwKL0NTMCA5IDAgUgo+PgovRXh0R1N0YXRlIDw8Ci9HUzAgMTEgMCBSCj4+Cj4+Ci9MZW5ndGggNzEKL0dyb3VwIDE5IDAgUgovTWF0cml4IFsgMSAwIDAgMSAwIDAgXQovU3VidHlwZSAvRm9ybQo+PgpzdHJlYW0KL0NTMCBjcyAwLjg1OCAwLjg4IDAuODkyICBzY24KL0dTMCBncwo0MjQuNSA5NS4zNTIgODEuMDQ5IDM2LjE2NyByZQpmKgoKZW5kc3RyZWFtCmVuZG9iagoxOSAwIG9iago8PAovUyAvVHJhbnNwYXJlbmN5Ci9UeXBlIC9Hcm91cAovSyBmYWxzZQovSSBmYWxzZQo+PgplbmRvYmoKMjAgMCBvYmoKPDwKL0JCb3ggWyA1MDguNDE5MDAgMTMxLjUxOTAwIDU4OS40NjcwMCA5NS4zNTIyMCBdCi9SZXNvdXJjZXMgPDwKL0NvbG9yU3BhY2UgPDwKL0NTMCA5IDAgUgo+PgovRXh0R1N0YXRlIDw8Ci9HUzAgMTEgMCBSCj4+Cj4+Ci9MZW5ndGggNzMKL0dyb3VwIDIxIDAgUgovTWF0cml4IFsgMSAwIDAgMSAwIDAgXQovU3VidHlwZSAvRm9ybQo+PgpzdHJlYW0KL0NTMCBjcyAwLjg1OCAwLjg4IDAuODkyICBzY24KL0dTMCBncwo1MDguNDE5IDk1LjM1MiA4MS4wNDkgMzYuMTY3IHJlCmYqCgplbmRzdHJlYW0KZW5kb2JqCjIxIDAgb2JqCjw8Ci9TIC9UcmFuc3BhcmVuY3kKL1R5cGUgL0dyb3VwCi9LIGZhbHNlCi9JIGZhbHNlCj4+CmVuZG9iagoyMiAwIG9iago8PAovQkJveCBbIDUxMy40MTcwMCAxOTUuNDM1MDAgNTk4Ljc2NDAwIDE2NS4yNDQwMCBdCi9SZXNvdXJjZXMgPDwKL0NvbG9yU3BhY2UgPDwKL0NTMCA5IDAgUgo+PgovRXh0R1N0YXRlIDw8Ci9HUzAgMTEgMCBSCj4+Cj4+Ci9MZW5ndGggNzQKL0dyb3VwIDIzIDAgUgovTWF0cml4IFsgMSAwIDAgMSAwIDAgXQovU3VidHlwZSAvRm9ybQo+PgpzdHJlYW0KL0NTMCBjcyAwLjg1OCAwLjg4IDAuODkyICBzY24KL0dTMCBncwo1MTMuNDE3IDE2NS4yNDQgODUuMzQ3IDMwLjE5MSByZQpmKgoKZW5kc3RyZWFtCmVuZG9iagoyMyAwIG9iago8PAovUyAvVHJhbnNwYXJlbmN5Ci9UeXBlIC9Hcm91cAovSyBmYWxzZQovSSBmYWxzZQo+PgplbmRvYmoKMjQgMCBvYmoKPDwKL0JCb3ggWyA0MjQuNTAwMDAgMTk2LjIwNjAwIDUwOS44NDcwMCAxNjUuMjQ0MDAgXQovUmVzb3VyY2VzIDw8Ci9Db2xvclNwYWNlIDw8Ci9DUzAgOSAwIFIKPj4KL0V4dEdTdGF0ZSA8PAovR1MwIDExIDAgUgo+Pgo+PgovTGVuZ3RoIDcyCi9Hcm91cCAyNSAwIFIKL01hdHJpeCBbIDEgMCAwIDEgMCAwIF0KL1N1YnR5cGUgL0Zvcm0KPj4Kc3RyZWFtCi9DUzAgY3MgMC44NTggMC44OCAwLjg5MiAgc2NuCi9HUzAgZ3MKNDI0LjUgMTY1LjI0NCA4NS4zNDcgMzAuOTYyIHJlCmYqCgplbmRzdHJlYW0KZW5kb2JqCjI1IDAgb2JqCjw8Ci9TIC9UcmFuc3BhcmVuY3kKL1R5cGUgL0dyb3VwCi9LIGZhbHNlCi9JIGZhbHNlCj4+CmVuZG9iagoyNiAwIG9iago8PAovQkJveCBbIDUxMy44OTQwMCA0NDYuMTYxMDAgNTk5LjI0MTAwIDQxMi42MTIwMCBdCi9SZXNvdXJjZXMgPDwKL0NvbG9yU3BhY2UgPDwKL0NTMCA5IDAgUgo+PgovRXh0R1N0YXRlIDw8Ci9HUzAgMTEgMCBSCj4+Cj4+Ci9MZW5ndGggNzQKL0dyb3VwIDI3IDAgUgovTWF0cml4IFsgMSAwIDAgMSAwIDAgXQovU3VidHlwZSAvRm9ybQo+PgpzdHJlYW0KL0NTMCBjcyAwLjg1OCAwLjg4IDAuODkyICBzY24KL0dTMCBncwo1MTMuODk0IDQxMi42MTIgODUuMzQ3IDMzLjU0OSByZQpmKgoKZW5kc3RyZWFtCmVuZG9iagoyNyAwIG9iago8PAovUyAvVHJhbnNwYXJlbmN5Ci9UeXBlIC9Hcm91cAovSyBmYWxzZQovSSBmYWxzZQo+PgplbmRvYmoKMjggMCBvYmoKPDwKL0JCb3ggWyA0MjQuOTc4MDAgNDQ3LjAxODAwIDUxMC4zMjUwMCA0MTIuNjEyMDAgXQovUmVzb3VyY2VzIDw8Ci9Db2xvclNwYWNlIDw8Ci9DUzAgOSAwIFIKPj4KL0V4dEdTdGF0ZSA8PAovR1MwIDExIDAgUgo+Pgo+PgovTGVuZ3RoIDc0Ci9Hcm91cCAyOSAwIFIKL01hdHJpeCBbIDEgMCAwIDEgMCAwIF0KL1N1YnR5cGUgL0Zvcm0KPj4Kc3RyZWFtCi9DUzAgY3MgMC44NTggMC44OCAwLjg5MiAgc2NuCi9HUzAgZ3MKNDI0Ljk3OCA0MTIuNjEyIDg1LjM0NyAzNC40MDYgcmUKZioKCmVuZHN0cmVhbQplbmRvYmoKMjkgMCBvYmoKPDwKL1MgL1RyYW5zcGFyZW5jeQovVHlwZSAvR3JvdXAKL0sgZmFsc2UKL0kgZmFsc2UKPj4KZW5kb2JqCjMwIDAgb2JqCjw8Ci9BSVMgZmFsc2UKL0JNIC9Ob3JtYWwKL2NhIDEKL09QTSAxCi9TTWFzayAvTm9uZQovQ0EgMQovVHlwZSAvRXh0R1N0YXRlCi9vcCB0cnVlCi9TQSB0cnVlCi9PUCB0cnVlCj4+CmVuZG9iagozMSAwIG9iago8PAovQUlTIGZhbHNlCi9CTSAvTm9ybWFsCi9jYSAwLjMwMDAwCi9PUE0gMQovU01hc2sgL05vbmUKL0NBIDAuMzAwMDAKL1R5cGUgL0V4dEdTdGF0ZQovb3AgZmFsc2UKL1NBIHRydWUKL09QIGZhbHNlCj4+CmVuZG9iagozMiAwIG9iago8PAovRmlyc3RDaGFyIDAKL1dpZHRocyBbIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgXQovU3VidHlwZSAvVHJ1ZVR5cGUKL1R5cGUgL0ZvbnQKL0Jhc2VGb250IC9BQUFBQUErTHVjaWRhQ29uc29sZQovTGFzdENoYXIgMTI3Ci9OYW1lIC9GMiswCi9Gb250RGVzY3JpcHRvciAzMyAwIFIKL1RvVW5pY29kZSAzNSAwIFIKPj4KZW5kb2JqCjMzIDAgb2JqCjw8Ci9Gb250QkJveCBbIDAgLTIxMC45Mzc1MCA2MDIuNTM5MTAgNzg5LjA2MjUwIF0KL0ZvbnROYW1lIC9BQUFBQUErTHVjaWRhQ29uc29sZQovRm9udEZpbGUyIDM0IDAgUgovRGVzY2VudCAtMjA1LjA3ODEwCi9GbGFncyA1Ci9Bc2NlbnQgNzgzLjIwMzEwCi9TdGVtViA4NwovVHlwZSAvRm9udERlc2NyaXB0b3IKL0l0YWxpY0FuZ2xlIDAKL0NhcEhlaWdodCA3ODMuMjAzMTAKPj4K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0V/wF/1vC/G2BNgKDN77DPWHcTwu1GkVT8ODyVkN7X04t5stTuHzG6i5o+YKZW5kc3RyZWFtCmVuZG9iagozNiAwIG9iago8PAovRW5jb2RpbmcgL1dpbkFuc2lFbmNvZGluZwovVHlwZSAvRm9udAovTmFtZSAvRjMKL0Jhc2VGb250IC9Db3VyaWVyLU9ibGlxdWUKL1N1YnR5cGUgL1R5cGUxCj4+CmVuZG9iagozNyAwIG9iago8PAovRmlyc3RDaGFyIDMyCi9XaWR0aHMgWyAyNDAgMCAwIDAgMCAwIDAgMCAwIDAgMCAwIDAgMzYwIDAgMzIwIDAgNzAwIDcwMCAwIDcwMCA3MDAgMCAwIDAgMCAwIDAgMCAwIDAgMCAwIDcyMCAwIDc1MCAwIDAgNjMwIDAgMCAyOTAgMCAwIDAgMCAwIDAgNjUwIDAgNzA1IDAgMCAwIDAgMCAwIDAgMCAwIDAgMCAwIDAgMCA1NzAgMCAwIDAgNTcwIDAgNTk5IDAgMCAwIDAgMCAwIDAgMCA1OTkgXQovRW5jb2RpbmcgL1dpbkFuc2lFbmNvZGluZwovVHlwZSAvRm9udAovQmFzZUZvbnQgL0JIUUxQVitHYWxheGllUG9sYXJpcy1Cb29rCi9MYXN0Q2hhciAxMTIKL0ZvbnREZXNjcmlwdG9yIDM4IDAgUgovU3VidHlwZSAvVHlwZTEKPj4KZW5kb2JqCjM4IDAgb2JqCjw8Ci9Gb250QkJveCBbIC0xOTkgLTI1NiAxNzc5IDExMzQgXQovRm9udE5hbWUgL0JIUUxQVitHYWxheGllUG9sYXJpcy1Cb29rCi9Gb250U3RyZXRjaCAvTm9ybWFsCi9EZXNjZW50IC0yNTYKL1hIZWlnaHQgNTMwCi9GbGFncyAzMgovRm9udEZpbGUzIDM5IDAgUgovQXNjZW50IDExMzQKL1N0ZW1WIDY4Ci9UeXBlIC9Gb250RGVzY3JpcHRvcgovRm9udFdlaWdodCA1MDAKL0l0YWxpY0FuZ2xlIDAKL0NoYXJTZXQgKFwwNTdzcGFjZVwwNTdoeXBoZW5cMDU3c2xhc2hcMDU3b25lXDA1N3R3b1wwNTdmb3VyXDA1N2ZpdmVcMDU3QVwwNTdDXDA1N0ZcMDU3SVwwNTdQXDA1N1JcMDU3YVwwNTdlXDA1N2dc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NTA4IDAgNTI2IDUwOSA0NTUgMCAwIDAgMCA1MTYgMCAwIDAgMCAwIDAgMCA0NzYgNTA3IDQ1NiA1MDcgNDc0IDMzOSA1MDggNTAyIDIzMSAwIDQ5MCAyMzIgNzM3IDUwMyA0ODQgNTA3IDAgMzQxIDQ1NSAzNTkgNTAzIDQ1NCA2NDUgMCA0ODEgNDE1IDAgMCAwIDAgMCAwIDAgMCAwIDAgMCAwIDAgMCAwIDAgMCAwIDAgMCAwIDAgMCAwIDAgMCAwIDAgMTAyMCBdCi9FbmNvZGluZyAvV2luQW5zaUVuY29kaW5nCi9UeXBlIC9Gb250Ci9CYXNlRm9udCAvR01EUEhMK0dhbGF4aWVQb2xhcmlzQ29uZGVuc2VkLUJvbGQKL0xhc3RDaGFyIDE1MQovRm9udERlc2NyaXB0b3IgNDIgMCBSCi9TdWJ0eXBlIC9UeXBlMQo+PgplbmRvYmoKNDIgMCBvYmoKPDwKL0ZvbnRCQm94IFsgLTEzMCAtMjUzIDEzNjQgMTA1MCBdCi9Gb250TmFtZSAvR01EUEhMK0dhbGF4aWVQb2xhcmlzQ29uZGVuc2VkLUJvbGQKL0ZvbnRTdHJldGNoIC9Db25kZW5zZWQKL0Rlc2NlbnQgLTI1MwovWEhlaWdodCA1MzAKL0ZsYWdzIDMyCi9Gb250RmlsZTMgNDMgMCBSCi9Bc2NlbnQgMTA1MAovU3RlbVYgMTMyCi9UeXBlIC9Gb250RGVzY3JpcHRvcgovRm9udFdlaWdodCA4MDAKL0l0YWxpY0FuZ2xlIDAKL0NoYXJTZXQgKFwwNTdzcGFjZVwwNTdudW1iZXJzaWduXDA1N3BhcmVubGVmdFwwNTdwYXJlbnJpZ2h0XDA1N2h5cGhlblwwNTdvbmVcMDU3dHdvXDA1N3RocmVlXDA1N2ZpdmVcMDU3c2l4XDA1N3NldmVuXDA1N2NvbG9uXDA1N0FcMDU3QlwwNTdDXDA1N0VcMDU3SFwwNTdJXDA1N0xcMDU3TlwwNTdPXDA1N1BcMDU3UlwwNTdTXDA1N1RcMDU3WVwwNTdhXDA1N2JcMDU3Y1wwNTdkXDA1N2VcMDU3ZlwwNTdnXDA1N2hcMDU3aVwwNTdrXDA1N2xcMDU3bVwwNTduXDA1N29cMDU3cFwwNTdyXDA1N3NcMDU3dFwwNTd1XDA1N3Zc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IFsgMTgwIDAgMCAwIDAgMCAwIDAgMjYwIDI2MCAwIDAgMCAzODAgMCAwIDAgMCAwIDAgMCAwIDAgMCAwIDAgMCAwIDAgMCAwIDAgMCAwIDAgMCAwIDAgMzc4IDAgMCAxNjYgMCAwIDAgMCAwIDAgNDE0IDAgMCAwIDM4MCAwIDAgMCAwIDAgMCAwIDAgMCAwIDAgMCAwIDAgMCAwIDAgMCAwIDAgMCAwIDAgMTU0IDAgNDM0IDQ1MCAwIDAgMCAwIDAgMCAwIDAgMCA0MTAgXQovRW5jb2RpbmcgL1dpbkFuc2lFbmNvZGluZwovVHlwZSAvRm9udAovQmFzZUZvbnQgL0NHQVJEVCtHYWxheGllUG9sYXJpc0NvbmRlbnNlZC1MaWdodAovTGFzdENoYXIgMTIxCi9Gb250RGVzY3JpcHRvciA0NSAwIFIKL1N1YnR5cGUgL1R5cGUxCj4+CmVuZG9iago0NSAwIG9iago8PAovRm9udEJCb3ggWyAtMTE1IC0yNTYgMTM2MCAxMDYwIF0KL0ZvbnROYW1lIC9DR0FSRFQrR2FsYXhpZVBvbGFyaXNDb25kZW5zZWQtTGlnaHQKL0ZvbnRTdHJldGNoIC9Db25kZW5zZWQKL0Rlc2NlbnQgLTI1NgovWEhlaWdodCA1MzAKL0ZsYWdzIDMyCi9Gb250RmlsZTMgNDYgMCBSCi9Bc2NlbnQgMTA2MAovU3RlbVYgMzYKL1R5cGUgL0ZvbnREZXNjcmlwdG9yCi9Gb250V2VpZ2h0IDQwMAovSXRhbGljQW5nbGUgMAovQ2hhclNldCAoXDA1N3NwYWNlXDA1N3BhcmVubGVmdFwwNTdwYXJlbnJpZ2h0XDA1N2h5cGhlblwwNTdGXDA1N0lcMDU3UFwwNTdUXDA1N2xcMDU3blwwNTdvXDA1N3kpCi9Gb250RmFtaWx5IChHYWxheGllIFBvbGFyaXMgQ29uZGVuc2VkIExpZ2h0KQovQ2FwSGVpZ2h0IDc4MAo+PgplbmRvYmoKNDYgMCBvYmoKPDwKL0xlbmd0aCA3NjAKL0ZpbHRlciAvRmxhdGVEZWNvZGUKL1N1YnR5cGUgL1R5cGUxQwo+PgpzdHJlYW0KSIkskEtME2sUx7/PYWaituMDpjGIdLzi9VW5qJH4WqgUSpMqhIsGUJuM7bRUa9FWi7jB58poXFiDiYtxAU0uhYFcuSGNmPisC8WUEGNiggsTNRqjkpjzjackdyCe3Tnn//vn/A8lRfMIpXR1jWdPk7t5g0eNqmcjWmNHVI1HEjUdsaAWS2jBjb5IuP30rPAPtpyysiK2wiajiDfL+MZfozzULwJ1MbQv6TNvLyUcpXW+1pqOk13xWUpZG1inbK6q2qYc7VIORqJRNawpzV0nNeVPxa0lIuGY4vPVVCp7olFlDkgocS2hxZNasHJ/R/yEGv2r7u85fZUS1ELEKkrWkPVkE9lK5pMFRCL7SANpJG2EV+q8jc3EbkUiXuIjMXKJPCBv6SJaS1tpmPbSj/OKmXn1pVn2kn7LmyV57pvD1E19RhdYacEjQz/284VHgmklfCTgbOf3y5jGNM+yIqOYliENab6QFSVskG7pSVaSgakMzRncF1YiQzyDcZiCGxm8N6MbgoRH9KSpd9IXBvfd1OXw3DCrJ+FDhtlmMbgyZJGizGw/xffZAK7FdYeDW5wrj8lAhB+vRsADHs/IaidWzKE9ll13hsJug4Og2S2DK4OfjoFrpttaKuj41Uxzw9xXmJahbQgceXCVtwsNFa3oxZZSbBlDb66ifFCoBVcIHNBW+puB6mEOqi0K20LoqEWXpclNj4EXWkqhpRW8DdOWTx5dQ+hAi+qBiQGagx4OJmFCHsCeiCjpSfrM4J45DPNeWJDGrddcH7jQufSJATeM4tF3rE+OCdcUvrDTEIvfsF1i9p/Hg6+WT/QGDjkL1QarFZ/672/f0RQInCqXpi70s3P9dGwY+qzDztyVXeo4uME9Mv7580g9utGt1rucx4vA/nAS/OA/MIl2tB/Yi370P9wLdqf0H3qSZqyTDsInDtTnMi5jEV4TYEPhTh2G+H+F13ARVrHz1kw6f4v1peBySsBQSkwtyC9MPbbZoLKEcfL/AgwA8VBqOQplbmRzdHJlYW0KZW5kb2JqCjQ3IDAgb2JqCjw8Ci9GaXJzdENoYXIgMzIKL1dpZHRocyBbIDE3NiAwIDAgMCA0MTUgMCAwIDAgMjc0IDI3NCAzMzAgMCAxODAgMzg3IDE4MCAzNDIgNTIwIDUyMCA1MjAgNTIwIDAgNTIwIDUyMCA1MjAgNTIwIDUyMCAxODAgMTgwIDAgMCAwIDQ0MiAwIDQ4MiA1MDYgNDg2IDUxMyA0MjggMzk5IDUxMCA1MTYgMTkyIDAgMCAzNzggNjI5IDUwNyA1MDQgNDQyIDAgNDU5IDQ2NCA0MDIgNTA0IDQ4MiA2NTkgMCA0ODQgNDYzIDAgMCAwIDAgMCAwIDQ0MyA0NjggNDI0IDQ2OCA0NDQgMjk3IDQ2OCA0NTUgMTgwIDE4MCA0NDAgMTgwIDY4OCA0NTUgNDYwIDQ2OCA0NjggMzAyIDQwMyAzMTcgNDU0IDQxNiA2MDAgNDM3IDQzMSAzOTcgMCAwIDAgMCA0MDcgMCAwIDAgMCAwIDAgMCAwIDAgMCAwIDAgMCAwIDAgMCAwIDAgMTY1IDI4NyAyOTAgXQovRW5jb2RpbmcgL1dpbkFuc2lFbmNvZGluZwovVHlwZSAvRm9udAovQmFzZUZvbnQgL0tTQk5MRCtHYWxheGllUG9sYXJpc0NvbmRlbnNlZC1Cb29rCi9MYXN0Q2hhciAxNDgKL0ZvbnREZXNjcmlwdG9yIDQ4IDAgUgovU3VidHlwZSAvVHlwZTEKPj4KZW5kb2JqCjQ4IDAgb2JqCjw8Ci9Gb250QkJveCBbIC0xNDAgLTI1NiAxMzYxIDEwNTAgXQovRm9udE5hbWUgL0tTQk5MRCtHYWxheGllUG9sYXJpc0NvbmRlbnNlZC1Cb29rCi9Gb250U3RyZXRjaCAvQ29uZGVuc2VkCi9EZXNjZW50IC0yNTYKL1hIZWlnaHQgNTMwCi9GbGFncyAzMgovRm9udEZpbGUzIDQ5IDAgUgovQXNjZW50IDEwNTAKL1N0ZW1WIDY0Ci9UeXBlIC9Gb250RGVzY3JpcHRvcgovRm9udFdlaWdodCA2MDAKL0l0YWxpY0FuZ2xlIDAKL0NoYXJTZXQgKFwwNTdzcGFjZVwwNTdkb2xsYXJcMDU3cGFyZW5sZWZ0XDA1N3BhcmVucmlnaHRcMDU3YXN0ZXJpc2tcMDU3Y29tbWFcMDU3aHlwaGVuXDA1N3BlcmlvZFwwNTdzbGFzaFwwNTd6ZXJvXDA1N29uZVwwNTd0d29cMDU3dGhyZWVcMDU3Zml2ZVwwNTdzaXhcMDU3c2V2ZW5cMDU3ZWlnaHRcMDU3bmluZVwwNTdjb2xvblwwNTdzZW1pY29sb25cMDU3cXVlc3Rpb25cMDU3QVwwNTdCXDA1N0NcMDU3RFwwNTdFXDA1N0ZcMDU3R1wwNTdIXDA1N0lc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YXJhdGlvbiAvUEFOVE9ORSMyMDI4NiMyMFUgOSAwIFIgPDwKL04gMQovRG9tYWluIFsgMCAxIF0KL0MxIFsgMC4xOTY3OSAwLjMzNjExIDAuNjQ0MjkgXQovUmFuZ2UgWyAwIDEgMCAxIDAgMSBdCi9DMCBbIDEgMSAxIF0KL0Z1bmN0aW9uVHlwZSAyCj4+IF0KZW5kb2JqCjYwIDAgb2JqClsgL1NlcGFyYXRpb24gL1BBTlRPTkUjMjAyODYjMjBDVlUgOSAwIFIgPDwKL04gMQovRG9tYWluIFsgMCAxIF0KL0MxIFsgMCAwLjM4MDQ0IDAuNjY4MDUgXQovUmFuZ2UgWyAwIDEgMCAxIDAgMSBdCi9DMCBbIDEgMSAxIF0KL0Z1bmN0aW9uVHlwZSAyCj4+IF0KZW5kb2JqCjYxIDAgb2JqCjw8Ci9TIC9UcmFuc3BhcmVuY3kKL0NTIDYyIDAgUgovSyBmYWxzZQovSSBmYWxzZQo+PgplbmRvYmoKNjIgMCBvYmoKWyAvSUNDQmFzZWQgMTAgMCBSIF0KZW5kb2JqCjYzIDAgb2JqCjw8Ci9MZW5ndGggMjk5NjUKPj4Kc3RyZWFtCnEKQlQKL0NTMCBjcwowLjAwMTAwIDAuMDAyMDAgMC4wMDIwMCBzY24KL0dTMCBncwovVDFfMCAxIFRmCjEwIDAgMCAxMCAyMC4zNjg3MCA3NDUuNjM5NjAgVG0KKERvIHlvdVwwNTQgeW91ciBzcG91c2VcMDU0IG9yIGNoaWxkcmVuIGhhdmUgYW55IGV4aXN0aW5nIGxpZmUgaW5zdXJhbmNlIG9yIGFubnVpdHkgY29udHJhY3RzXDA3NykgVGoKKCAgICAgICBZICAgICAgTikgVGoKKCApIFRqCiggICkgVGoKMCAtMS4zMDAwMCBURAooV2lsbCB0aGUgY292ZXJhZ2UgYXBwbGllZCBmb3IgcmVwbGFjZSBhbnkgZXhpc3RpbmcgbGlmZSBpbnN1cmFuY2Ugb3IgYW5udWl0aWVzXDA3NykgVGoKKCAgICAgICBZICAgICAgTikgVGoKKCApIFRqCiggICkgVGoKMCAtMS43MDAwMCBURAooSWYgeW91IGFuc3dlcmVkIFwyMjN5ZXNcMjI0IHRvIGVpdGhlciBxdWVzdGlvbiBwbGVhc2UgY29tcGxldGUgYW5kIHNpZ24gdGhlIE5vdGljZSBvZiBSZXBsYWNlbWVudFwwNTYpIFRqCkVUCi9DUzEgY3MKMSBzY24KMTcgNzU5LjU4ODAwIDU3OCAxMy44MDAwMCByZQpmKgpCVAovQ1MwIGNzCjEgMSAxIHNjbgovVDFfMSAxIFRmCjkuNTAwMDAgMCAwIDkuNTAwMDAgMjAuMzY4NzAgNzYyLjY4ODUwIFRtCihTZWN0aW9uIDUgXDA1NSBPdGhlciBJbnN1cmFuY2UpIFRqCkVUCjAuMTM3MDAgMC4xMjMwMCAwLjEyNjAwIHNjbgovR1MxIGdzCnEKMSAwIDAgMSAzNDEuMDU5MzAgNzUxLjcyMTQwIGNtCjAgMCBtCjAgLTUuMDIwMDAgbAotMC40NDgwMCAtNS40NTEwMCBsCi01LjM1NjAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTAuNDAwMDAgbAotNC45MzkwMCAwIGwKaAotNS4wMTkwMCAtNS4xMDAwMCA0LjQyNzAwIDQuNTQ3MDAgcmUKZgpRCnEKMSAwIDAgMSAzNTcuMjE4NzAgNzUxLjcyMTQwIGNtCjAgMCBtCjAgLTUuMDIwMDAgbAotMC40NDgwMCAtNS40NTEwMCBsCi01LjM1NjAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTAuNDAwMDAgbAotNC45MzkwMCAwIGwKaAotNS4wMTkwMCAtNS4xMDAwMCA0LjQyNzAwIDQuNTQ3MDAgcmUKZgpRCnEKMSAwIDAgMSA1MzEuMTE0MDAgMTEyLjk2MDYwIGNtCjAgMCBtCjAgLTUuMDIwMDAgbAotMC40NDgwMCAtNS40NTEwMCBsCi01LjM1NjAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTAuNDAwMDAgbAotNC45MzkwMCAwIGwKaAotNS4wMTkwMCAtNS4xMDAwMCA0LjQyNzAwIDQuNTQ3MDAgcmUKZgpRCnEKMSAwIDAgMSA1NTUuMzI0MzAgMTEyLjk2MDYwIGNtCjAgMCBtCjAgLTUuMDIwMDAgbAotMC40NDgwMCAtNS40NTEwMCBsCi01LjM1NjAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTAuNDAwMDAgbAotNC45MzkwMCAwIGwKaAotNS4wMTkwMCAtNS4xMDAwMCA0LjQyNzAwIDQuNTQ3MDAgcmUKZgpRCkJUCjAuMDAxMDAgMC4wMDIwMCAwLjAwMjAwIHNjbgovR1MwIGdzCi9UMV8wIDEgVGYKMTAgMCAwIDEwIDI0MS41ODM1MCA2ODEuMTU2MjAgVG0KKCApIFRqCi9UMV8xIDEgVGYKLTIyLjA5MzAwIC0wLjM3NDAwIFRkCihQbGVhc2UgYW4pIFRqCihzKSBUago0LjIxMDAwIDAgVGQKKHcpIFRqCjAuNjM2MDAgMCBUZAooZSkgVGoKKHIpIFRqCjAuODA4MDAgMCBUZAooIHRoZSBmb2xsb3dpbmcgU3RhdGVtZW50IG9mIEhlYWx0aCBmb3IgYWxsIGNvdmVyYWdlXDA3MikgVGoKL1QxXzAgMSBUZgotNS42NTQwMCAtMS4yMDAwMCBUZApbIChJXDA1NiApIC03NDUuMzAwMDAgKEhhcyBhbnkgQXBwbGljYW50IGJlZW4gZGlhZ25vc2VkIG9yIHRyZWF0ZWQgYnkgYSBtZW1iZXIgb2YgdGhlIG1lZGljYWwgKSBdIFRKCjAgLTEuMjAwMDAgVEQKWyAoICkgLTExMTcuMzAwMDAgKHByb2Zlc3Npb25cMDU0IG9yIHRlc3RlZCBwb3NpdGl2ZSBmb3JcMDcyIEh1bWFuIEltbXVub2RlZmljaWVuY3kgVmlydXMgXDA1MEhJVlwwNTFcMDU0ICkgXSBUSgpUKgpbICggKSAtMTExNy4zMDAwMCAoQWNxdWlyZWQgSW1tdW5lIERlZmljaWVuY3kgU3luZHJvbWUgXDA1MEFJRFNcMDUxXDA1NCBvciBBSURTXDA1NVJlbGF0ZWQgQ29tcGxleCBcMDUwQVJDXDA1MVwwNzcpIF0gVEoKL1QxXzEgMSBUZgowIC0xLjgwMDAwIFRECihDb21wbGV0ZSBPTkxZIGlmIGFwcGx5aW5nIGZvciBTaW1wbGlmaWVkIElzc3VlIGFtb3VudHNcMDcyKSBUagovVDFfMCAxIFRmCjAgLTEuMjAwMDAgVEQKWyAoSUlcMDU2ICApIC0zNzcuNDAwMDAgKEhhcyBhKSBdIFRKCihuKSBUagozLjcyNTAwIDAgVGQKKHkpIFRqCiggKSBUagowLjU5ODAwIDAgVGQKKEFwcGxpY2FudCApIFRqCihlKSBUago0LjAyNzAwIDAgVGQKKHYpIFRqCjAuNDA3MDAgMCBUZAooZSkgVGoKKHIpIFRqCjAuNzM4MDAgMCBUZAooIGFwcGxpZWQgKSBUagooZikgVGoKMy4yOTUwMCAwIFRkCihvKSBUagoocikgVGoKMC43NTQwMCAwIFRkCiggYW5kIGJlZW4gKSBUagoocikgVGoKMy45OTcwMCAwIFRkCihlamVjdGVkICkgVGoKKGYpIFRqCjMuMTg5MDAgMCBUZAoobykgVGoKKHIpIFRqCjAuNzU1MDAgMCBUZAooIGxpKSBUagooZikgVGoKMC44MjkwMCAwIFRkCihlIGluc3UpIFRqCihyKSBUagoyLjQwNTAwIDAgVGQKKGFuY2VcMDc3XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTYpIFRqCi0yNC43MTgwMCAtMS4yMDAwMCBUZApbIChJSUlcMDU2ICkgLTM2MS4zMDAwMCAoSGFzIGFueSBBcHBsaWNhbikgXSBUSgoodCBiZWVuIGhvc3BpdGFsaSkgVGoKKHopIFRqCjEzLjM4OTAwIDAgVGQKKGVkIGluIHRoZSBwYXN0IDkwIGQpIFRqCihhKSBUago3LjIxNTAwIDAgVGQKKHkpIFRqCjAuNDE0MDAgMCBUZAooc1wwNzcgXDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTYpIFRqCi0yMS4wMTcwMCAtMS42MDAwMCBUZApbIChJVlwwNTYgKSAtMjYzLjMwMDAwIChJbiB0aGUgcGFzdCA1IHllYXJzXDA1NCBoYXMgYW55IEFwcGxpY2FudCBiZWVuIGhvc3BpdGFsaXplZCBmb3JcMDU0IGJlZW4gZGlhZ25vc2VkIG9yICkgXSBUSgowIC0xIFREClsgKCApIC0xMTE3LjMwMDAwICh0cmVhdGVkIGJ5IGEgbWVtYmVyIG9mIHRoZSBtZWRpY2FsIHByb2Zlc3Npb24gb3IgdGFrZW4gcHJlc2NyaXB0aW9uIG1lZGljYXRpb24gZm9yXDA3MikgXSBUSgowIC0xLjIwMDAwIFREClsgKCApIC0xMTUyLjMwMDAwIChBXDA1NiApIC0yNTMuMTAwMDAgKEFuZ2luYVwwNTQgaGVhcnQgYXR0YWNrXDA1NCBzdHJva2VcMDU0IGhlYXJ0IGJ5cGFzcyBzdXJnZXJ5XDA1NCBhbmdpb3BsYXN0eVwwNTQgY29yb25hcnkpIF0gVEoKVCoKWyAoICkgLTExNTIuMzAwMDAgKCApIC05MTUuMjAwMDAgKGFydGVyeSBzdGVudGluZ1wwNTQgb3IgY29yb25hcnkgYXJ0ZXJ5IGRpc2Vhc2VcMDc3ICkgXSBUSgooXDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NikgVGoKVCoKWyAoICkgLTExNTIuMzAwMDAgKEJcMDU2ICkgLTIyOS4yMDAwMCAoQW55IGZvcm0gb2YgY2FuY2VyIHRvIGluY2x1ZGUgbGV1a2VtaWEgb3IgSG9kZ2tpblwyMjJzIERpc2Vhc2UgKSBdIFRKClQqClsgKCApIC0xMTUyLjMwMDAwICggKSAtOTE1LjIwMDAwIChcMDUwZXhjbHVkaW5nIG5vblwwNTVpbnZhc2l2ZVwwNTQgbm9uXDA1NW0pIF0gVEoKKGVsYW5vbWEgc2tpbiBjYW5jZXJcMDUxXDA3NyBcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2KSBUagpUKgpbICggKSAtMTE1Mi4zMDAwMCAoQ1wwNTYgKSAtMjQ5LjEwMDAwIChDaHJvbmljIG9ic3RydWN0aXZlIHB1bG1vbmFyeSBkaXNlYXNlIFwwNTBDT1BEXDA1MVwwNTQgZW1waHlzZW1hXDA1NCBvciBhbnkgb3RoZXIpIF0gVEoKVCoKWyAoICkgLTExNTIuMzAwMDAgKCApIC05MTUuMjAwMDAgKGNocm9uaWMgcmVzcGlyYXRvcnkgZGlzb3JkZXJcMDU0IGV4Y2x1ZGluZyBhc3RobWFcMDc3ICkgXSBUSgooXDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTYpIFRqClQqClsgKCApIC0xMTUyLjMwMDAwIChEXDA1NiApIC0yMjIuMjAwMDAgKEFsY29ob2xpc20gb3IgZHJ1ZyBvciBhbGNvaG9sIGFidXNlXDA1NCBjaXJyaG9zaXNcMDU0IGhlcGF0aXRpc1wwNTQgb3IgYW55IG90aGVyKSBdIFRKClQqClsgKCApIC0xMTUyLjMwMDAwICggKSAtOTE1LjIwMDAwIChkaXNlYXNlIG9mIHRoZSBsaXZlclwwNzcgKSBdIFRKCihcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NlwwNTZcMDU2XDA1NikgVGoKOS40NjgwMCA2LjU3NDAwIFRkCiggKSBUagpFVAovQ1MxIGNzCjEgc2NuCjE3LjUzODAwIDY5Mi4xMDQwMCA1NzcuMjYzMDAgMTMuODAwMDAgcmUKZioKQlQKL0NTMCBjcwoxIDEgMSBzY24KL1QxXzEgMSBUZgo5LjUwMDAwIDAgMCA5LjUwMDAwIDIwLjY1NjMwIDY5NS4yMDMxMCBUbQooU2VjdGlvbiA2IFwwNTUgU3RhdGVtZW50IG9mIEhlYWx0aCkgVGoKMC4wMDEwMCAwLjAwMjAwIDAuMDAyMDAgc2NuCjEwIDAgMCAxMCAzMzcuMzkzMTAgNjc5LjE3ODcwIFRtCiggIEVtcGxveWVlKSBUagovVDFfMCAxIFRmCjAuODg0MDAgLTYuODg3MDAgVGQKKFllcyAgICAgICAgTm8gICAgICApIFRqCkVUCjAuMTM3MDAgMC4xMjMwMCAwLjEyNjAwIHNjbgovR1MxIGdzCnEKMSAwIDAgMSAzNDUuNTY0MTAgNjE2Ljc1NDkwIGNtCjAgMCBtCjAgLTUuMDIwMDAgbAotMC40NDgwMCAtNS40NTEwMCBsCi01LjM1NjAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTAuNDAwMDAgbAotNC45MzkwMCAwIGwKaAotNS4wMTkwMCAtNS4xMDAwMCA0LjQyNzAwIDQuNTQ3MDAgcmUKZgpRCnEKMSAwIDAgMSAzNzEuNzk3MTAgNjE2Ljc1NDkwIGNtCjAgMCBtCjAgLTUuMDIwMDAgbAotMC40NDgwMCAtNS40NTEwMCBsCi01LjM1NjAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTAuNDAwMDAgbAotNC45MzkwMCAwIGwKaAotNS4wMTkwMCAtNS4xMDAwMCA0LjQyNzAwIDQuNTQ3MDAgcmUKZgpRCkJUCjAuMDAxMDAgMC4wMDIwMCAwLjAwMjAwIHNjbgovR1MwIGdzCjEwIDAgMCAxMCAzNDYuMjI4NTAgNTk4LjMwMzcwIFRtCihZZXMgICAgICAgIE5vICAgICAgKSBUagpFVAowLjEzNzAwIDAuMTIzMDAgMC4xMjYwMCBzY24KL0dTMSBncwpxCjEgMCAwIDEgMzQ1LjU2NDEwIDYwNC43NTQ5MCBjbQowIDAgbQowIC01LjAyMDAwIGwKLTAuNDQ4MDAgLTUuNDUxMDAgbAotNS4zNTYwMCAtNS40NTEwMCBsCi01LjM1NjAwIC0wLjQwMDAwIGwKLTQuOTM5MDAgMCBsCmgKLTUuMDE5MDAgLTUuMTAwMDAgNC40MjcwMCA0LjU0NzAwIHJlCmYKUQpxCjEgMCAwIDEgMzcxLjc5NzEwIDYwNC43NTQ5MCBjbQowIDAgbQowIC01LjAyMDAwIGwKLTAuNDQ4MDAgLTUuNDUxMDAgbAotNS4zNTYwMCAtNS40NTEwMCBsCi01LjM1NjAwIC0wLjQwMDAwIGwKLTQuOTM5MDAgMCBsCmgKLTUuMDE5MDAgLTUuMTAwMDAgNC40MjcwMCA0LjU0NzAwIHJlCmYKUQpCVAowLjAwMTAwIDAuMDAyMDAgMC4wMDIwMCBzY24KL0dTMCBncwoxMCAwIDAgMTAgMzQ2LjIyODUwIDU0OC41NTM3MCBUbQooWWVzICAgICAgICBObyAgICAgICkgVGoKRVQKMC4xMzcwMCAwLjEyMzAwIDAuMTI2MDAgc2NuCi9HUzEgZ3MKcQoxIDAgMCAxIDM0NS41NjQxMCA1NTUuMDA0OTAgY20KMCAwIG0KMCAtNS4wMjAwMCBsCi0wLjQ0ODAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTUuNDUxMDAgbAotNS4zNTYwMCAtMC40MDAwMCBsCi00LjkzOTAwIDAgbApoCi01LjAxOTAwIC01LjEwMDAwIDQuNDI3MDAgNC41NDcwMCByZQpmClEKcQoxIDAgMCAxIDM3MS43OTcxMCA1NTUuMDA0OTAgY20KMCAwIG0KMCAtNS4wMjAwMCBsCi0wLjQ0ODAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTUuNDUxMDAgbAotNS4zNTYwMCAtMC40MDAwMCBsCi00LjkzOTAwIDAgbApoCi01LjAxOTAwIC01LjEwMDAwIDQuNDI3MDAgNC41NDcwMCByZQpmClEKQlQKMC4wMDEwMCAwLjAwMjAwIDAuMDAyMDAgc2NuCi9HUzAgZ3MKMTAgMCAwIDEwIDM0Ni4yMjg1MCA1MjQuMzAzNzAgVG0KKFllcyAgICAgICAgTm8gICAgICApIFRqCkVUCjAuMTM3MDAgMC4xMjMwMCAwLjEyNjAwIHNjbgovR1MxIGdzCnEKMSAwIDAgMSAzNDUuNTY0MTAgNTMwLjc1NDkwIGNtCjAgMCBtCjAgLTUuMDIwMDAgbAotMC40NDgwMCAtNS40NTEwMCBsCi01LjM1NjAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTAuNDAwMDAgbAotNC45MzkwMCAwIGwKaAotNS4wMTkwMCAtNS4xMDAwMCA0LjQyNzAwIDQuNTQ3MDAgcmUKZgpRCnEKMSAwIDAgMSAzNzEuNzk3MTAgNTMwLjc1NDkwIGNtCjAgMCBtCjAgLTUuMDIwMDAgbAotMC40NDgwMCAtNS40NTEwMCBsCi01LjM1NjAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTAuNDAwMDAgbAotNC45MzkwMCAwIGwKaAotNS4wMTkwMCAtNS4xMDAwMCA0LjQyNzAwIDQuNTQ3MDAgcmUKZgpRCkJUCjAuMDAxMDAgMC4wMDIwMCAwLjAwMjAwIHNjbgovR1MwIGdzCjEwIDAgMCAxMCAzNDYuMjI4NTAgNTAwLjU1MzcwIFRtCihZZXMgICAgICAgIE5vICAgICAgKSBUagpFVAowLjEzNzAwIDAuMTIzMDAgMC4xMjYwMCBzY24KL0dTMSBncwpxCjEgMCAwIDEgMzQ1LjU2NDEwIDUwNy4wMDQ5MCBjbQowIDAgbQowIC01LjAyMDAwIGwKLTAuNDQ4MDAgLTUuNDUxMDAgbAotNS4zNTYwMCAtNS40NTEwMCBsCi01LjM1NjAwIC0wLjQwMDAwIGwKLTQuOTM5MDAgMCBsCmgKLTUuMDE5MDAgLTUuMTAwMDAgNC40MjcwMCA0LjU0NzAwIHJlCmYKUQpxCjEgMCAwIDEgMzcxLjc5NzEwIDUwNy4wMDQ5MCBjbQowIDAgbQowIC01LjAyMDAwIGwKLTAuNDQ4MDAgLTUuNDUxMDAgbAotNS4zNTYwMCAtNS40NTEwMCBsCi01LjM1NjAwIC0wLjQwMDAwIGwKLTQuOTM5MDAgMCBsCmgKLTUuMDE5MDAgLTUuMTAwMDAgNC40MjcwMCA0LjU0NzAwIHJlCmYKUQpCVAowLjAwMTAwIDAuMDAyMDAgMC4wMDIwMCBzY24KL0dTMCBncwoxMCAwIDAgMTAgMzQ2LjIyODUwIDQ3Ni4zMDM3MCBUbQooWWVzICAgICAgICBObyAgICAgICkgVGoKRVQKMC4xMzcwMCAwLjEyMzAwIDAuMTI2MDAgc2NuCi9HUzEgZ3MKcQoxIDAgMCAxIDM0NS41NjQxMCA0ODIuNzU0OTAgY20KMCAwIG0KMCAtNS4wMjAwMCBsCi0wLjQ0ODAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTUuNDUxMDAgbAotNS4zNTYwMCAtMC40MDAwMCBsCi00LjkzOTAwIDAgbApoCi01LjAxOTAwIC01LjEwMDAwIDQuNDI3MDAgNC41NDcwMCByZQpmClEKcQoxIDAgMCAxIDM3MS43OTcxMCA0ODIuNzU0OTAgY20KMCAwIG0KMCAtNS4wMjAwMCBsCi0wLjQ0ODAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTUuNDUxMDAgbAotNS4zNTYwMCAtMC40MDAwMCBsCi00LjkzOTAwIDAgbApoCi01LjAxOTAwIC01LjEwMDAwIDQuNDI3MDAgNC41NDcwMCByZQpmClEKQlQKMC4wMDEwMCAwLjAwMjAwIDAuMDAyMDAgc2NuCi9HUzAgZ3MKMTAgMCAwIDEwIDM0NS4yMjg1MCA2NjUuNjY1MDAgVG0KKFllcyAgICAgICAgTm8gICAgICApIFRqCkVUCjAuMTM3MDAgMC4xMjMwMCAwLjEyNjAwIHNjbgovR1MxIGdzCnEKMSAwIDAgMSAzNDQuNTY0MTAgNjcyLjExNjcwIGNtCjAgMCBtCjAgLTUuMDIwMDAgbAotMC40NDgwMCAtNS40NTEwMCBsCi01LjM1NjAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTAuNDAwMDAgbAotNC45MzkwMCAwIGwKaAotNS4wMTkwMCAtNS4xMDAwMCA0LjQyNzAwIDQuNTQ3MDAgcmUKZgpRCnEKMSAwIDAgMSAzNzAuNzk3MTAgNjcyLjExNjcwIGNtCjAgMCBtCjAgLTUuMDIwMDAgbAotMC40NDgwMCAtNS40NTEwMCBsCi01LjM1NjAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTAuNDAwMDAgbAotNC45MzkwMCAwIGwKaAotNS4wMTkwMCAtNS4xMDAwMCA0LjQyNzAwIDQuNTQ3MDAgcmUKZgpRCkJUCjAuMDAxMDAgMC4wMDIwMCAwLjAwMjAwIHNjbgovR1MwIGdzCi9UMV8xIDEgVGYKMTAgMCAwIDEwIDM5Ny40NTU2MCA2NzguNTc3MTAgVG0KKCAgICAgICBTcG91c2UpIFRqCi9UMV8wIDEgVGYKMS4yODYwMCAtNi44MjcwMCBUZAooWWVzICAgICAgICBObyAgICAgICkgVGoKRVQKMC4xMzcwMCAwLjEyMzAwIDAuMTI2MDAgc2NuCi9HUzEgZ3MKcQoxIDAgMCAxIDQwOS42NTU4MCA2MTYuNzU0OTAgY20KMCAwIG0KMCAtNS4wMjAwMCBsCi0wLjQ0ODAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTUuNDUxMDAgbAotNS4zNTYwMCAtMC40MDAwMCBsCi00LjkzOTAwIDAgbApoCi01LjAxOTAwIC01LjEwMDAwIDQuNDI3MDAgNC41NDcwMCByZQpmClEKcQoxIDAgMCAxIDQzNS44ODg4MCA2MTYuNzU0OTAgY20KMCAwIG0KMCAtNS4wMjAwMCBsCi0wLjQ0ODAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTUuNDUxMDAgbAotNS4zNTYwMCAtMC40MDAwMCBsCi00LjkzOTAwIDAgbApoCi01LjAxOTAwIC01LjEwMDAwIDQuNDI3MDAgNC41NDcwMCByZQpmClEKQlQKMC4wMDEwMCAwLjAwMjAwIDAuMDAyMDAgc2NuCi9HUzAgZ3MKMTAgMCAwIDEwIDQxMC4zMjAzMCA1OTguMzAzNzAgVG0KKFllcyAgICAgICAgTm8gICAgICApIFRqCkVUCjAuMTM3MDAgMC4xMjMwMCAwLjEyNjAwIHNjbgovR1MxIGdzCnEKMSAwIDAgMSA0MDkuNjU1ODAgNjA0Ljc1NDkwIGNtCjAgMCBtCjAgLTUuMDIwMDAgbAotMC40NDgwMCAtNS40NTEwMCBsCi01LjM1NjAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTAuNDAwMDAgbAotNC45MzkwMCAwIGwKaAotNS4wMTkwMCAtNS4xMDAwMCA0LjQyNzAwIDQuNTQ3MDAgcmUKZgpRCnEKMSAwIDAgMSA0MzUuODg4ODAgNjA0Ljc1NDkwIGNtCjAgMCBtCjAgLTUuMDIwMDAgbAotMC40NDgwMCAtNS40NTEwMCBsCi01LjM1NjAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTAuNDAwMDAgbAotNC45MzkwMCAwIGwKaAotNS4wMTkwMCAtNS4xMDAwMCA0LjQyNzAwIDQuNTQ3MDAgcmUKZgpRCkJUCjAuMDAxMDAgMC4wMDIwMCAwLjAwMjAwIHNjbgovR1MwIGdzCjEwIDAgMCAxMCA0MTAuMzIwMzAgNTQ4LjU1MzcwIFRtCihZZXMgICAgICAgIE5vICAgICAgKSBUagpFVAowLjEzNzAwIDAuMTIzMDAgMC4xMjYwMCBzY24KL0dTMSBncwpxCjEgMCAwIDEgNDA5LjY1NTgwIDU1NS4wMDQ5MCBjbQowIDAgbQowIC01LjAyMDAwIGwKLTAuNDQ4MDAgLTUuNDUxMDAgbAotNS4zNTYwMCAtNS40NTEwMCBsCi01LjM1NjAwIC0wLjQwMDAwIGwKLTQuOTM5MDAgMCBsCmgKLTUuMDE5MDAgLTUuMTAwMDAgNC40MjcwMCA0LjU0NzAwIHJlCmYKUQpxCjEgMCAwIDEgNDM2Ljg4ODgwIDU1NS4wMDQ5MCBjbQowIDAgbQowIC01LjAyMDAwIGwKLTAuNDQ4MDAgLTUuNDUxMDAgbAotNS4zNTYwMCAtNS40NTEwMCBsCi01LjM1NjAwIC0wLjQwMDAwIGwKLTQuOTM5MDAgMCBsCmgKLTUuMDE5MDAgLTUuMTAwMDAgNC40MjcwMCA0LjU0NzAwIHJlCmYKUQpCVAowLjAwMTAwIDAuMDAyMDAgMC4wMDIwMCBzY24KL0dTMCBncwoxMCAwIDAgMTAgNDEwLjMyMDMwIDUyNC4zMDM3MCBUbQooWWVzICAgICAgICBObyAgICAgICkgVGoKRVQKMC4xMzcwMCAwLjEyMzAwIDAuMTI2MDAgc2NuCi9HUzEgZ3MKcQoxIDAgMCAxIDQwOS42NTU4MCA1MzAuNzU0OTAgY20KMCAwIG0KMCAtNS4wMjAwMCBsCi0wLjQ0ODAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTUuNDUxMDAgbAotNS4zNTYwMCAtMC40MDAwMCBsCi00LjkzOTAwIDAgbApoCi01LjAxOTAwIC01LjEwMDAwIDQuNDI3MDAgNC41NDcwMCByZQpmClEKcQoxIDAgMCAxIDQzNS44ODg4MCA1MzAuNzU0OTAgY20KMCAwIG0KMCAtNS4wMjAwMCBsCi0wLjQ0ODAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTUuNDUxMDAgbAotNS4zNTYwMCAtMC40MDAwMCBsCi00LjkzOTAwIDAgbApoCi01LjAxOTAwIC01LjEwMDAwIDQuNDI3MDAgNC41NDcwMCByZQpmClEKQlQKMC4wMDEwMCAwLjAwMjAwIDAuMDAyMDAgc2NuCi9HUzAgZ3MKMTAgMCAwIDEwIDQxMC4zMjAzMCA1MDAuNTUzNzAgVG0KKFllcyAgICAgICAgTm8gICAgICApIFRqCkVUCjAuMTM3MDAgMC4xMjMwMCAwLjEyNjAwIHNjbgovR1MxIGdzCnEKMSAwIDAgMSA0MDkuNjU1ODAgNTA3LjAwNDkwIGNtCjAgMCBtCjAgLTUuMDIwMDAgbAotMC40NDgwMCAtNS40NTEwMCBsCi01LjM1NjAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTAuNDAwMDAgbAotNC45MzkwMCAwIGwKaAotNS4wMTkwMCAtNS4xMDAwMCA0LjQyNzAwIDQuNTQ3MDAgcmUKZgpRCnEKMSAwIDAgMSA0MzUuODg4ODAgNTA3LjAwNDkwIGNtCjAgMCBtCjAgLTUuMDIwMDAgbAotMC40NDgwMCAtNS40NTEwMCBsCi01LjM1NjAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTAuNDAwMDAgbAotNC45MzkwMCAwIGwKaAotNS4wMTkwMCAtNS4xMDAwMCA0LjQyNzAwIDQuNTQ3MDAgcmUKZgpRCkJUCjAuMDAxMDAgMC4wMDIwMCAwLjAwMjAwIHNjbgovR1MwIGdzCjEwIDAgMCAxMCA0MTAuMzIwMzAgNDc2LjMwMzcwIFRtCihZZXMgICAgICAgIE5vICAgICAgKSBUagpFVAowLjEzNzAwIDAuMTIzMDAgMC4xMjYwMCBzY24KL0dTMSBncwpxCjEgMCAwIDEgNDA5LjY1NTgwIDQ4Mi43NTQ5MCBjbQowIDAgbQowIC01LjAyMDAwIGwKLTAuNDQ4MDAgLTUuNDUxMDAgbAotNS4zNTYwMCAtNS40NTEwMCBsCi01LjM1NjAwIC0wLjQwMDAwIGwKLTQuOTM5MDAgMCBsCmgKLTUuMDE5MDAgLTUuMTAwMDAgNC40MjcwMCA0LjU0NzAwIHJlCmYKUQpxCjEgMCAwIDEgNDM1Ljg4ODgwIDQ4Mi43NTQ5MCBjbQowIDAgbQowIC01LjAyMDAwIGwKLTAuNDQ4MDAgLTUuNDUxMDAgbAotNS4zNTYwMCAtNS40NTEwMCBsCi01LjM1NjAwIC0wLjQwMDAwIGwKLTQuOTM5MDAgMCBsCmgKLTUuMDE5MDAgLTUuMTAwMDAgNC40MjcwMCA0LjU0NzAwIHJlCmYKUQpCVAowLjAwMTAwIDAuMDAyMDAgMC4wMDIwMCBzY24KL0dTMCBncwoxMCAwIDAgMTAgNDA5LjMyMDMwIDY2NS42NjUwMCBUbQooWWVzICAgICAgICBObyAgICAgICkgVGoKRVQKMC4xMzcwMCAwLjEyMzAwIDAuMTI2MDAgc2NuCi9HUzEgZ3MKcQoxIDAgMCAxIDQwOC42NTU4MCA2NzIuMTE2NzAgY20KMCAwIG0KMCAtNS4wMjAwMCBsCi0wLjQ0ODAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTUuNDUxMDAgbAotNS4zNTYwMCAtMC40MDAwMCBsCi00LjkzOTAwIDAgbApoCi01LjAxOTAwIC01LjEwMDAwIDQuNDI3MDAgNC41NDcwMCByZQpmClEKcQoxIDAgMCAxIDQzNC44ODg4MCA2NzIuMTE2NzAgY20KMCAwIG0KMCAtNS4wMjAwMCBsCi0wLjQ0ODAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTUuNDUxMDAgbAotNS4zNTYwMCAtMC40MDAwMCBsCi00LjkzOTAwIDAgbApoCi01LjAxOTAwIC01LjEwMDAwIDQuNDI3MDAgNC41NDcwMCByZQpmClEKQlQKMC4wMDEwMCAwLjAwMjAwIDAuMDAyMDAgc2NuCi9HUzAgZ3MKL1QxXzEgMSBUZgoxMCAwIDAgMTAgNDYwLjc4NTYwIDY3OC43MDcwMCBUbQooICAgICAgICBDaGlsZCAxKSBUagovVDFfMCAxIFRmCjEuMzcwMDAgLTYuODQwMDAgVGQKKFllcyAgICAgICAgTm8gICAgICApIFRqCkVUCjAuMTM3MDAgMC4xMjMwMCAwLjEyNjAwIHNjbgovR1MxIGdzCnEKMSAwIDAgMSA0NzMuODIxMDAgNjE2Ljc1NDkwIGNtCjAgMCBtCjAgLTUuMDIwMDAgbAotMC40NDgwMCAtNS40NTEwMCBsCi01LjM1NjAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTAuNDAwMDAgbAotNC45MzkwMCAwIGwKaAotNS4wMTkwMCAtNS4xMDAwMCA0LjQyNzAwIDQuNTQ3MDAgcmUKZgpRCnEKMSAwIDAgMSA1MDAuMDU0MDAgNjE2Ljc1NDkwIGNtCjAgMCBtCjAgLTUuMDIwMDAgbAotMC40NDgwMCAtNS40NTEwMCBsCi01LjM1NjAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTAuNDAwMDAgbAotNC45MzkwMCAwIGwKaAotNS4wMTkwMCAtNS4xMDAwMCA0LjQyNzAwIDQuNTQ3MDAgcmUKZgpRCkJUCjAuMDAxMDAgMC4wMDIwMCAwLjAwMjAwIHNjbgovR1MwIGdzCjEwIDAgMCAxMCA0NzQuNDg1ODAgNTk4LjMwMzcwIFRtCihZZXMgICAgICAgIE5vICAgICAgKSBUagpFVAowLjEzNzAwIDAuMTIzMDAgMC4xMjYwMCBzY24KL0dTMSBncwpxCjEgMCAwIDEgNDczLjgyMTAwIDYwNC43NTQ5MCBjbQowIDAgbQowIC01LjAyMDAwIGwKLTAuNDQ4MDAgLTUuNDUxMDAgbAotNS4zNTYwMCAtNS40NTEwMCBsCi01LjM1NjAwIC0wLjQwMDAwIGwKLTQuOTM5MDAgMCBsCmgKLTUuMDE5MDAgLTUuMTAwMDAgNC40MjcwMCA0LjU0NzAwIHJlCmYKUQpxCjEgMCAwIDEgNTAwLjA1NDAwIDYwNC43NTQ5MCBjbQowIDAgbQowIC01LjAyMDAwIGwKLTAuNDQ4MDAgLTUuNDUxMDAgbAotNS4zNTYwMCAtNS40NTEwMCBsCi01LjM1NjAwIC0wLjQwMDAwIGwKLTQuOTM5MDAgMCBsCmgKLTUuMDE5MDAgLTUuMTAwMDAgNC40MjcwMCA0LjU0NzAwIHJlCmYKUQpCVAowLjAwMTAwIDAuMDAyMDAgMC4wMDIwMCBzY24KL0dTMCBncwoxMCAwIDAgMTAgNDc0LjQ4NTgwIDU0OC41NTM3MCBUbQooWWVzICAgICAgICBObyAgICAgICkgVGoKRVQKMC4xMzcwMCAwLjEyMzAwIDAuMTI2MDAgc2NuCi9HUzEgZ3MKcQoxIDAgMCAxIDQ3My44MjEwMCA1NTUuMDA0OTAgY20KMCAwIG0KMCAtNS4wMjAwMCBsCi0wLjQ0ODAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTUuNDUxMDAgbAotNS4zNTYwMCAtMC40MDAwMCBsCi00LjkzOTAwIDAgbApoCi01LjAxOTAwIC01LjEwMDAwIDQuNDI3MDAgNC41NDcwMCByZQpmClEKcQoxIDAgMCAxIDUwMC4wNTQwMCA1NTUuMDA0OTAgY20KMCAwIG0KMCAtNS4wMjAwMCBsCi0wLjQ0ODAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTUuNDUxMDAgbAotNS4zNTYwMCAtMC40MDAwMCBsCi00LjkzOTAwIDAgbApoCi01LjAxOTAwIC01LjEwMDAwIDQuNDI3MDAgNC41NDcwMCByZQpmClEKQlQKMC4wMDEwMCAwLjAwMjAwIDAuMDAyMDAgc2NuCi9HUzAgZ3MKMTAgMCAwIDEwIDQ3NC40ODU4MCA1MjQuMzAzNzAgVG0KKFllcyAgICAgICAgTm8gICAgICApIFRqCkVUCjAuMTM3MDAgMC4xMjMwMCAwLjEyNjAwIHNjbgovR1MxIGdzCnEKMSAwIDAgMSA0NzMuODIxMDAgNTMwLjc1NDkwIGNtCjAgMCBtCjAgLTUuMDIwMDAgbAotMC40NDgwMCAtNS40NTEwMCBsCi01LjM1NjAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTAuNDAwMDAgbAotNC45MzkwMCAwIGwKaAotNS4wMTkwMCAtNS4xMDAwMCA0LjQyNzAwIDQuNTQ3MDAgcmUKZgpRCnEKMSAwIDAgMSA1MDAuMDU0MDAgNTMwLjc1NDkwIGNtCjAgMCBtCjAgLTUuMDIwMDAgbAotMC40NDgwMCAtNS40NTEwMCBsCi01LjM1NjAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTAuNDAwMDAgbAotNC45MzkwMCAwIGwKaAotNS4wMTkwMCAtNS4xMDAwMCA0LjQyNzAwIDQuNTQ3MDAgcmUKZgpRCkJUCjAuMDAxMDAgMC4wMDIwMCAwLjAwMjAwIHNjbgovR1MwIGdzCjEwIDAgMCAxMCA0NzQuNDg1ODAgNTAwLjU1MzcwIFRtCihZZXMgICAgICAgIE5vICAgICAgKSBUagpFVAowLjEzNzAwIDAuMTIzMDAgMC4xMjYwMCBzY24KL0dTMSBncwpxCjEgMCAwIDEgNDczLjgyMTAwIDUwNy4wMDQ5MCBjbQowIDAgbQowIC01LjAyMDAwIGwKLTAuNDQ4MDAgLTUuNDUxMDAgbAotNS4zNTYwMCAtNS40NTEwMCBsCi01LjM1NjAwIC0wLjQwMDAwIGwKLTQuOTM5MDAgMCBsCmgKLTUuMDE5MDAgLTUuMTAwMDAgNC40MjcwMCA0LjU0NzAwIHJlCmYKUQpxCjEgMCAwIDEgNTAwLjA1NDAwIDUwNy4wMDQ5MCBjbQowIDAgbQowIC01LjAyMDAwIGwKLTAuNDQ4MDAgLTUuNDUxMDAgbAotNS4zNTYwMCAtNS40NTEwMCBsCi01LjM1NjAwIC0wLjQwMDAwIGwKLTQuOTM5MDAgMCBsCmgKLTUuMDE5MDAgLTUuMTAwMDAgNC40MjcwMCA0LjU0NzAwIHJlCmYKUQpCVAowLjAwMTAwIDAuMDAyMDAgMC4wMDIwMCBzY24KL0dTMCBncwoxMCAwIDAgMTAgNDc0LjQ4NTgwIDQ3Ni4zMDM3MCBUbQooWWVzICAgICAgICBObyAgICAgICkgVGoKRVQKMC4xMzcwMCAwLjEyMzAwIDAuMTI2MDAgc2NuCi9HUzEgZ3MKcQoxIDAgMCAxIDQ3My44MjEwMCA0ODIuNzU0OTAgY20KMCAwIG0KMCAtNS4wMjAwMCBsCi0wLjQ0ODAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTUuNDUxMDAgbAotNS4zNTYwMCAtMC40MDAwMCBsCi00LjkzOTAwIDAgbApoCi01LjAxOTAwIC01LjEwMDAwIDQuNDI3MDAgNC41NDcwMCByZQpmClEKcQoxIDAgMCAxIDUwMC4wNTQwMCA0ODIuNzU0OTAgY20KMCAwIG0KMCAtNS4wMjAwMCBsCi0wLjQ0ODAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTUuNDUxMDAgbAotNS4zNTYwMCAtMC40MDAwMCBsCi00LjkzOTAwIDAgbApoCi01LjAxOTAwIC01LjEwMDAwIDQuNDI3MDAgNC41NDcwMCByZQpmClEKQlQKMC4wMDEwMCAwLjAwMjAwIDAuMDAyMDAgc2NuCi9HUzAgZ3MKMTAgMCAwIDEwIDQ3My40ODU4MCA2NjUuNjY1MDAgVG0KKFllcyAgICAgICAgTm8gICAgICApIFRqCkVUCjAuMTM3MDAgMC4xMjMwMCAwLjEyNjAwIHNjbgovR1MxIGdzCnEKMSAwIDAgMSA0NzIuODIxMDAgNjcyLjExNjcwIGNtCjAgMCBtCjAgLTUuMDIwMDAgbAotMC40NDgwMCAtNS40NTEwMCBsCi01LjM1NjAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTAuNDAwMDAgbAotNC45MzkwMCAwIGwKaAotNS4wMTkwMCAtNS4xMDAwMCA0LjQyNzAwIDQuNTQ3MDAgcmUKZgpRCnEKMSAwIDAgMSA0OTkuMDU0MDAgNjcyLjExNjcwIGNtCjAgMCBtCjAgLTUuMDIwMDAgbAotMC40NDgwMCAtNS40NTEwMCBsCi01LjM1NjAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTAuNDAwMDAgbAotNC45MzkwMCAwIGwKaAotNS4wMTkwMCAtNS4xMDAwMCA0LjQyNzAwIDQuNTQ3MDAgcmUKZgpRCkJUCjAuMDAxMDAgMC4wMDIwMCAwLjAwMjAwIHNjbgovR1MwIGdzCi9UMV8xIDEgVGYKMTAgMCAwIDEwIDUyNS43ODUyMCA2NzkuNDM4NTAgVG0KKCAgICAgICBDaGlsZCAyKSBUagovVDFfMCAxIFRmCjEuMjg3MDAgLTYuOTEzMDAgVGQKKFllcyAgICAgICAgTm8gICAgICApIFRqCkVUCjAuMTM3MDAgMC4xMjMwMCAwLjEyNjAwIHNjbgovR1MxIGdzCnEKMSAwIDAgMSA1MzcuOTg1NzAgNjE2Ljc1NDkwIGNtCjAgMCBtCjAgLTUuMDIwMDAgbAotMC40NDgwMCAtNS40NTEwMCBsCi01LjM1NjAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTAuNDAwMDAgbAotNC45MzkwMCAwIGwKaAotNS4wMTkwMCAtNS4xMDAwMCA0LjQyNzAwIDQuNTQ3MDAgcmUKZgpRCnEKMSAwIDAgMSA1NjQuMjE4NzAgNjE2Ljc1NDkwIGNtCjAgMCBtCjAgLTUuMDIwMDAgbAotMC40NDgwMCAtNS40NTEwMCBsCi01LjM1NjAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTAuNDAwMDAgbAotNC45MzkwMCAwIGwKaAotNS4wMTkwMCAtNS4xMDAwMCA0LjQyNzAwIDQuNTQ3MDAgcmUKZgpRCkJUCjAuMDAxMDAgMC4wMDIwMCAwLjAwMjAwIHNjbgovR1MwIGdzCjEwIDAgMCAxMCA1MzguNjUwNDAgNTk4LjMwMzcwIFRtCihZZXMgICAgICAgIE5vICAgICAgKSBUagpFVAowLjEzNzAwIDAuMTIzMDAgMC4xMjYwMCBzY24KL0dTMSBncwpxCjEgMCAwIDEgNTM3Ljk4NTcwIDYwNC43NTQ5MCBjbQowIDAgbQowIC01LjAyMDAwIGwKLTAuNDQ4MDAgLTUuNDUxMDAgbAotNS4zNTYwMCAtNS40NTEwMCBsCi01LjM1NjAwIC0wLjQwMDAwIGwKLTQuOTM5MDAgMCBsCmgKLTUuMDE5MDAgLTUuMTAwMDAgNC40MjcwMCA0LjU0NzAwIHJlCmYKUQpxCjEgMCAwIDEgNTY0LjIxODcwIDYwNC43NTQ5MCBjbQowIDAgbQowIC01LjAyMDAwIGwKLTAuNDQ4MDAgLTUuNDUxMDAgbAotNS4zNTYwMCAtNS40NTEwMCBsCi01LjM1NjAwIC0wLjQwMDAwIGwKLTQuOTM5MDAgMCBsCmgKLTUuMDE5MDAgLTUuMTAwMDAgNC40MjcwMCA0LjU0NzAwIHJlCmYKUQpCVAowLjAwMTAwIDAuMDAyMDAgMC4wMDIwMCBzY24KL0dTMCBncwoxMCAwIDAgMTAgNTM4LjY1MDQwIDU0OC41NTM3MCBUbQooWWVzICAgICAgICBObyAgICAgICkgVGoKRVQKMC4xMzcwMCAwLjEyMzAwIDAuMTI2MDAgc2NuCi9HUzEgZ3MKcQoxIDAgMCAxIDUzNy45ODU3MCA1NTUuMDA0OTAgY20KMCAwIG0KMCAtNS4wMjAwMCBsCi0wLjQ0ODAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTUuNDUxMDAgbAotNS4zNTYwMCAtMC40MDAwMCBsCi00LjkzOTAwIDAgbApoCi01LjAxOTAwIC01LjEwMDAwIDQuNDI3MDAgNC41NDcwMCByZQpmClEKcQoxIDAgMCAxIDU2NC4yMTg3MCA1NTUuMDA0OTAgY20KMCAwIG0KMCAtNS4wMjAwMCBsCi0wLjQ0ODAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTUuNDUxMDAgbAotNS4zNTYwMCAtMC40MDAwMCBsCi00LjkzOTAwIDAgbApoCi01LjAxOTAwIC01LjEwMDAwIDQuNDI3MDAgNC41NDcwMCByZQpmClEKQlQKMC4wMDEwMCAwLjAwMjAwIDAuMDAyMDAgc2NuCi9HUzAgZ3MKMTAgMCAwIDEwIDUzOC42NTA0MCA1MjQuMzAzNzAgVG0KKFllcyAgICAgICAgTm8gICAgICApIFRqCkVUCjAuMTM3MDAgMC4xMjMwMCAwLjEyNjAwIHNjbgovR1MxIGdzCnEKMSAwIDAgMSA1MzcuOTg1NzAgNTMwLjc1NDkwIGNtCjAgMCBtCjAgLTUuMDIwMDAgbAotMC40NDgwMCAtNS40NTEwMCBsCi01LjM1NjAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTAuNDAwMDAgbAotNC45MzkwMCAwIGwKaAotNS4wMTkwMCAtNS4xMDAwMCA0LjQyNzAwIDQuNTQ3MDAgcmUKZgpRCnEKMSAwIDAgMSA1NjQuMjE4NzAgNTMwLjc1NDkwIGNtCjAgMCBtCjAgLTUuMDIwMDAgbAotMC40NDgwMCAtNS40NTEwMCBsCi01LjM1NjAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTAuNDAwMDAgbAotNC45MzkwMCAwIGwKaAotNS4wMTkwMCAtNS4xMDAwMCA0LjQyNzAwIDQuNTQ3MDAgcmUKZgpRCkJUCjAuMDAxMDAgMC4wMDIwMCAwLjAwMjAwIHNjbgovR1MwIGdzCjEwIDAgMCAxMCA1MzguNjUwNDAgNTAwLjU1MzcwIFRtCihZZXMgICAgICAgIE5vICAgICAgKSBUagpFVAowLjEzNzAwIDAuMTIzMDAgMC4xMjYwMCBzY24KL0dTMSBncwpxCjEgMCAwIDEgNTM3Ljk4NTcwIDUwNy4wMDQ5MCBjbQowIDAgbQowIC01LjAyMDAwIGwKLTAuNDQ4MDAgLTUuNDUxMDAgbAotNS4zNTYwMCAtNS40NTEwMCBsCi01LjM1NjAwIC0wLjQwMDAwIGwKLTQuOTM5MDAgMCBsCmgKLTUuMDE5MDAgLTUuMTAwMDAgNC40MjcwMCA0LjU0NzAwIHJlCmYKUQpxCjEgMCAwIDEgNTY0LjIxODcwIDUwNy4wMDQ5MCBjbQowIDAgbQowIC01LjAyMDAwIGwKLTAuNDQ4MDAgLTUuNDUxMDAgbAotNS4zNTYwMCAtNS40NTEwMCBsCi01LjM1NjAwIC0wLjQwMDAwIGwKLTQuOTM5MDAgMCBsCmgKLTUuMDE5MDAgLTUuMTAwMDAgNC40MjcwMCA0LjU0NzAwIHJlCmYKUQpCVAowLjAwMTAwIDAuMDAyMDAgMC4wMDIwMCBzY24KL0dTMCBncwoxMCAwIDAgMTAgNTM4LjY1MDQwIDQ3Ni4zMDM3MCBUbQooWWVzICAgICAgICBObyAgICAgICkgVGoKRVQKMC4xMzcwMCAwLjEyMzAwIDAuMTI2MDAgc2NuCi9HUzEgZ3MKcQoxIDAgMCAxIDUzNy45ODU3MCA0ODIuNzU0OTAgY20KMCAwIG0KMCAtNS4wMjAwMCBsCi0wLjQ0ODAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTUuNDUxMDAgbAotNS4zNTYwMCAtMC40MDAwMCBsCi00LjkzOTAwIDAgbApoCi01LjAxOTAwIC01LjEwMDAwIDQuNDI3MDAgNC41NDcwMCByZQpmClEKcQoxIDAgMCAxIDU2NC4yMTg3MCA0ODIuNzU0OTAgY20KMCAwIG0KMCAtNS4wMjAwMCBsCi0wLjQ0ODAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTUuNDUxMDAgbAotNS4zNTYwMCAtMC40MDAwMCBsCi00LjkzOTAwIDAgbApoCi01LjAxOTAwIC01LjEwMDAwIDQuNDI3MDAgNC41NDcwMCByZQpmClEKQlQKMC4wMDEwMCAwLjAwMjAwIDAuMDAyMDAgc2NuCi9HUzAgZ3MKMTAgMCAwIDEwIDUzNy42NTA0MCA2NjUuNjY1MDAgVG0KKFllcyAgICAgICAgTm8gICAgICApIFRqCkVUCjAuMTM3MDAgMC4xMjMwMCAwLjEyNjAwIHNjbgovR1MxIGdzCnEKMSAwIDAgMSA1MzYuOTg1NzAgNjcyLjExNjcwIGNtCjAgMCBtCjAgLTUuMDIwMDAgbAotMC40NDgwMCAtNS40NTEwMCBsCi01LjM1NjAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTAuNDAwMDAgbAotNC45MzkwMCAwIGwKaAotNS4wMTkwMCAtNS4xMDAwMCA0LjQyNzAwIDQuNTQ3MDAgcmUKZgpRCnEKMSAwIDAgMSA1NjMuMjE4NzAgNjcyLjExNjcwIGNtCjAgMCBtCjAgLTUuMDIwMDAgbAotMC40NDgwMCAtNS40NTEwMCBsCi01LjM1NjAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTAuNDAwMDAgbAotNC45MzkwMCAwIGwKaAotNS4wMTkwMCAtNS4xMDAwMCA0LjQyNzAwIDQuNTQ3MDAgcmUKZgpRCnEKMSAwIDAgMSAzMTUuOTk1NjAgMTEyLjk2MDYwIGNtCjAgMCBtCjAgLTUuMDIwMDAgbAotMC40NDgwMCAtNS40NTEwMCBsCi01LjM1NjAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTAuNDAwMDAgbAotNC45MzkwMCAwIGwKaAotNS4wMTkwMCAtNS4xMDAwMCA0LjQyNzAwIDQuNTQ3MDAgcmUKZgpRCnEKMSAwIDAgMSAzNDAuMjA1ODAgMTEyLjk2MDYwIGNtCjAgMCBtCjAgLTUuMDIwMDAgbAotMC40NDgwMCAtNS40NTEwMCBsCi01LjM1NjAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTAuNDAwMDAgbAotNC45MzkwMCAwIGwKaAotNS4wMTkwMCAtNS4xMDAwMCA0LjQyNzAwIDQuNTQ3MDAgcmUKZgpRCkJUCjAuMDAxMDAgMC4wMDIwMCAwLjAwMjAwIHNjbgovR1MwIGdzCjggMCAwIDggNDAuNTY2NDAgMjMuOTg3ODAgVG0KKFVuZGVyd3JpdHRlbiApIFRqCihiKSBUago1LjQxNDAwIDAgVGQKKHkgNVN0YSkgVGoKKHIpIFRqCjIuNjQ2MDAgMCBUZAooIExpKSBUagooZikgVGoKMS4wMjYwMCAwIFRkCihlIEluc3UpIFRqCihyKSBUagoyLjQxNjAwIDAgVGQKKGFuY2UgQ29tcGEpIFRqCihuKSBUago0LjkzNzAwIDAgVGQKKHkgXDA1MGEgQmF0b24gUm91Z2VcMDU0IExvdWlzaWFuYSBDb21wYSkgVGoKKG4pIFRqCjEzLjA2NTAwIDAgVGQKKHlcMDUxICkgVGoKNyAwIDAgNyAyODMuNjQxMTAgMjMuOTg3ODAgVG0KPDdmPiBUago4IDAgMCA4IDI4Ni40ODk3MCAyMy45ODc4MCBUbQooIE5vdCBhdmFpbGFibGUgaW4gYWxsIHN0YXRlcyApIFRqCjcgMCAwIDcgMzYwLjkwMzgwIDIzLjk4NzgwIFRtCjw3Zj4gVGoKOCAwIDAgOCAzNjMuNzUyOTAgMjMuOTg3ODAgVG0KKCApIFRqCihBKSBUagowLjY0OTAwIDAgVGQKKGRtaW4gT2ZmaWNlXDA3MiA3NzcgUmVzZWEpIFRqCihyKSBUago4LjY5MDAwIDAgVGQKKGNoIEQpIFRqCihyKSBUagoxLjgzNTAwIDAgVGQKKFwwNTZcMDU0IExpbmNvbG5cMDU0IE5FIDY4NTIxICkgVGoKNyAwIDAgNyA1MTEuNjI5NDAgMjMuOTg3ODAgVG0KPDdmPiBUago4IDAgMCA4IDUxNC40NzkwMCAyMy45ODc4MCBUbQooIDFcMDU1ODY2XDA1NTg2M1wwNTU5NzUzKSBUagovVDFfMiAxIFRmCjcgMCAwIDcgMjAuOTUwMjAgOC40NzA3MCBUbQooSUNDMTQgRlBQXDA1NUFwcCBSMTExNCkgVGoKNzguMjMyMDAgMCBUZAooICAgICAxXDA1NzE1KSBUagotMzkuMDczMDAgMCBUZAooUGFnZSAyKSBUagpFVAovQ1MxIGNzCjEgc2NuCjE2LjkwNjAwIDQ1My4xOTMwMCA1NzguMTg3MDAgMTMuODAwMDAgcmUKZioKQlQKL0NTMCBjcwoxIDEgMSBzY24KL1QxXzEgMSBUZgo5LjUwMDAwIDAgMCA5LjUwMDAwIDIwLjk0OTIwIDQ1Ni4wOTI4MCBUbQooU2VjdGlvbiA3IFwwNTUgQ29uZGl0aW9ucyBSZWxhdGluZyB0byB0aGlzIEFwcGxpY2F0aW9uICkgVGoKMC4xNTkwMCAwLjMzOTAwIDAuNjQ2MDAgc2NuCjAgLTEuNDQ5MDAgVGQKKFJlcHJlc2VudGF0aW9ucykgVGoKMC4wMDEwMCAwLjAwMjAwIDAuMDAyMDAgc2NuCi9UMV8wIDEgVGYKLTAuMDEwMDAgVGMKMTAgMCAwIDEwIDIwLjk0ODcwIDQzMC4zMjY3MCBUbQooSSByZXByZXNlbnQgdG8gdGhlIGJlc3Qgb2YgbXkga25vd2xlZGdlIGFuZCBiZWxpZWYgdGhhdCBhbGwgc3RhdGVtZW50cyBhbmQgYW5zd2VycyBpbiB0aGlzIGFwcGxpY2F0aW9uIGFyZSBjb21wbGV0ZVwwNTQgdHJ1ZSBhbmQgYykgVGoKKG9ycmVjdGx5IHJlY29yZGVkXDA1NCBhbmQgYXJlIG1hZGUgYXMgKSBUagpUKgooYSBjb25zaWRlcmF0aW9uIGZvciB0aGUgYXBwbGllZCBmb3IgaW5zdXJhbmNlIFwwNTYgSSB1bmRlcnN0YW5kIHRoYXQgNVN0YXIgTGlmZSBJbnN1cmFuY2UgQ29tcGFueSBcMDUwNVN0YXIgTGlmZVwwNTEgd2lsbCByZWx5IG9uIG15IHN0YSkgVGoKKHRlbWVudHMgYW5kIGFuc3dlcnMgYXMgYmVpbmcgdHJ1ZSApIFRqCjAgLTEuMjAwMDAgVEQKKGFuZCBjb21wbGV0ZSBpbiBkZWNpZGluZyB3aGV0aGVyIHRvIGlzc3VlIGluc3VyYW5jZSBvbiB0aGUgcHJvcG9zZWQgaW5zdXJlZFwwNTBzXDA1MVwwNTYgNVN0YXIgTGlmZSBtYXkgcmVzY2luZCB0aGUgcG9saWN5IGluIGFjY29yZGFuY2UpIFRqCiggd2l0aCB0aGUgQ29udGVzdGFiaWxpdHkgcHJvdmlzaW9uICkgVGoKMCAtMS4yMDAwMCBURAoob2YgdGhlIFBvbGljeSBkdWUgdG8gYW55IG1hdGVyaWFsIG1pc3JlcHJlc2VudGF0aW9uIG9mIGZhY3QgbWFkZSBpbiB0aGlzIGFwcGxpY2F0aW9uXDA1NiBJbnN1cmFuY2UgaXMgZWZmZWN0aXZlIHVuZGVyIHRoZSBwb2xpY3kgbykgVGoKKG5seSB3aGVuIGl0IGlzIGRlbGl2ZXJlZCB0byB0aGUgb3duZXJcMDU0ICkgVGoKVCoKKGFuZCB0aGVuIG9ubHkgaWYgdGhlIGZ1bGwgZmlyc3QgcHJlbWl1bSBpcyBwYWlkIGFuZCBhbGwgb2YgdGhlIHN0YXRlbWVudHMgaW4gdGhpcyBhcHBsaWNhdGlvbiByZW1haW4gY29ycmVjdCBhbmQgY29tcGxldGVcMDU2KSBUagowLjE1OTAwIDAuMzM5MDAgMC42NDYwMCBzY24KL1QxXzEgMSBUZgowIFRjCjkuNTAwMDAgMCAwIDkuNTAwMDAgMjAuOTQ4NzAgMzY0LjMyNzEwIFRtCihBdXRob3JpemF0aW9uKSBUagowLjAwMTAwIDAuMDAyMDAgMC4wMDIwMCBzY24KL1QxXzAgMSBUZgotMC4wMTAwMCBUYwoxMCAwIDAgMTAgMjAuOTQ4NzAgMzUyLjMyNzEwIFRtCihJIGF1dGhvcml6ZSA1U3RhciBMaWZlIHRvIGNvbGxlY3QgbWVkaWNhbCBpbmZvcm1hdGlvbiBvciBpbnZlc3RpZ2F0aW9uIHJlcG9ydHMgYWJvdXQgcHJvcG9zZWQgaW5zdXJlZHMgbmFtZWQgaW4gdGhpcyBhcHBsaWNhdGlvKSBUagooblwwNTYgSSBhdXRob3JpemUgdGhvc2Ugd2l0aCBzdWNoICkgVGoKVCoKKGluZm9ybWF0aW9uIG9yIHJlcG9ydHMgdG8gcmVsZWFzZSB0aGVtIHRvIDVTdGFyIExpZmVcMDU2ICBJIGdpdmUgNVN0YXIgTGlmZSBwZXJtaXNzaW9uIHRvIHNlbmQgc3VjaCBpbmZvcm1hdGlvbiBvciByZXBvcnRzIHRvIE1JQlwwNTQpIFRqCiggSW5jXDA1NiBcMDUwXDIyM01JQlwyMjRcMDUxXDA1NCByZWluc3VyZXJzXDA1NCB0aGUgKSBUagpUKgooSW5zdXJhbmNlIFJlcHJlc2VudGF0aXZlIHdobyBzb2xpY2l0ZWQgdGhlIGFwcGxpY2F0aW9uXDA1NCBhbmQgYW55IHRoaXJkIHBhcnRpZXMgd2hvIGFkbWluaXN0ZXIgdGhlIHBvbGljaWVzIGlzc3VlZCBieSA1U3RhciBMaWZlXDA1NikgVGoKKCBJIGF1dGhvcml6ZSA1U3RhciBMaWZlXDA1NCBvciBpdHMgKSBUagpUKgoocmVpbnN1cmVyc1wwNTQgdG8gbWFrZSBhIGJyaWVmIHJlcG9ydCBvZiBoZWFsdGggaW5mb3JtYXRpb24gdG8gTUlCXDA1NiAgVGhpcyBhdXRob3JpemF0aW9uIHNoYWxsIHJlbWFpbiBpbiBlZmZlY3QgZm9yIHRoZSB0aW1lIGxpbWl0XDA1NCkgVGoKKCBpZiBhbnlcMDU0IHBlcm1pdHRlZCBieSBhcHBsaWNhYmxlIGxhdyBpbikgVGoKVCoKKHRoZSBzdGF0ZSB3aGVyZSB0aGUgcG9saWN5IGlzIGRlbGl2ZXJlZCBvciBpc3N1ZWQgZm9yIGRlbGl2ZXJ5XDA1NCBidXQgaW4gbm8gZXZlbnQgbW9yZSB0aGFuIDMwIG1vbnRocyBmcm9tIHRoZSBkYXRlIEkgc2lnbiBiZWxvd1wwNTYpIFRqCjAuMTU5MDAgMC4zMzkwMCAwLjY0NjAwIHNjbgovVDFfMSAxIFRmCjAgVGMKOS41MDAwMCAwIDAgOS41MDAwMCAyMC45NDg3MCAyODYuMzI3MTAgVG0KKEFja25vd2xlZGdtZW50cykgVGoKMC4wMDEwMCAwLjAwMjAwIDAuMDAyMDAgc2NuCi9UMV8wIDEgVGYKLTAuMDEwMDAgVGMKMTAgMCAwIDEwIDIwLjk0ODcwIDI3NC4zMjcxMCBUbQooSSBhY2tub3dsZWRnZSB0aGF0IEkgaGF2ZSByZWNlaXZlZCBvciB3aWxsIHJlY2VpdmUgXDA1MGluIHRoZSBjYXNlIG9mIHNvbGljaXRhdGlvbiBieSBkaXJlY3QgcmVzcG9uc2UgbWV0aG9kc1wwNTEgdGhlIEFjY2VsZXJhdGVkIEJlbikgVGoKKGVmaXQgRGlzY2xvc3VyZSBmb3JtXDA1MHNcMDUxXDA1NiAgKSBUagovVDFfMSAxIFRmCi0wLjAyNTAwIFRjCjAuMDI1MDAgVHcKMCAtMi4zMDAwMCBURAooTm90ZVwwNzIpIFRqCi9UMV8wIDEgVGYKMCBUdwooIEFueSBwZXJzb24gd2hvIGtub3dpbmdseSBwcmVzZW50cyBhIGZhbHNlIHN0YXRlbWVudCBpbiBhbiBhcHBsaWNhdGlvbiBmb3IgaW5zdXJhbmNlIG1heSBiZSBndWlsdHkgb2YgYSBjcmltaW5hbCBvZmZlbnNlIGFuZCBzdSkgVGoKKGJqZWN0IHRvIHBlbmFsdGllcyB1bmRlciBzdGF0ZSBsYXdcMDU2ICkgVGoKL0NTMiBjcwoxIHNjbgovVDFfMSAxIFRmCjAgVGMKOS41MDAwMCAwIDAgOS41MDAwMCAyMC45NDg3MCAyMjYuMzI3MTAgVG0KKFNpZ24gSGVyZSkgVGoKL0NTMCBjcwowLjAwMTAwIDAuMDAyMDAgMC4wMDIwMCBzY24KL1QxXzAgMSBUZgoxMCAwIDAgMTAgMjAuOTQ4NzAgMjE0LjMyNzEwIFRtCihFbXBsKSBUagoobykgVGoKMi4yMDcwMCAwIFRkCih5KSBUagowLjQxMzAwIDAgVGQKKGVlIFwwNTBQb2xpY3kgT3duZXJcMDUxXDA3MiApIFRqCi9UVDAgMSBUZgo4IDAgMCA4IDExMi44MDY2MCAyMTQuMzI3MTAgVG0KWyAoXDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3KSAtMi45MDAwMCAoXDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzNykgXSBUSgovVDFfMCAxIFRmCjQ5LjcxMDAwIDAgVGQKKCAgICkgVGoKMTAgMCAwIDEwIDUxNC43MTA0MCAyMTQuMzI3MTAgVG0KKERhdGUgXDA3MikgVGoKOCAwIDAgOCA1MzUuNDM5MDAgMjE0LjMyNzEwIFRtCiggKSBUagovVFQwIDEgVGYKKFwxMzdcMTM3XDEzN1wxMzcgXDA1N1wxMzdcMTM3XDEzN1wxMzcgXDA1N1wxMzdcMTM3XDEzN1wxMzcpIFRqCi9UMV8wIDEgVGYKNi41NjEwMCAwIFRkCiggKSBUagoxMCAwIDAgMTAgMjAuOTQ4NzAgMTg4LjMyNzEwIFRtCihTaWduZWQgYXQgQ2l0eVwwNzIgKSBUagovVFQwIDEgVGYKOCAwIDAgOCA3NC41NTcxMCAxODguMzI3MTAgVG0KKFwxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzNykgVGoKL1QxXzAgMSBUZgoyMC41MjIwMCAwIFRkCiggICAgICApIFRqCi0wLjAxODAwIFRjCjAuMDE4MDAgVHcKMTAgMCAwIDEwIDI0Ny4xODQ2MCAxODguMzI3MTAgVG0KKFN0YXRlXDA3MikgVGoKMCBUYwowIFR3CjggMCAwIDggMjY3Ljc1MzkwIDE4OC4zMjcxMCBUbQooICkgVGoKL1RUMCAxIFRmCihcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3KSBUagovVDFfMCAxIFRmCi0wLjAxMDAwIFRjCjEwIDAgMCAxMCAyMC45NDg3MCAxNjkuMzI3MTAgVG0KKEkgY2VydGlmeSBJIGhhdmUgYXV0aG9yaXplZCBteSBlbXBsb3llciB0byBtYWtlIHBheXJvbGwgZGVkdWN0aW9uIG9mIHByZW1pdW1zIGZvciBteXNlbGYgYW5kIG15IGZhbWlseSBtZW1iZXJzXDA1NiAgU2lnbmVkXDA3MiApIFRqCi9UVDAgMSBUZgowIFRjCjggMCAwIDggNDUzLjY0NzkwIDE2OS4zMjcxMCBUbQooXDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzNykgVGoKL1QxXzEgMSBUZgotMC4wMTAwMCBUYwoxMCAwIDAgMTAgMjAuOTQ4NzAgMTQ4LjMyNzEwIFRtCihJbnN1cmFuY2UgUmVwcmVzZW50YXRpdmUgQ2VydGlmaWNhdGlvbiBcMDUwd2hlbiBJbnN1cmFuY2UgUmVwcmVzZW50YXRpdmUgYXNzaXN0ZWQgaW4gY29tcGxldGlvbiBvZiB0aGUgYXBwbGljYXRpb25cMDUxXDA3MiApIFRqCi9UMV8wIDEgVGYKKEkgY2VydGlmeSB0aGF0IEkgcmV2aWV3ZWQgYWxsIHF1ZXN0aW9ucyApIFRqCjAgLTEuMjAwMDAgVEQKKG9uIHRoaXMgYXBwbGljYXRpb25cMDU0IGFuZCB0aGF0IHRoZSBhbnN3ZXJzIGhhdmUgYmVlbiByZWNvcmRlZCBhY2N1cmF0ZWx5XDA1NiBJIGtub3cgb2Ygbm90aGluZyBhZmZlY3RpbmcgdGhlIGluc3VyYWJpbGl0eSBvZiB0aGUgcHIpIFRqCihvcG9zZWQgaW5zdXJlZFwwNTBzXDA1MSB3aGljaCBpcyBub3QgZnVsbHkgKSBUagpUKgoocmVjb3JkZWQgb24gdGhpcyBhcHBsaWNhdGlvblwwNTYpIFRqCjAgLTEuODAwMDAgVEQKKFRvIG15IGtub3dsZWRnZVwwNTQgdGhlIEFwcGxpY2FudCBoYXMgZXhpc3RpbmcgbGlmZSBpbnN1cmFuY2Ugb3IgYW5udWl0eSBjb3ZlcmFnZVwwNTYgICAgICAgICAgIFllcyAgICAgICBObyAgICAgIElmIHllc1wwNTQgYXJlIHRoZXkgcmUpIFRqCihwbGFjaW5nIGV4aXN0aW5nIGNvdmVyYWdlXDA3NyAgICAgICAgICBZZXMgICAgICAgTm8pIFRqCjAgLTIuODAwMDAgVEQKKEluc3VyYW5jZSBSZXByZXNlbnRhdGl2ZSBOYW1lXDA3MiApIFRqCi9UVDAgMSBUZgowIFRjCjggMCAwIDggMTM2LjgyNTcwIDc4LjMyNzEwIFRtCihcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzcpIFRqCi9UMV8wIDEgVGYKMTAgMCAwIDEwIDUwOC45NjYzMCA3OC4zMjcxMCBUbQooICkgVGoKLTAuMDEwMDAgVGMKLTQ4LjgwMjAwIC0yLjgwMDAwIFRkCihJbnN1cmFuY2UgUmVwcmVzZW50YXRpdmUgU2lnbmF0dXJlXDA3MiApIFRqCi9UVDAgMSBUZgowIFRjCjggMCAwIDggMTUwLjc3NDkwIDUwLjMyNzEwIFRtCihcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzNykgVGoKL1QxXzAgMSBUZgo0NC42OTMwMCAwIFRkCiggICApIFRqCjEwIDAgMCAxMCA1MTIuNTQ1NDAgNTAuMzI3MTAgVG0KKERhdGUgXDA3MikgVGoKOCAwIDAgOCA1MzMuMjc0OTAgNTAuMzI3MTAgVG0KKCApIFRqCi9UVDAgMSBUZgooXDEzN1wxMzdcMTM3XDEzNyBcMDU3XDEzN1wxMzdcMTM3XDEzNyBcMDU3XDEzN1wxMzdcMTM3XDEzNykgVGoKRVQKMC4xMzcwMCAwLjEyMzAwIDAuMTI2MDAgc2NuCi9HUzEgZ3MKcQoxIDAgMCAxIDMwNS45NTYzMCA3MzguNzA0NTAgY20KMCAwIG0KMCAtNS4wMjAwMCBsCi0wLjQ0ODAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTUuNDUxMDAgbAotNS4zNTYwMCAtMC40MDAwMCBsCi00LjkzOTAwIDAgbApoCi01LjAxOTAwIC01LjEwMDAwIDQuNDI3MDAgNC41NDcwMCByZQpmClEKcQoxIDAgMCAxIDMyMi4xMTU2MCA3MzguNzA0NTAgY20KMCAwIG0KMCAtNS4wMjAwMCBsCi0wLjQ0ODAwIC01LjQ1MTAwIGwKLTUuMzU2MDAgLTUuNDUxMDAgbAotNS4zNTYwMCAtMC40MDAwMCBsCi00LjkzOTAwIDAgbApoCi01LjAxOTAwIC01LjEwMDAwIDQuNDI3MDAgNC41NDcwMCByZQpmClEKUQpxCjEgMCAwIDEgMCAwIGNtCkJUCi9GMSAxMiBUZgoxNC40MDAwMCBUTApFVApCVAoxIDAgMCAxIDMwOSAxMDguMDUwODAgVG0KL0YyKzAgMTAgVGYKMTIgVEwKKFgpIFRqClQqCkVUCkJUCjEgMCAwIDEgNTI1IDEwOC4wNTA4MCBUbQovRjIrMCAxMCBUZgoxMiBUTAooWCkgVGoKVCoKRVQKQlQKMSAwIDAgMSA4MSAxODkuMDUwODAgVG0KL0YyKzAgMTAgVGYKMTIgVEwKKENoaWNhZ28pIFRqClQqCkVUCkJUCjEgMCAwIDEgMjc4IDE4OS4wNTA4MCBUbQovRjIrMCAxMCBUZgoxMiBUTAooSUwpIFRqClQqCkVUCkJUCjEgMCAwIDEgMTQzIDgzLjA1MDc4IFRtCi9GMiswIDEwIFRmCjEyIFRMCihUZXN0aW5nIEFnZW50KSBUagpUKgpFVApCVAoxIDAgMCAxIDM2NiA2MTIuMDUwODAgVG0KL0YyKzAgMTAgVGYKMTIgVEwKKFgpIFRqClQqCkVUCkJUCjEgMCAwIDEgMzY2IDU1MC4wNTA4MCBUbQovRjIrMCAxMCBUZgoxMiBUTAooWCkgVGoKVCoKRVQKQlQKMSAwIDAgMSAzNjYgNTAyLjA1MDgwIFRtCi9GMiswIDEwIFRmCjEyIFRMCihYKSBUagpUKgpFVApCVAoxIDAgMCAxIDM4NyA2NjYuMDUwODAgVG0KVCoKRVQKQlQKMSAwIDAgMSAzODggNjEyLjA1MDgwIFRtClQqCkVUCkJUCjEgMCAwIDEgMzg4IDYwMC4wNTA4MCBUbQpUKgpFVApCVAoxIDAgMCAxIDM4OCA1NTAuMDUwODAgVG0KVCoKRVQKQlQKMSAwIDAgMSAzODggNTI1LjA1MDgwIFRtClQqCkVUCkJUCjEgMCAwIDEgMzg4IDUwMy4wNTA4MCBUbQpUKgpFVApCVAoxIDAgMCAxIDM4OCA0NzguMDUwODAgVG0KVCoKRVQKQlQKMSAwIDAgMSA0MjkgNjY1LjA1MDgwIFRtCi9GMiswIDEwIFRmCjEyIFRMCihYKSBUagpUKgpFVApCVAoxIDAgMCAxIDQzMSA2MTIuMDUwODAgVG0KL0YyKzAgMTAgVGYKMTIgVEwKKFgpIFRqClQqCkVUCkJUCjEgMCAwIDEgNDMxIDYwMC4wNTA4MCBUbQovRjIrMCAxMCBUZgoxMiBUTAooWCkgVGoKVCoKRVQKQlQKMSAwIDAgMSA0MzEgNTUwLjA1MDgwIFRtCi9GMiswIDEwIFRmCjEyIFRMCihYKSBUagpUKgpFVApCVAoxIDAgMCAxIDQzMSA1MjUuMDUwODAgVG0KL0YyKzAgMTAgVGYKMTIgVEwKKFgpIFRqClQqCkVUCkJUCjEgMCAwIDEgNDMxIDUwMi4wNTA4MCBUbQovRjIrMCAxMCBUZgoxMiBUTAooWCkgVGoKVCoKRVQKQlQKMSAwIDAgMSA0MzEgNDc3LjA1MDgwIFRtCi9GMiswIDEwIFRmCjEyIFRMCihYKSBUagpUKgpFVApCVAoxIDAgMCAxIDQ1MiA2NjUuMDUwODAgVG0KVCoKRVQKQlQKMSAwIDAgMSA0NTIgNjEyLjA1MDgwIFRtClQqCkVUCkJUCjEgMCAwIDEgNDUyIDYwMC4wNTA4MCBUbQpUKgpFVApCVAoxIDAgMCAxIDQ1MiA1NTEuMDUwODAgVG0KVCoKRVQKQlQKMSAwIDAgMSA0NTIgNTI1LjA1MDgwIFRtClQqCkVUCkJUCjEgMCAwIDEgNDUyIDUwMi4wNTA4MCBUbQpUKgpFVApCVAoxIDAgMCAxIDQ1MiA0NzguMDUwODAgVG0KVCoKRVQKQlQKMSAwIDAgMSA0OTQgNjY1LjA1MDgwIFRtCi9GMiswIDEwIFRmCjEyIFRMCihYKSBUagpUKgpFVApCVAoxIDAgMCAxIDQ5NSA2MTIuMDUwODAgVG0KL0YyKzAgMTAgVGYKMTIgVEwKKFgpIFRqClQqCkVUCkJUCjEgMCAwIDEgNDk1IDYwMC4wNTA4MCBUbQovRjIrMCAxMCBUZgoxMiBUTAooWCkgVGoKVCoKRVQKQlQKMSAwIDAgMSA0OTUgNTUwLjA1MDgwIFRtCi9GMiswIDEwIFRmCjEyIFRMCihYKSBUagpUKgpFVApCVAoxIDAgMCAxIDQ5NSA1MjUuMDUwODAgVG0KL0YyKzAgMTAgVGYKMTIgVEwKKFgpIFRqClQqCkVUCkJUCjEgMCAwIDEgNDk1IDUwMi4wNTA4MCBUbQovRjIrMCAxMCBUZgoxMiBUTAooWCkgVGoKVCoKRVQKQlQKMSAwIDAgMSA1MTYgNjY1LjA1MDgwIFRtClQqCkVUCkJUCjEgMCAwIDEgNTE3IDYxMi4wNTA4MCBUbQpUKgpFVApCVAoxIDAgMCAxIDUxNyA2MDAuMDUwODAgVG0KVCoKRVQKQlQKMSAwIDAgMSA1MTcgNTUwLjA1MDgwIFRtClQqCkVUCkJUCjEgMCAwIDEgNTE2IDUyNS4wNTA4MCBUbQpUKgpFVApCVAoxIDAgMCAxIDUxNyA1MDIuMDUwODAgVG0KVCoKRVQKQlQKMSAwIDAgMSA1MTYgNDc4LjA1MDgwIFRtClQqCkVUCkJUCjEgMCAwIDEgNTU4IDY2NS4wNTA4MCBUbQovRjIrMCAxMCBUZgoxMiBUTAooWCkgVGoKVCoKRVQKQlQKMSAwIDAgMSA1NTkgNjEyLjA1MDgwIFRtCi9GMiswIDEwIFRmCjEyIFRMCihYKSBUagpUKgpFVApCVAoxIDAgMCAxIDU1OSA2MDAuMDUwODAgVG0KL0YyKzAgMTAgVGYKMTIgVEwKKFgpIFRqClQqCkVUCkJUCjEgMCAwIDEgNTU5IDU1MC4wNTA4MCBUbQovRjIrMCAxMCBUZgoxMiBUTAooWCkgVGoKVCoKRVQKQlQKMSAwIDAgMSA1NTkgNTI1LjA1MDgwIFRtCi9GMiswIDEwIFRmCjEyIFRMCihYKSBUagpUKgpFVApCVAoxIDAgMCAxIDU1OSA1MDIuMDUwODAgVG0KL0YyKzAgMTAgVGYKMTIgVEwKKFgpIFRqClQqCkVUCkJUCjEgMCAwIDEgNTU5IDQ3Ny4wNTA4MCBUbQovRjIrMCAxMCBUZgoxMiBUTAooWCkgVGoKVCoKRVQKQlQKMSAwIDAgMSA1ODIgNjY1LjA1MDgwIFRtClQqCkVUCkJUCjEgMCAwIDEgNTgzIDYxMi4wNTA4MCBUbQpUKgpFVApCVAoxIDAgMCAxIDU4MyA2MDAuMDUwODAgVG0KVCoKRVQKQlQKMSAwIDAgMSA1ODMgNTUwLjA1MDgwIFRtClQqCkVUCkJUCjEgMCAwIDEgNTgzIDUyNS4wNTA4MCBUbQpUKgpFVApCVAoxIDAgMCAxIDU4MiA1MDMuMDUwODAgVG0KVCoKRVQKQlQKMSAwIDAgMSA1ODIgNDc4LjA1MDgwIFRtClQqCkVUCkJUCjEgMCAwIDEgMzM1IDc0OC4wNTA4MCBUbQovRjIrMCAxMCBUZgoxMiBUTAooWCkgVGoKVCoKRVQKQlQKMSAwIDAgMSAzMDkgMTA4LjA1MDgwIFRtCi9GMiswIDEwIFRmCjEyIFRMCihYKSBUagpUKgpFVApCVAoxIDAgMCAxIDUyNSAxMDguMDUwODAgVG0KL0YyKzAgMTAgVGYKMTIgVEwKKFgpIFRqClQqCkVUCkJUCjEgMCAwIDEgMzAwIDczNC4wNTA4MCBUbQovRjIrMCAxMCBUZgoxMiBUTAooWCkgVGoKVCoKRVQKUQoKZW5kc3RyZWFtCmVuZG9iago2NCAwIG9iagpbIC9TZXBhcmF0aW9uIC9QQU5UT05FIzIwMTkzIzIwQ1ZVIDkgMCBSIDw8Ci9OIDEKL0RvbWFpbiBbIDAgMSBdCi9DMSBbIDAuODQ5NzAgMC4yMjU1MiAwLjI4ODgyIF0KL1JhbmdlIFsgMCAxIDAgMSAwIDEgXQovQzAgWyAxIDEgMSBdCi9GdW5jdGlvblR5cGUgMgo+PiBdCmVuZG9iago2NSAwIG9iago8PAovUyAvVHJhbnNwYXJlbmN5Ci9DUyA2NiAwIFIKL0sgZmFsc2UKL0kgZmFsc2UKPj4KZW5kb2JqCjY2IDAgb2JqClsgL0lDQ0Jhc2VkIDEwIDAgUiBdCmVuZG9iagp4cmVmCjAgNjcKMDAwMDAwMDAwMCA2NTUzNSBmIAowMDAwMDAwMDA5IDAwMDAwIG4gCjAwMDAwMDAwNzQgMDAwMDAgbiAKMDAwMDAwMDExNCAwMDAwMCBuIAowMDAwMDAwNzk5IDAwMDAwIG4gCjAwMDAwMDEzMDcgMDAwMDAgbiAKMDAwMDAwMTM1NiAwMDAwMCBuIAowMDAwMDQ5MTM2IDAwMDAwIG4gCjAwMDAwNDk0MzQgMDAwMDAgbiAKMDAwMDA0OTUwMyAwMDAwMCBuIAowMDAwMDQ5NTM5IDAwMDAwIG4gCjAwMDAwNTIxOTIgMDAwMDAgbiAKMDAwMDA1MjMxNSAwMDAwMCBuIAowMDAwMDUyNjE1IDAwMDAwIG4gCjAwMDAwNTI2ODUgMDAwMDAgbiAKMDAwMDA1Mjk4NyAwMDAwMCBuIAowMDAwMDUzMDU3IDAwMDAwIG4gCjAwMDAwNTMzNjAgMDAwMDAgbiAKMDAwMDA1MzQzMCAwMDAwMCBuIAowMDAwMDUzNzI5IDAwMDAwIG4gCjAwMDAwNTM3OTkgMDAwMDAgbiAKMDAwMDA1NDEwMCAwMDAwMCBuIAowMDAwMDU0MTcwIDAwMDAwIG4gCjAwMDAwNTQ0NzMgMDAwMDAgbiAKMDAwMDA1NDU0MyAwMDAwMCBuIAowMDAwMDU0ODQ0IDAwMDAwIG4gCjAwMDAwNTQ5MTQgMDAwMDAgbiAKMDAwMDA1NTIxNyAwMDAwMCBuIAowMDAwMDU1Mjg3IDAwMDAwIG4gCjAwMDAwNTU1OTAgMDAwMDAgbiAKMDAwMDA1NTY2MCAwMDAwMCBuIAowMDAwMDU1NzgxIDAwMDAwIG4gCjAwMDAwNTU5MTYgMDAwMDAgbiAKMDAwMDA1NzM3MyAwMDAwMCBuIAowMDAwMDU3NjA3IDAwMDAwIG4gCjAwMDAwNjk0NzUgMDAwMDAgbiAKMDAwMDA3MDI2MiAwMDAwMCBuIAowMDAwMDcwMzc2IDAwMDAwIG4gCjAwMDAwNzA3NTAgMDAwMDAgbiAKMDAwMDA3MTE2NiAwMDAwMCBuIAowMDAwMDcyNjE4IDAwMDAwIG4gCjAwMDAwNzI3MjYgMDAwMDAgbiAKMDAwMDA3MzI1NCAwMDAwMCBuIAowMDAwMDczODk2IDAwMDAwIG4gCjAwMDAwNzczNjcgMDAwMDAgbiAKMDAwMDA3Nzc1OSAwMDAwMCBuIAowMDAwMDc4MTc3IDAwMDAwIG4gCjAwMDAwNzkwMjcgMDAwMDAgbiAKMDAwMDA3OTU5NCAwMDAwMCBuIAowMDAwMDgwNDIzIDAwMDAwIG4gCjAwMDAwODQ4NjEgMDAwMDAgbiAKMDAwMDA4NTM0NSAwMDAwMCBuIAowMDAwMDg1ODk2IDAwMDAwIG4gCjAwMDAwODg2ODMgMDAwMDAgbiAKMDAwMDA4ODg1NSAwMDAwMCBuIAowMDAwMDg5MTI0IDAwMDAwIG4gCjAwMDAxMDM0MTEgMDAwMDAgbiAKMDAwMDEwMzcyMSAwMDAwMCBuIAowMDAwMTA0MDA4IDAwMDAwIG4gCjAwMDAxMTE1ODUgMDAwMDAgbiAKMDAwMDExMTc1NCAwMDAwMCBuIAowMDAwMTExOTE5IDAwMDAwIG4gCjAwMDAxMTE5ODcgMDAwMDAgbiAKMDAwMDExMjAyNCAwMDAwMCBuIAowMDAwMTQyMDQzIDAwMDAwIG4gCjAwMDAxNDIyMTQgMDAwMDAgbiAKMDAwMDE0MjI4MiAwMDAwMCBuIAp0cmFpbGVyCjw8Ci9TaXplIDY3Ci9Sb290IDUgMCBSCi9JbmZvIDIgMCBSCj4+CnN0YXJ0eHJlZgoxNDIzMTkKJSVFT0YK',
                      'pages': '2', 'documentId': '1'},
         'inlineTemplates': [{'recipients': {
             'carbonCopies': [{'templateRequired': False,
                               'name': 'Test CC Recipient',
                               'routingOrder': '2',
                               'tabs': {},
                               'roleName': 'None',
                               'recipientId': '2',
                               'email': 'zmason@delmarsd.com'}]},
             'sequence': '1'}]},
        #  {
        # 'document': {'name': 'ChildAttachmentForm',
        #              'sequence': '1',
        #              'fileExtension': 'pdf',
        #              'documentBase64': 'SlZCRVJpMHhMalFOQ2lXVGpJdWVJRkpsY0c5eWRFeGhZaUJIWlc1bGNtRjBaV1FnVUVSR0lHUnZZM1Z0Wlc1MElHaDBkSEE2THk5M2QzY3VjbVZ3YjNKMGJHRmlMbU52YlEwS01TQXdJRzlpYWcwS1BEd2dMMFl4SURJZ01DQlNJQzlHTWlBeklEQWdVaUF2UmpNZ05DQXdJRklnUGo0TkNtVnVaRzlpYWcwS01pQXdJRzlpYWcwS1BEd2dMMEpoYzJWR2IyNTBJQzlJWld4MlpYUnBZMkVnTDBWdVkyOWthVzVuSUM5WGFXNUJibk5wUlc1amIyUnBibWNnTDA1aGJXVWdMMFl4SUM5VGRXSjBlWEJsSUM5VWVYQmxNU0F2Vkhsd1pTQXZSbTl1ZENBK1BnMEtaVzVrYjJKcURRb3pJREFnYjJKcURRbzhQQ0F2UW1GelpVWnZiblFnTDBobGJIWmxkR2xqWVMxQ2IyeGtJQzlGYm1OdlpHbHVaeUF2VjJsdVFXNXphVVZ1WTI5a2FXNW5JQzlPWVcxbElDOUdNaUF2VTNWaWRIbHdaU0F2Vkhsd1pURWdMMVI1Y0dVZ0wwWnZiblFnUGo0TkNtVnVaRzlpYWcwS05DQXdJRzlpYWcwS1BEd2dMMEpoYzJWR2IyNTBJQzlVYVcxbGN5MVNiMjFoYmlBdlJXNWpiMlJwYm1jZ0wxZHBia0Z1YzJsRmJtTnZaR2x1WnlBdlRtRnRaU0F2UmpNZ0wxTjFZblI1Y0dVZ0wxUjVjR1V4SUM5VWVYQmxJQzlHYjI1MElENCtEUXBsYm1Sdlltb05DalVnTUNCdlltb05Danc4SUM5RGIyNTBaVzUwY3lBNUlEQWdVaUF2VFdWa2FXRkNiM2dnV3lBd0lEQWdOakV5SURjNU1pQmRJQzlRWVhKbGJuUWdPQ0F3SUZJZ0wxSmxjMjkxY21ObGN5QThQQ0F2Um05dWRDQXhJREFnVWlBdlVISnZZMU5sZENCYklDOVFSRVlnTDFSbGVIUWdMMGx0WVdkbFFpQXZTVzFoWjJWRElDOUpiV0ZuWlVrZ1hTQStQaUF2VW05MFlYUmxJREFnTDFSeVlXNXpJRHc4SUNBK1BpQU5DaUFnTDFSNWNHVWdMMUJoWjJVZ1BqNE5DbVZ1Wkc5aWFnMEtOaUF3SUc5aWFnMEtQRHdnTDA5MWRHeHBibVZ6SURFd0lEQWdVaUF2VUdGblpVMXZaR1VnTDFWelpVNXZibVVnTDFCaFoyVnpJRGdnTUNCU0lDOVVlWEJsSUM5RFlYUmhiRzluSUQ0K0RRcGxibVJ2WW1vTkNqY2dNQ0J2WW1vTkNqdzhJQzlCZFhSb2IzSWdLRndvWVc1dmJubHRiM1Z6WENrcElDOURjbVZoZEdsdmJrUmhkR1VnS0VRNk1qQXhOVEE0TURFd05qRXdNVGdyTURBbk1EQW5LU0F2UTNKbFlYUnZjaUFvWENoMWJuTndaV05wWm1sbFpGd3BLU0F2UzJWNWQyOXlaSE1nS0NrZ0wxQnliMlIxWTJWeUlDaFNaWEJ2Y25STVlXSWdVRVJHSUV4cFluSmhjbmtnTFNCM2QzY3VjbVZ3YjNKMGJHRmlMbU52YlNrZ0wxTjFZbXBsWTNRZ0tGd29kVzV6Y0dWamFXWnBaV1JjS1NrZ0RRb2dJQzlVYVhSc1pTQW9YQ2hoYm05dWVXMXZkWE5jS1NrZ1BqNE5DbVZ1Wkc5aWFnMEtPQ0F3SUc5aWFnMEtQRHdnTDBOdmRXNTBJREVnTDB0cFpITWdXeUExSURBZ1VpQmRJQzlVZVhCbElDOVFZV2RsY3lBK1BnMEtaVzVrYjJKcURRbzVJREFnYjJKcURRbzhQQ0F2Um1sc2RHVnlJRnNnTDBGVFEwbEpPRFZFWldOdlpHVWdMMFpzWVhSbFJHVmpiMlJsSUYwZ0wweGxibWQwYUNBeE56TXlJRDQrRFFwemRISmxZVzBOQ2tkaUlpOG9PeXcvTkRNbU9sSXVMelZGSjF3elB6SWxQU2hOTTBKaVJqQXVOREZhSkdCaGNsMWtWand0VEVoVFF6cHFKVDVqS25GQlBTbGZabDVVYVhSTVNrdzBTR0ZJVGxoeE0ybDFMVEVsUWpCUFdDUkxLaXNsTWpNNVIwWk1JV1J1SzJOeFhDY3lVeUZtU2lJNk5FNXBRWEpVUXo5cUtsNU9XMGd0YTB4QUxqRWpYejVlTTJnNVBWTmlQRnc5S0Q5Rk16ZFVSalpPVEcxSllYVTFkVTBvTEZsVlJpSmRYVTB1TzJ0REtVdzJTRTluU2lKa1hUbERKbWc3Wm1aaU4xbHFialJ1TUNGMFhDUTFXV0JDWUR0S05HRTRMaTlzVHpoZ2JUbHhMbGs5VTBKRlVEOVBURVExVUMxWmIwdHliamswTjJJN09TRStUMkJEYjJaS1NpcFNMalpoUzFaZlAxWXhhejg5UG1SU01FUW9SMU15VFZKWWNEOHhNbE03UVRKVVNVQmtPRXRoTFcxcFRGdHVOVWRnTFU0eEltSTFSMmhTTFdZeFlHMVlWV2dqYkRaZlRrSnlJVTFEUkZvelhWSlZiVVZtYUZOVkoyazdYV05uVEZ0Y1J5VlNKa0JVSVY4NFJqRXVJVUp4SVRWZGNTZ2xPME5SVTFoQlprZHBPRU02SmxoV1NVdEZTakF2UURRbVlFUTNRQzBxVEdrN05qbFBibGREUkZ4SVpTMUJKVHhmVlQwbFd6SkRNMEVrTWtNL2RGWTVRU0luWEVCalJUcFdYbGRUUTBZbFFsMU1UbEZvTkhKSklqYzZNU1ZjUTF3dmJURlZMV1k3WVZzb0tGYzJibkZZU1dNb1VTTkdRUzVVT1NJMVJ5UXJWazRsUmlOZVAxdEtJbWhUWGxaVFRFRTlNRUJRWVVOWGNpcE5NQ3AxUTJOTU5pYzZSajlNVjFGYVpTWThibFJ4U0dJNFcwUTliVUFxS0c0ellVMDdaRmRzU2lnd2RVNDZUMVp4S0d0MFBHWWtkVXhOVXlnckxWWk5jVHRtUENnK2NXeHlSMFp4TzJjbldXcEdaMHdsSVdRM1BqNHBiR1kvY1dvalprUklYVnBpWTI0MmJDTWpXRFltS1Q5alRWaGlibWd3S1Y5cFAweGlYMDlMVHpCSlcyUW5iQzVaTXlOdVJEMHRheWRVUVVRcVQxY2lOQ3NsU3kxWk55NXNjbTFzSWlkclAxVnlXUzVpWXlKb0sxUk5LbDAwVWxRblpWcGJKa3NwYkRKZVF6RnZMa1VpTVVBK1JXVStYRGsyTVRNd2FVNGtYR05aWW1OWE9sMXpOVFpzUkVvMllFcGViaUpUT1VseVBXaEtKVWtsVFVnbkxFbzJkRkJUTUNjNlJUWTdhMElqVEV4T05sMVBPRE1vTVQ5bmNUWkNLaXc2U3pWWWNpRm5jRzl1UURCcVhsWkZQeW8zVG01dU8yUmJjVHBEVnpaTVdVRTlaR1ZDUlR0VWN5RWhQa1pjTEZKYU9VTTJKRU5VY1VVOEtUMTFkR3N2VmtkVEluQjFaVkJaVFRWcUwxOVpiaTVTTGo1eEpqb2hhQ016TVM5REwwZExaV1JNWGw5aUwzUkdRU3hMTWtrbVVqUkRZRjgyZFQ5dUxUWjFhbEpsVTNVNEkwRThiMjAyV2xZNlFDMUViRk04TERVM1ZGdE9ialpBTmpraUpVVm9VajFVYm1ZbldYQktiMkpZVWkwcmJDNHJORTl0U3lvNmNXWXBOQ2RrYVNReVoyUnRjREJRUUZnOFUwbHZLV2hyVTBaMVlsWnZZeVJzUVRzK1oyczhZMEpiUERkS1JrSkpUMDl2SWtwYUwyZE1Uelp5UDNGaEwwbEZSVkJPUDB0elBsMVBWVlpPVlZzaEwySTBNMEZlSW1WWFVYQXBLR3BGUFVreExuVlRVWEJmSkM1RklsMWROakF1V21aT00yNUFWMmhKTVY1S1RTVllSSFJtYWxKYlFDZ3pWRmhxTHlKSFVGWmZYWEFxY2lsWklWczFSM0VoUlQ5bk4zQmlVemNzSWlSemJrUnVYVkpnSWpvbUl5OGlMMmhzZFhVM0tGZFJZU01tWmloWlJpeE5JVTlLZEM5UVpXUk9PVjVHSkhFeUoyVmNORUJtWjFVNE0yRlJMaWRnUFZGRWNDOXZMRElsSVN4cVhWa3FPbUZiYVVOR2J5US9aVG81SmlJMFVuSWpVa1JxYUZ4SEwwODNMemdtU21GUk9EaEJQVkUzTjAwdFBUUkxRRGRvV0NwaldWOVhWa2R2WjAxWU9XZEtYVjlYSmxCQlZUWm1KVmRLUFNoZ0xIRlNQU1JzSXl4RUltWm1iU05lUUhGMVgyRXBKRWh5SWxkcE5pMWdZbE1xWHlkdFZDRmxMeUZKUDA4MkxrRjBhemRpVVQwcWNEMUpPbEpsVzIxUlB6Rm5RQ2tuVVVrM0xrbHBhU1l2ZFM1UWJqVkFLMWtzSVQ1Z1BUQTJMRlpCV0V3akxsMG5ZbDVtYlVaelZrQkRMRGc0WjA4NlV6QWpTVHRGSjExWlVHQTVKR2xuVjJnOU5DSkpiamhXVlZOSkl5Sm1NQ3MyVkQ5WmNpNWpPMnBRT1MxSlpXZGZYaUZIY0ZzaVltSm9VSEpiUldWbEpFZzBjanNyU3lOZVlWWkJVRXBPSVhSZlRtTk5PbGxlT0hWQ01ISXFUU0kwY3pJd1dYTnBiQ2xDT3k0dk5Xc3pYMUkzYzNJL05GOVhMV0ZtWlYxWWJTYzVXVHNwYjNOalBUdE1VbEU3Y1Uxd1kxMW1jemhGT0NScU5Gd3BORkZJVERrb0psTmlUanhZWVhSYVgyOGxWaUZYYlVWVVVDaERjVUZOTVdnc0xsQkdTQzQzYlRGaVFqRmxSMUpJVVRkMUtTRmNjamNuYlNZNWRFWmpPVmxmSkNOY1pWazZSR1VtV2tRN0prNXBNbGxHVFRwZVNFSnlJelV4UmxacWNDbGlPbkZBUkRobFUxQTVLREpjWlcxeFEwMDZQVjBoTkRrNE5XaHpTbDRyY1c1WlpWUnZZekJDVjE4dGZqNWxibVJ6ZEhKbFlXME5DbVZ1Wkc5aWFnMEtNVEFnTUNCdlltb05Danc4SUM5RGIzVnVkQ0F3SUM5VWVYQmxJQzlQZFhSc2FXNWxjeUErUGcwS1pXNWtiMkpxRFFwNGNtVm1EUW93SURFeERRb3dNREF3TURBd01EQXdJRFkxTlRNMUlHWU5DakF3TURBd01EQXdOelVnTURBd01EQWdiZzBLTURBd01EQXdNREV5T1NBd01EQXdNQ0J1RFFvd01EQXdNREF3TWpNNUlEQXdNREF3SUc0TkNqQXdNREF3TURBek5UUWdNREF3TURBZ2JnMEtNREF3TURBd01EUTJOaUF3TURBd01DQnVEUW93TURBd01EQXdOall6SURBd01EQXdJRzROQ2pBd01EQXdNREEzTlRFZ01EQXdNREFnYmcwS01EQXdNREF3TURrNE55QXdNREF3TUNCdURRb3dNREF3TURBeE1EUTVJREF3TURBd0lHNE5DakF3TURBd01ESTROemNnTURBd01EQWdiZzBLZEhKaGFXeGxjZzBLUER3Z0wwbEVJQTBLSUNVZ1VtVndiM0owVEdGaUlHZGxibVZ5WVhSbFpDQlFSRVlnWkc5amRXMWxiblFnTFMwZ1pHbG5aWE4wSUNob2RIUndPaTh2ZDNkM0xuSmxjRzl5ZEd4aFlpNWpiMjBwRFFvZ1d5aG9KVnd3TXpOcFhESXpNM2hjTWpJM1hESXlORHBjTWpZeU1Gd3lORFJIZUZ3eU1EWmNNRE14S1NBb2FDVmNNRE16YVZ3eU16TjRYREl5TjF3eU1qUTZYREkyTWpCY01qUTBSM2hjTWpBMlhEQXpNU2xkRFFvZ0wwbHVabThnTnlBd0lGSWdMMUp2YjNRZ05pQXdJRklnTDFOcGVtVWdNVEVnUGo0TkNuTjBZWEowZUhKbFpnMEtNamt5TncwS0pTVkZUMFlOQ2c9PQ==',
        #              'pages': '1',
        #              'documentId': '1'},
        # 'inlineTemplates': [{'recipients': {
        #     'carbonCopies': [
        #         {'templateRequired': False,
        #          'name': 'Test CC Recipient',
        #          'routingOrder': '2', 'tabs': {},
        #          'roleName': 'None',
        #          'recipientId': '2',
        #          'email': 'zmason@delmarsd.com'}]},
        #                      'sequence': '1'}]
        #  },
        {
            'document': {
                'name': 'FPPReplacementFormTemplate',
                'sequence': '1', 'fileExtension': 'pdf',
                'documentBase64': 'JVBERi0xLjMKMSAwIG9iago8PAovS2lkcyBbIDMgMCBSIF0KL1R5cGUgL1BhZ2VzCi9Db3VudCAxCj4+CmVuZG9iagoyIDAgb2JqCjw8Ci9Qcm9kdWNlciAoUHlQREYyKQo+PgplbmRvYmoKMyAwIG9iago8PAovUGFyZW50IDEgMCBSCi9Db250ZW50cyA1IDAgUgovVHlwZSAvUGFnZQovUmVzb3VyY2VzIDw8Ci9Db2xvclNwYWNlIDw8Ci9DczYgNiAwIFIKPj4KL0V4dEdTdGF0ZSA8PAovR1MxIDggMCBSCj4+Ci9Gb250IDw8Ci9GMiswIDkgMCBSCi9GMSAxMyAwIFIKL1RUMiAxNCAwIFIKPj4KL1Byb2NTZXQgWyAvSW1hZ2VDIC9UZXh0IC9QREYgL0ltYWdlSSAvSW1hZ2VCIF0KPj4KL1JvdGF0ZSAwCi9Dcm9wQm94IFsgMCAwIDYxMiA3OTIgXQovTWVkaWFCb3ggWyAwIDAgNjEyIDc5MiBdCi9Bbm5vdHMgWyBdCj4+CmVuZG9iago0IDAgb2JqCjw8Ci9UeXBlIC9DYXRhbG9nCi9QYWdlcyAxIDAgUgo+PgplbmRvYmoKNSAwIG9iago8PAovTGVuZ3RoIDQyODkKPj4Kc3RyZWFtCnEKL0dTMSBncwpCVAovVFQyIDEgVGYKMTIgMCAwIDEyIDkwIDM4Ljc2MDAwIFRtCi9DczYgY3MKMCAwIDAgc2NuCjAuMDAwNTAgVGMKMC4wMDQ1MCBUdwpbIChSRVBMTU5UIEZvcm0pIDguMzAwMDAgKCBSMTIxMFwwNTBBXDA1MVwwNTVJTCkgNi4zMDAwMCAoICkgXSBUSgoxMy40NDUwMCA1Ny4zNTAwMCBURAotMC4wMDA3MCBUYwowLjAwNTcwIFR3ClsgKFNUQVRFIE9GIElMTElOT0lTICkgXSBUSgotMC40NjAwMCAtMS4xNTAwMCBURAowLjAwMDIwIFRjCi0wLjAwMDIwIFR3CihJTVBPUlRBTlQgTk9USUNFXDA3MiApIFRqCi02Ljc4MDAwIC0xLjE1MDAwIFRECi0wLjAwMDQwIFRjCjAuMDAyOTAgVHcKWyAoUkVQTEFDRU1FTlQgT0YgTElGRSApIF0gVEoKMTEuNDE1MDAgMCBURAowLjAwMDEwIFRjCi0wLjAwMDEwIFR3ClsgKElOU1VSQU5DRSBPUikgLTIuOTAwMDAgKCBBTk5VSVRZICkgXSBUSgotMi44NzAwMCAtMS4xNDUwMCBURAowIFRjCjAgVHcKKFwxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzcpIFRqCkVUCjI2NyA2ODMuNjQwMDAgNzggMS4zMjAwMCByZQpmCkJUCjEyIDAgMCAxMiAzNDUgNjg1LjYyMDAwIFRtCiggKSBUagotMy4yNTAwMCAtMS4xNTAwMCBURAooICkgVGoKLTE4IC0xLjE1MDAwIFRECjAuMDAwMzAgVGMKMC4yOTk3MCBUdwpbICggKSAtMjQ1MCAoQXJlIHlvdSB0aGlua2luZyBhYm91dCBidXlpbmcgYSBuZXcgbGlmZSBpbnN1cmFuY2UgcG9saWN5IG9yIGFubnVpdHkgYW5kICkgXSBUSgpUKgowLjAwMDEwIFRjCjAuMDEzMjAgVHcKWyAoZGlzY29udGludWluZyBvciBjaGFuZ2luZyBhbiApIF0gVEoKMTEuOTQ1MDAgMCBURAotMC4wMDA3MCBUYwowLjAxNDQwIFR3ClsgKGV4aXN0aW5nIG9uZVwwNzcpIC02LjkwMDAwICggIElmIHlvKSBdIFRKCjcuNzY1MDAgMCBURAotMC4wMDAxMCBUYwowLjAxNTEwIFR3ClsgKHUgYXJlXDA1NCB5b3VyIGRlY2lzaW9uKSBdIFRKCjcuOTI1MDAgMCBURAotMC4wMDA3MCBUYwowLjAxNDcwIFR3ClsgKCBjb3VsZCBiZSBhIGdvb2Qgb25lICkgXSBUSgotMjcuNjM1MDAgLTEuMTUwMDAgVEQKMC4wMDA2MCBUYwowLjAwMDQwIFR3ClsgKFwyMjYgb3IgYSBtKSA4LjQwMDAwIChpKSAtMS42MDAwMCAoc3Rha2VcMDU2ICBZb3Ugd2lsbCBub3Qga25vdyBmb3Igc3VyZSkgXSBUSgoxNy4yNzAwMCAwIFRECjAuMDAxMDAgVGMKMC4wMDEwMCBUdwpbICggdW5sZXMpIDUuMjAwMDAgKHMgeW91IG0pIDguODAwMDAgKGEpIC0wLjIwMDAwIChrZSBhIGNhcmVmKSBdIFRKCjkuODY1MDAgMCBURAowLjAwMDMwIFRjCjAuMDAxMzAgVHcKKHVsIGNvbXBhcmlzb24gb2YgeW91ciApIFRqCi0yNy4xMzUwMCAtMS4xNDUwMCBURAotMC4wMDAyMCBUYwowLjAwMDIwIFR3CihleGlzdGluZyBiZW5lZml0cyBhbmQgdGhlIHByb3Bvc2VkIGJlbmVmaXRzXDA1NiApIFRqCjAgLTEuMTUwMDAgVEQKMCBUYwowIFR3CiggKSBUagpUKgowLjAwMDYwIFRjCjAuMTA0NDAgVHcKWyAoICkgLTI2NDUgKE1ha2Ugc3VyZSB5b3UgdW5kZXJzdGFuZCB0aGUgZmFjdHNcMDU2ICBZb3Ugc2hvdWxkIGFzayB0aGUgaW5zdXJhbmNlIHByb2R1Y2VyIG9yICkgXSBUSgpUKgotMC4wMDE0MCBUdwpbIChjb20pIDguNDAwMDAgKHApIDAuNjAwMDAgKGFueSB0aGF0IHNvbGQgeW91IHlvdXIgZXhpc3RpbmcgcG9saWMpIF0gVEoKMTYuODQwMDAgMCBURAowLjAwMDMwIFRjCi0wLjAwMDMwIFR3ClsgKHkgdG8gZ2l2ZSB5b3UgaW5mb3JtKSA4LjEwMDAwIChhdGlvbiBhYm91dCBpdFwwNTYgKSBdIFRKCi0xNi44NDAwMCAtMS4xNTAwMCBURAowIFRjCjAgVHcKKCApIFRqCjAgLTEuMTQ1MDAgVEQKLTAuMDAwMTAgVGMKMC4wNzg2MCBUdwpbICggKSAtMjY3MS41MDAwMCAoSGVhciBib3RoIHNpZGVzIGJlZm9yZSB5b3UgZGVjaWRlXDA1NiAgVGgpIF0gVEoKMTkuMDQ1MDAgMCBURAotMC4wMDAyMCBUYwowLjA3OTYwIFR3ClsgKGlzIHdheSB5b3UgY2FuIGJlIHN1cmUgeW91IGFyZSBtYWtpbmcgYSApIF0gVEoKLTE5LjA0NTAwIC0xLjE1MDAwIFRECjAuMDAwMjAgVGMKLTAuMDAxMDAgVHcKKGRlY2lzaW9uIHRoYXQgaXMgaW4geW91ciBiZXN0IGludGVyZXN0XDA1NiApIFRqClQqCjAgVGMKMCBUdwooICkgVGoKVCoKMC4wMDA1MCBUYwowLjI5NDUwIFR3ClsgKCApIC0yNDU1IChXZSBhcmUgcmVxdWlyZWQgYnkgbGF3IHRvIG5vdGlmeSB5b3VyKSBdIFRKCjIwLjIxNTAwIDAgVEQKMC4wMDA4MCBUYwowLjI5NTAwIFR3ClsgKCBleGlzdGluZyBjb20pIDguNjAwMDAgKHApIDAuODAwMDAgKGFueSB0aGF0IHlvdSBtKSA4LjYwMDAwIChhKSAtMC40MDAwMCAoeSBiZSkgLTUuNDAwMDAgKCApIF0gVEoKLTIwLjIxNTAwIC0xLjE1MDAwIFRECjAuMDAwMjAgVGMKLTAuMDAwMjAgVHcKKHJlcGxhY2luZyB0aGVpciBwb2xpY3lcMDU2ICkgVGoKMCAtMS4xNDUwMCBURAowIFRjCjAgVHcKKCApIFRqCjAgLTEuMTUwMDAgVEQKMC4wMDAxMCBUYwowLjE1NDkwIFR3ClsgKCApIC0yNTk1IChMaXN0IGJlbG93IHRoZSBpZGVudGlmaWNhdGlvbiBvZiBwb2xpY2llKSBdIFRKCjE5LjEzNTAwIDAgVEQKMC4wMDA3MCBUYwowLjE1MjYwIFR3ClsgKHMpIDQuOTAwMDAgKCB3aGljaCBhcmUgaW52bykgNS43MDAwMCAobHZlZCBpbiB0aGUgcmVwKSA1LjcwMDAwIChsKSAtMS41MDAwMCAoYWNlbSkgOC41MDAwMCAoZW50KSAtNi41MDAwMCAoICkgXSBUSgotMTkuMTM1MDAgLTEuMTUwMDAgVEQKMC4wMDE0MCBUYwowIFR3ClsgKGFjdGlvbikgNi40MDAwMCAoXDA3MikgNC4yMDAwMCAoICkgXSBUSgpUKgowIFRjCiggKSBUagpUKgowLjAwMDEwIFRjCi0wLjAwMDEwIFR3ClsgKCApIC0yNzUwIChDb250cmFjdCBOdW1iZXJcMDcyICBcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzcgKSBdIFRKCjAgLTEuMTQ1MDAgVEQKWyAoICkgLTI3NTAgKENvbnRyYWN0IE51bWJlclwwNzIgIFwxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzNyApIF0gVEoKMCAtMS4xNTAwMCBURApbICggKSAtMjc1MCAoQ29udHJhY3QgTnVtYmVyXDA3MiAgXDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3ICkgXSBUSgpUKgpbICggKSAtMjc1MCAoQ29udHJhY3QgTnVtYmVyXDA3MiAgXDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3ICkgXSBUSgpUKgowIFRjCjAgVHcKKCApIFRqClQqCiggKSBUagowIC0xLjE0NTAwIFREClsgKERhdGVcMDcyICBcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDA1NCAyMFwxMzdcMTM3ICkgLTE1ODUgKCApIC0yNzUwICggKSAtMjc1MCAoXDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzNyApIF0gVEoKMCAtMS4xNTAwMCBURAoyLjc1MDAwIFR3ClsgKCAgICAgICBJbnN1cmFuY2UgKSAyNzUwIChQcm9kdWNlclwyMjJzICkgMjc1MCAoUykgNi4xMDAwMCAoaSkgLTIuMjAwMDAgKGduYXR1cmUgKSBdIFRKClQqClsgKCAgICAgICBcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3XDEzN1wxMzdcMTM3ICkgXSBUSgpUKgowLjAwMDIwIFRjCjIuNzQ5ODAgVHcKWyAoICAgICAgIEluc3VyYW5jZSApIDI3NTAgKFByb2R1Y2VyXDIyMnMgKSAyNzUwIChQKSA2LjMwMDAwIChyKSAtMS44MDAwMCAoaW50ZWQgKSAyNzUwIChOYW0pIDggKGUpIC01LjkwMDAwICggKSBdIFRKClQqCjAgVGMKMCBUdwooICkgVGoKRVQKUQpxCjEgMCAwIDEgMCAwIGNtCkJUCi9GMSAxMiBUZgoxNC40MDAwMCBUTApFVAowIDAgMCBSRwowIDAgMCByZwpCVAovRjEgMTIgVGYKMTQuNDAw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AbZ5tvC3aLfguFm40blKucK6O7q1uy67p7whvJu9Fb2Pvgq+hL7/v3q/9cBwwOzBZ8Hjwl/C28NYw9TEUcTOxUvFyMZGxsPHQce/yD3IvMk6ybnKOMq3yzbLtsw1zLXNNc21zjbOts83z7jQOdC60TzRvtI/0sHTRNPG1EnUy9VO1dHWVdbY11zX4Nhk2OjZbNnx2nba+9uA3AXcit0Q3ZbeHN6i3ynfr+A24L3hROHM4lPi2+Nj4+vkc+T85YTmDeaW5x/nqegy6LzpRunQ6lvq5etw6/vshu0R7ZzuKO6070DvzPBY8OXxcvH/8ozzGfOn9DT0wvVQ9d72bfb794r4Gfio+Tj5x/pX+uf7d/wH/Jj9Kf26/kv+3P9t//8CDAD3hPP7CgplbmRzdHJlYW0KZW5kb2JqCjggMCBvYmoKPDwKL1R5cGUgL0V4dEdTdGF0ZQovU00gMC4wMjAwMAovb3AgZmFsc2UKL1NBIGZhbHNlCi9PUE0gMQovT1AgZmFsc2UKPj4KZW5kb2JqCjkgMCBvYmoKPDwKL0ZpcnN0Q2hhciAwCi9XaWR0aHMgWyA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIDYwMi41MzkxMCA2MDIuNTM5MTAgNjAyLjUzOTEwIF0KL1N1YnR5cGUgL1RydWVUeXBlCi9UeXBlIC9Gb250Ci9CYXNlRm9udCAvQUFBQUFBK0x1Y2lkYUNvbnNvbGUKL0xhc3RDaGFyIDEyNwovTmFtZSAv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b21hblBTTVQKL0xhc3RDaGFyIDE1MAovRm9udERlc2NyaXB0b3IgMTUgMCBSCi9TdWJ0eXBlIC9UcnVlVHlwZQo+PgplbmRvYmoKMTUgMCBvYmoKPDwKL0ZvbnRCQm94IFsgLTU2OCAtMzA3IDIwMDAgMTAwNyBdCi9TdGVtViA4MgovRGVzY2VudCAtMjE2Ci9YSGVpZ2h0IDAKL0ZsYWdzIDM0Ci9Gb250U3RyZXRjaCAvTm9ybWFsCi9Bc2NlbnQgODkxCi9Gb250TmFtZSAvVGltZXNOZXdSb21hblBTTVQKL1R5cGUgL0ZvbnREZXNjcmlwdG9yCi9Gb250V2VpZ2h0IDQwMAovSXRhbGljQW5nbGUgMAovRm9udEZhbWlseSAoVGltZXMgTmV3IFJvbWFuKQovQ2FwSGVpZ2h0IDY1Ngo+PgplbmRvYmoKeHJlZgowIDE2CjAwMDAwMDAwMDAgNjU1MzUgZiAKMDAwMDAwMDAwOSAwMDAwMCBuIAowMDAwMDAwMDY4IDAwMDAwIG4gCjAwMDAwMDAxMDggMDAwMDAgbiAKMDAwMDAwMDQxMyAwMDAwMCBuIAowMDAwMDAwNDYyIDAwMDAwIG4gCjAwMDAwMDQ4MDMgMDAwMDAgbiAKMDAwMDAwNDgzOCAwMDAwMCBuIAowMDAwMDA3NTEzIDAwMDAwIG4gCjAwMDAwMDc2MDAgMDAwMDAgbiAKMDAwMDAwOTA1NiAwMDAwMCBuIAowMDAwMDA5MjkwIDAwMDAwIG4gCjAwMDAwMjExNTggMDAwMDAgbiAKMDAwMDAyMTk0NSAwMDAwMCBuIAowMDAwMDIyMDUzIDAwMDAwIG4gCjAwMDAwMjI1NzUgMDAwMDAgbiAKdHJhaWxlcgo8PAovU2l6ZSAxNgovUm9vdCA0IDAgUgovSW5mbyAyIDAgUgo+PgpzdGFydHhyZWYKMjI4MzYKJSVFT0YK',
                'pages': '1', 'documentId': '1'},
            'inlineTemplates': [{'recipients': {
                'carbonCopies': [
                    {'templateRequired': False,
                     'name': 'Test CC Recipient',
                     'routingOrder': '2', 'tabs': {},
                     'roleName': 'None',
                     'recipientId': '2',
                     'email': 'zmason@delmarsd.com'}]},
                'sequence': '1'}]}
    ],
                              'emailSubject': 'Enrollment imported: FPPTI for Joe Smith (Another Test Case)',
                              'accountID': '5988eb5b-bee1-4825-a078-dcac445a22ce'})
    # envelope_result = create_envelope(email_subject="testing: signature needed",
    #                                  components=[general_template, child_attachment_form],
    #                                  docusign_transport=transport,
    #                                 )

    # url = envelope_result.get_signing_url(employee, callback_url='https://5starenroll.com', docusign_transport=transport)

    # print(url)
