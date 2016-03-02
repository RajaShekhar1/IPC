import decimal

from sqlalchemy.dialects.postgresql import JSON
from taa.services.agents import ApiTokenService

from taa import db
from taa.helpers import JsonSerializable
from taa.services.cases import CaseCensus, CaseService


class EnrollmentSerializer(JsonSerializable):
    __json_hidden__ = ['census_record', 'case']


class EnrollmentApplication(EnrollmentSerializer, db.Model):
    """Describes an application made for an enrollment"""
    __tablename__ = 'enrollment_applications'

    id = db.Column(db.Integer, primary_key=True)
    case_id = db.Column(db.Integer, db.ForeignKey('cases.id'), nullable=True, index=True)
    case = db.relationship('Case', backref=db.backref('enrollment_records',
                                                      lazy='dynamic'))
    census_record_id = db.Column(db.Integer, db.ForeignKey('case_census.id'),
                                 nullable=False, index=True)
    census_record = db.relationship('CaseCensus',
                                    backref=db.backref(
                                        'enrollment_applications',
                                        lazy='joined'))
    signature_time = db.Column(db.DateTime, index=True)
    signature_city = db.Column(db.UnicodeText)
    signature_state = db.Column(db.Unicode(2))
    identity_token = db.Column(db.UnicodeText)
    identity_token_type = db.Column(db.Unicode(64))
    # Application status
    APPLICATION_STATUS_ENROLLED = u'enrolled'
    APPLICATION_STATUS_PENDING_AGENT = u'pending_agent'
    APPLICATION_STATUS_PENDING_EMPLOYEE = u'pending_employee'
    APPLICATION_STATUS_DECLINED = u'declined'
    APPLICATION_STATUS_VOIDED = u'voided'
    application_status = db.Column(db.Unicode(32))
    # Payment mode
    payment_mode = db.Column(db.Integer(), nullable=True)
    METHOD_INPERSON = u'in_person'
    METHOD_SELF_EMAIL = u'self_enroll_email'
    METHOD_PHONE = u'phone'
    method = db.Column(db.Unicode(32))
    # Agent
    agent_id = db.Column(db.Integer, db.ForeignKey('agents.id'), nullable=True)
    agent_code = db.Column(db.Unicode(16))
    agent_name = db.Column(db.Unicode(256))

    # Helpful for querying for all records an agent is allowed to see within a given case
    db.Index('ix_enrollment_applications_agent_case', agent_id, case_id)

    # Policy owner
    is_employee_owner = db.Column(db.Boolean)
    employee_other_owner_name = db.Column(db.UnicodeText)
    employee_other_owner_ssn = db.Column(db.Unicode(16))
    is_spouse_owner = db.Column(db.Boolean)
    spouse_other_owner_name = db.Column(db.UnicodeText)
    spouse_other_owner_ssn = db.Column(db.Unicode(16))
    # Employee beneficiary
    is_employee_beneficiary_spouse = db.Column(db.Boolean)
    employee_beneficiary_name = db.Column(db.UnicodeText)
    employee_beneficiary_relationship = db.Column(db.UnicodeText)
    employee_beneficiary_birthdate = db.Column(db.UnicodeText)
    employee_beneficiary_ssn = db.Column(db.Unicode(16))
    # Spouse beneficiary
    is_spouse_beneficiary_employee = db.Column(db.Boolean)
    spouse_beneficiary_name = db.Column(db.UnicodeText)
    spouse_beneficiary_relationship = db.Column(db.UnicodeText)
    spouse_beneficiary_birthdate = db.Column(db.UnicodeText)
    spouse_beneficiary_ssn = db.Column(db.Unicode(16))

    # Save the raw data that we receive
    received_data = db.Column(JSON(none_as_null=False))
    standardized_data = db.Column(JSON(none_as_null=False))

    docusign_envelope_id = db.Column(db.Unicode(128), index=True)
    agent_signing_status = db.Column(db.Unicode(32))
    agent_signing_datetime = db.Column(db.DateTime)
    applicant_signing_status = db.Column(db.Unicode(32))
    applicant_signing_datetime = db.Column(db.DateTime)

    SIGNING_STATUS_PENDING = u'pending'
    SIGNING_STATUS_DECLINED = u'declined_to_sign'
    SIGNING_STATUS_TIMEOUT = u'timeout'
    SIGNING_STATUS_ERROR = u'error'
    SIGNING_STATUS_TTL_ERROR = u'ttl_expired'
    SIGNING_STATUS_COMPLETE = u'signed'
    SIGNING_STATUS_NA = u'not_applicable'

    def is_pending(self):
        return self.is_pending_employee() or self.is_pending_agent()

    def is_pending_employee(self):
        return self.application_status in [self.APPLICATION_STATUS_PENDING_EMPLOYEE]

    def is_pending_agent(self):
        return self.application_status in [self.APPLICATION_STATUS_PENDING_AGENT]

    def did_decline(self):
        return self.application_status == self.APPLICATION_STATUS_DECLINED

    def did_enroll(self):
        return self.application_status == self.APPLICATION_STATUS_ENROLLED

    def did_process(self):
        # Pending does not count as processed.
        return self.application_status in [self.APPLICATION_STATUS_DECLINED, self.APPLICATION_STATUS_ENROLLED]

    def is_terminal_status(self):
        return self.application_status in [
            self.APPLICATION_STATUS_DECLINED,
            self.APPLICATION_STATUS_ENROLLED,
            self.APPLICATION_STATUS_VOIDED,
        ]



class EnrollmentApplicationCoverageSerializer(JsonSerializable):
    __json_hidden__ = ['enrollment']


class EnrollmentApplicationCoverage(EnrollmentApplicationCoverageSerializer,
                                    db.Model):
    __tablename__ = 'enrollment_application_coverage'
    id = db.Column(db.Integer, primary_key=True)
    enrollment_application_id = db.Column(db.Integer,
                                          db.ForeignKey(
                                              'enrollment_applications.id'),
                                          nullable=False, index=True)
    enrollment = db.relationship('EnrollmentApplication',
                                 backref=db.backref('coverages'))
    # Product
    product_id = db.Column(db.Integer, db.ForeignKey('products.id'))
    product = db.relationship('Product')
    # Applicant type
    APPLICANT_TYPE_EMPLOYEE = u'employee'
    APPLICANT_TYPE_SPOUSE = u'spouse'
    APPLICANT_TYPE_CHILD = u'children'
    applicant_type = db.Column(db.Unicode(32))
    # Coverage status
    COVERAGE_STATUS_ENROLLED = u'enrolled'
    COVERAGE_STATUS_DECLINED = u'declined'
    coverage_status = db.Column(db.Unicode(32))
    coverage_selection = db.Column(db.Unicode(32))
    # Additional non-census data
    height_inches = db.Column(db.Integer)
    weight_pounds = db.Column(db.Integer)
    is_smoker = db.Column(db.Boolean)
    # Coverage Selected
    coverage_face_value = db.Column(db.Unicode(256))
    # All specified in dollars. No precision (num digits) or scale (num
    # decimal digits) means no coercing
    weekly_premium = db.Column(db.Numeric)
    biweekly_premium = db.Column(db.Numeric)
    monthly_premium = db.Column(db.Numeric)
    semimonthly_premium = db.Column(db.Numeric)
    annual_premium = db.Column(db.Numeric)
    # SOH Question answers stored as a JSON array of
    # objects {question: '', answer: ''}
    soh_answers = db.Column(db.UnicodeText)

    def get_annualized_premium(self):
        if self.annual_premium is not None:
            return self.annual_premium
        elif self.monthly_premium is not None:
            return self.monthly_premium * 12
        elif self.semimonthly_premium is not None:
            return self.semimonthly_premium * 24
        elif self.biweekly_premium is not None:
            return self.biweekly_premium * 26
        elif self.weekly_premium is not None:
            return self.weekly_premium * 52
        else:
            return decimal.Decimal('0.00')

    def get_premium(self):
        if self.annual_premium is not None:
            return self.annual_premium
        elif self.monthly_premium is not None:
            return self.monthly_premium
        elif self.semimonthly_premium is not None:
            return self.semimonthly_premium
        elif self.biweekly_premium is not None:
            return self.biweekly_premium
        elif self.weekly_premium is not None:
            return self.weekly_premium
        else:
            return decimal.Decimal('0.00')

    def did_enroll(self):
        return self.coverage_status == self.COVERAGE_STATUS_ENROLLED


class SelfEnrollmentLinkSerializer(JsonSerializable):
    __json_hidden__ = ['census_record', 'case', 'emails', 'self_enrollment_setup']

class SelfEnrollmentLink(SelfEnrollmentLinkSerializer, db.Model):
    __tablename__ = 'self_enrollment_links'

    id = db.Column(db.Integer, primary_key=True)
    census_record_id = db.Column(db.Integer, db.ForeignKey('case_census.id'),
                                 nullable=True)
    census_record = db.relationship('CaseCensus',
                                    backref='self_enrollment_links'
     )
    self_enrollment_setup_id = db.Column(db.Integer,
                                         db.ForeignKey(
                                             'self_enrollment_setups.id'),
                                         nullable=False)
    self_enrollment_setup = db.relationship('SelfEnrollmentSetup')
    url = db.Column(db.Unicode(2000), nullable=False, index=True)
    clicks = db.Column(db.Integer, server_default='0', nullable=False)
    emails = db.relationship('SelfEnrollmentEmailLog', backref='link')


    def is_active(self):
        return self.is_linked() and self.is_case_active() and self.is_self_enroll_active()

    def is_self_enroll_active(self):
        return self.self_enrollment_setup.case.is_self_enrollment

    def is_case_active(self):
        return self.self_enrollment_setup.case.can_enroll()

    def is_linked(self):
        return self.self_enrollment_setup and self.self_enrollment_setup.case

class SelfEnrollmentEmailBatchSerializer(JsonSerializable):
    __json_hidden__ = ['email_logs', 'case']
    __json_add__ = {
        'email_count': lambda batch: batch.get_email_count()
    }
    def get_email_count(self):
        return db.session.query(SelfEnrollmentEmailLog
        ).filter_by(batch_id=self.id
        ).count()

class SelfEnrollmentEmailBatchSerializerWithEmails(JsonSerializable):
    __json_hidden__ = ['case']


class _SelfEnrollmentEmailBatch(db.Model):
    __tablename__ = "self_enrollment_email_batches"

    id = db.Column(db.Integer, primary_key=True)

    agent_id = db.Column(db.Integer, db.ForeignKey('agents.id'), nullable=False)

    email_from_address = db.Column(db.Unicode)
    email_from_name = db.Column(db.Unicode)
    email_subject = db.Column(db.Unicode)
    email_body = db.Column(db.UnicodeText)

    sent_date = db.Column(db.DateTime, nullable=False, default=db.func.now())

    case_id = db.Column(db.Integer, db.ForeignKey('cases.id'), nullable=False, index=True)
    case = db.relationship('Case', backref="batches")

class SelfEnrollmentEmailBatchWithEmails(SelfEnrollmentEmailBatchSerializerWithEmails, _SelfEnrollmentEmailBatch):
    pass

#This class doesn't serialize emails.
class SelfEnrollmentEmailBatch(SelfEnrollmentEmailBatchSerializer, _SelfEnrollmentEmailBatch):
    pass

class SelfEnrollmentEmailLogSerializer(JsonSerializable):
    __json_hidden__ = ['agent', 'census_record', 'link', 'batch']


class SelfEnrollmentEmailLog(SelfEnrollmentEmailLogSerializer, db.Model):
    __tablename__ = 'self_enrollment_email_log'

    id = db.Column(db.Integer, primary_key=True)
    link_id = db.Column(db.Integer, db.ForeignKey('self_enrollment_links.id'),
                        nullable=False, index=True)
    census_id = db.Column(db.Integer, db.ForeignKey('case_census.id'),
                          nullable=False, index=True)
    census_record = db.relationship('CaseCensus', backref='email_logs')
    agent_id = db.Column(db.Integer, db.ForeignKey('agents.id'), nullable=False)
    sent_date = db.Column(db.DateTime, nullable=False, default=db.func.now())
    email_to_address = db.Column(db.Unicode)
    email_to_name = db.Column(db.Unicode)
    email_body = db.Column(db.Unicode)

    batch_id = db.Column(db.Integer, db.ForeignKey('self_enrollment_email_batches.id'), nullable=True, index=True)
    batch = db.relationship('_SelfEnrollmentEmailBatch', backref="email_logs")

    is_success = db.Column(db.Boolean, nullable=False)

    status = db.Column(db.Unicode(16), index=True)

    STATUS_PENDING = u'pending'
    STATUS_FAILURE = u'failure'
    STATUS_SUCCESS = u'success'

CaseCensus.sent_email_count = db.column_property(
    db.select([db.func.count(SelfEnrollmentEmailLog.id)]).\
            where(SelfEnrollmentEmailLog.census_id==CaseCensus.id).\
            where(db.or_(
                SelfEnrollmentEmailLog.status == SelfEnrollmentEmailLog.STATUS_SUCCESS,
                SelfEnrollmentEmailLog.status == SelfEnrollmentEmailLog.STATUS_PENDING,
            )).correlate_except(SelfEnrollmentEmailLog)
)


def get_batch_case_id(batch):
    token = batch.case_token
    if not token:
        return None

    case = CaseService().get_case_for_token(token)
    if not case:
        return None
    else:
        return case.id


def get_batch_user(batch):
    auth_token = batch.auth_token
    if not auth_token:
        return None

    api_token = ApiTokenService().find(api_token=auth_token).first()
    if not api_token:
        return None

    return api_token.stormpath_url


class EnrollmentImportBatchSerializer(JsonSerializable):
    __json_hidden__ = ['batch_items']
    __json_add__ = {
        'case_id': get_batch_case_id,
        'user_name': get_batch_user,
    }

class EnrollmentImportBatch(EnrollmentImportBatchSerializer, db.Model):
    """
    Records an attempt to submit a batch of enrollments.
    """
    __tablename__ = 'enrollment_import_batches'
    id = db.Column(db.Integer, primary_key=True)

    # Submission sources
    SUBMIT_SOURCE_DROPBOX = u'dropbox'
    SUBMIT_SOURCE_LOGGED_IN_WIZARD = u'logged_in_wizard'
    SUBMIT_SOURCE_SELF_ENROLL_WIZARD = u'self_enroll_wizard'
    SUBMIT_SOURCE_API = u'api'
    source = db.Column(db.Unicode(32), nullable=False)

    timestamp = db.Column(db.DateTime, server_default=db.func.now())
    auth_token = db.Column(db.Unicode(64))
    case_token = db.Column(db.Unicode(64), index=True)
    num_processed = db.Column(db.Integer)
    num_errors = db.Column(db.Integer)
    log_hash = db.Column(db.Unicode(64), index=True)


class EnrollmentImportBatchItemSerializer(JsonSerializable):
    __json_hidden__ = ["enrollment_record", "enrollment_batch"]


class EnrollmentImportBatchItem(EnrollmentImportBatchItemSerializer, db.Model):
    """
    Records the ID and status of each enrollment as part of an import request.
    """
    __tablename__ = 'enrollment_import_batch_items'

    id = db.Column(db.Integer, primary_key=True)
    enrollment_batch_id = db.Column(db.Integer, db.ForeignKey('enrollment_import_batches.id'), index=True)
    enrollment_batch = db.relationship('EnrollmentImportBatch', backref='batch_items')
    enrollment_record_id = db.Column(db.Integer, db.ForeignKey('enrollment_applications.id'))
    enrollment_record = db.relationship('EnrollmentApplication')

    STATUS_QUEUED = u'queued'
    STATUS_PROCESSING = u'processing'
    STATUS_ERROR = u'error'
    STATUS_SUCCESS = u'success'

    status = db.Column(db.Unicode(32))
    error_message = db.Column(db.UnicodeText)
    processed_time = db.Column(db.DateTime, server_default=db.func.now())


class FormTemplate(db.Model):
    """
    Stores the DocuSign-compatible XML data for generating PDFs of enrollment applications.
    """
    __tablename__ = 'form_templates'

    id = db.Column(db.Integer, primary_key=True)
    template_id = db.Column(db.Unicode, nullable=False)
    data = db.Column(db.LargeBinary)
    name = db.Column(db.Unicode)
    description = db.Column(db.Unicode)
    pages = db.Column(db.Integer, nullable=False)
    modified_at = db.Column(db.DateTime)

    db.Index('ix_form_templates_template_id', template_id, unique=True)


class FormTemplateTabs(db.Model):
    """
    The "tabs" are placeholders for applicant data on the enrollment PDF.
    """
    __tablename__ = 'form_template_tabs'

    id = db.Column(db.Integer, primary_key=True)
    form_template_id = db.Column(db.Integer, db.ForeignKey('form_templates.id'),
                                 nullable=False, index=True)
    template = db.relationship('FormTemplate', backref='tabs')
    page = db.Column(db.Integer, nullable=False)
    x = db.Column(db.Integer, nullable=False)
    y = db.Column(db.Integer, nullable=False)
    name = db.Column(db.Unicode)
    type_ = db.Column(db.Unicode)
    label = db.Column(db.Unicode)
    is_bold = db.Column(db.Boolean, server_default='FALSE')
    is_italic = db.Column(db.Boolean, server_default='FALSE')
    is_underline = db.Column(db.Boolean, server_default='FALSE')
    custom_type = db.Column(db.Unicode)
    width = db.Column(db.Integer)
    height = db.Column(db.Integer)
    font = db.Column(db.Unicode)
    font_size = db.Column(db.Integer)
    font_color = db.Column(db.Unicode)
    recipient_role = db.Column(db.Unicode)

    template_tab_locked = db.Column(db.Boolean)
    template_tab_required = db.Column(db.Boolean)
    custom_tab_locked = db.Column(db.Boolean)
    custom_tab_required = db.Column(db.Boolean)


