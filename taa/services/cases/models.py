from datetime import datetime, timedelta

from sqlalchemy.dialects.postgresql import JSON

from taa import db
from taa.helpers import JsonSerializable

# The product selection for a given case
case_products = db.Table('case_products', db.metadata,
                         db.Column('case_id', db.Integer,
                                   db.ForeignKey('cases.id'),
                                   primary_key=True),
                         db.Column('product_id', db.Integer,
                                   db.ForeignKey('products.id'),
                                   primary_key=True),
                         )

case_partner_agents = db.Table('case_partner_agents', db.metadata,
                               db.Column('case_id', db.Integer,
                                         db.ForeignKey('cases.id'),
                                         primary_key=True),
                               db.Column('agent_id', db.Integer,
                                         db.ForeignKey('agents.id'),
                                         primary_key=True),
                               )

# Index going the other way so both lookups can be sped up.
db.Index('ix_case_partners_agent', case_partner_agents.c.agent_id)


class CaseSerializer(JsonSerializable):
    __json_modifiers__ = {
        'products': lambda products, _: [p for p in products],
        'enrollment_periods': lambda periods, _: [p for p in periods],
        'partner_agents': lambda agents, _: [a for a in agents],
         # When serializing active, make sure we take into account the enrollment period.
        'active': lambda _, case: case.can_enroll()
    }
    __json_hidden__ = ['census_records', 'enrollment_records', 'batches']


class Case(CaseSerializer, db.Model):
    """
    Model an agent's case describing enrollments at a company
    """
    __tablename__ = 'cases'

    id = db.Column(db.Integer, primary_key=True)
    company_name = db.Column(db.String, nullable=False)
    group_number = db.Column(db.String, nullable=True)
    situs_state = db.Column(db.String(2), nullable=True)
    situs_city = db.Column(db.String)
    agent_id = db.Column(db.Integer, db.ForeignKey('agents.id'), nullable=True)
    owner_agent = db.relationship('Agent', backref='owned_cases')
    active = db.Column(db.Boolean, default=False, index=True)
    created_date = db.Column(db.DateTime)
    is_stp = db.Column(db.Boolean, default=False)
    enrollment_period_type = db.Column(db.String(16), nullable=True)
    # Note: this flag is used for a few other restrictions now, and has a
    # broader meaning that a partner agent can view census data for only
    # records he has enrolled.
    can_partners_download_enrollments = db.Column(db.Boolean, default=True)
    OPEN_ENROLLMENT_TYPE = u'open'
    ANNUAL_ENROLLMENT_TYPE = u'annual'
    # This relationship defines what products are explicitly enabled for
    # a given case
    products = db.relationship('Product', secondary=case_products,
                               backref=db.backref('cases', lazy='dynamic'))
    partner_agents = db.relationship('Agent', secondary=case_partner_agents,
                               backref=db.backref('partner_cases',
                                                  lazy='dynamic'))
    payment_mode = db.Column(db.Integer, nullable=True)
    is_self_enrollment = db.Column(db.Boolean, server_default='FALSE',
                                   nullable=False)
    self_enrollment_setup = db.relationship('SelfEnrollmentSetup',
                                            uselist=False, backref='case')
    case_token = db.Column(db.String(64), nullable=True, index=True)

    # Store settings for the products as JSON. Includes rider settings and rate overrides.
    product_settings = db.Column(JSON(none_as_null=False), nullable=True)
    include_bank_draft_form = db.Column(db.Boolean, nullable=False, server_default='FALSE')
    occupation_class_settings = db.Column(JSON(none_as_null=False),
                                          nullable=True)

    # Call center workflow setting
    should_use_call_center_workflow = db.Column(db.Boolean, server_default='FALSE', nullable=False)


    def get_product_names(self):
        return ','.join(p.name for p in self.products)

    def format_owner_name(self):
        if self.owner_agent:
            return self.owner_agent.name()
        else:
            return "(No Owner)"

    def format_location(self):
        if not self.situs_city and not self.situs_state:
            return ''
        elif not self.situs_city:
            return self.situs_state

        return "{0}, {1}".format(self.situs_city, self.situs_state)

    def format_is_active(self):
        return "Active" if self.can_enroll() else "Not Active"

    def can_enroll(self):
        return (self.active and
                any(p.currently_active() for p in self.enrollment_periods) and
                self.has_signing_agent()
                )

    def has_signing_agent(self):
        # Require an owner agent, even if we have a self-enroll enrolling agent.
        return bool(self.owner_agent)

    def format_created_date(self):
        return self.created_date.strftime('%m/%d/%Y')

    def can_partner_agent_download_enrollments(self):
        if self.can_partners_download_enrollments is None:
            return True
        else:
            return self.can_partners_download_enrollments




class PeriodSerializer(JsonSerializable):
    __json_hidden__ = ['case']
    __json_modifiers__ = {
        # Use date strings rather than datetime strings
        'start_date': lambda d, _: d.strftime('%Y-%m-%d') if d else None,
        'end_date': lambda d, _: d.strftime('%Y-%m-%d') if d else None
    }


class CaseEnrollmentPeriod(PeriodSerializer, db.Model):
    __tablename__ = 'case_enrollment_periods'

    id = db.Column(db.Integer, primary_key=True)
    case_id = db.Column(db.Integer, db.ForeignKey('cases.id'), index=True)
    period_type = db.Column(db.String(32))
    __mapper_args__ = {
        'polymorphic_on': period_type,
        'with_polymorphic': '*',
    }
    start_date = db.Column(db.DateTime)
    end_date = db.Column(db.DateTime)
    case = db.relationship('Case', backref=db.backref('enrollment_periods'))


class CaseOpenEnrollmentPeriod(CaseEnrollmentPeriod):
    PERIOD_TYPE = u'open_with_start'
    __mapper_args__ = {'polymorphic_identity': PERIOD_TYPE}

    def populate_data_dict(self, data):
        data['enrollment_period_type'] = Case.OPEN_ENROLLMENT_TYPE
        data['open_period_start_date'] = self.start_date if self.start_date else ''
        data['open_period_end_date'] = self.end_date if self.end_date else ''
        return data

    def currently_active(self):
        now = datetime.now()
        if not self.start_date:
            # This is required
            return False

        if self.start_date < now and not self.end_date:
            # End date is blank and start date is in the past
            return True
        elif self.end_date and self.start_date < now < (self.end_date + timedelta(days=1)):
            # Now is between start and end date
            return True
        else:
            return False

    def get_start_date(self):
        return self.start_date

    def get_end_date(self):
        return self.end_date


class CaseAnnualEnrollmentPeriod(CaseEnrollmentPeriod):
    PERIOD_TYPE = u'annual_period'
    __mapper_args__ = {'polymorphic_identity': PERIOD_TYPE}

    def populate_data_dict(self, data):
        data['enrollment_period_type'] = Case.ANNUAL_ENROLLMENT_TYPE
        if 'annual_period_dates' not in data:
            data['annual_period_dates'] = []
        data['annual_period_dates'].append({
            'period_start_date':
                self.start_date.strftime('%m/%d') if self.start_date else '',
            'period_end_date':
                self.end_date.strftime('%m/%d') if self.end_date else '',
        })
        return data

    def currently_active(self):
        # Need to set the year for the start and end dates to current year
        if not self.start_date or not self.end_date:
            return False
        return self.get_start_date() <= datetime.now() < self.get_end_date()

    def get_start_date(self):
        current_year = datetime.now().year
        return datetime(self._current_year(), self.start_date.month,
                        self.start_date.day) if self.start_date else None

    def get_end_date(self):
        return datetime(self._current_year(), self.end_date.month,
                        self.end_date.day) if self.end_date else None

    def _current_year(self):
        return datetime.now().year


class CensusRecordSerializer(JsonSerializable):
    __json_modifiers__ = {
        # 'enrollment_applications': lambda apps, _: [a for a in apps]
    }
    __json_add__ = {
        'enrollment_status': lambda record: record.get_enrollment_status(),
        'sent_email': lambda record: record.sent_email_count,
    }
    __json_hidden__ = ['case', 'enrollment_applications', 'self_enrollment_links', 'email_logs']

    def get_enrollment_status(self):
        from taa.services.enrollments import EnrollmentApplicationService
        enrollments = EnrollmentApplicationService()
        return enrollments.get_enrollment_status(self)

    def get_sent_email(self):
        from taa.services.enrollments import SelfEnrollmentEmailService, SelfEnrollmentEmailLog
        email_logs = SelfEnrollmentEmailService().get_for_census_record(self)
        return any(email.status == SelfEnrollmentEmailLog.STATUS_SUCCESS for email in email_logs)

class CaseCensus(CensusRecordSerializer, db.Model):
    __tablename__ = 'case_census'

    id = db.Column(db.Integer, primary_key=True)
    case_id = db.Column(db.Integer, db.ForeignKey('cases.id'), nullable=True, index=True)
    case = db.relationship('Case', backref=db.backref('census_records'))
    upload_date = db.Column(db.DateTime, server_default=db.func.now())
    is_uploaded_census = db.Column(db.Boolean, server_default='TRUE')
    # Employee
    employee_ssn = db.Column(db.String(9))
    employee_first = db.Column(db.String(256))
    employee_last = db.Column(db.String(256))
    employee_gender = db.Column(db.String(6))
    employee_birthdate = db.Column(db.Date)
    employee_email = db.Column(db.String(256))
    employee_phone = db.Column(db.String(32))
    employee_height_inches = db.Column(db.String(16))
    employee_weight_lbs = db.Column(db.String(16))
    employee_smoker = db.Column(db.String(8))
    employee_street_address = db.Column(db.String(256))
    employee_street_address2 = db.Column(db.String(256))
    employee_city = db.Column(db.String(256))
    employee_state = db.Column(db.String(2))
    employee_zip = db.Column(db.String(5))
    # Spouse
    spouse_ssn = db.Column(db.String(9))
    spouse_first = db.Column(db.String(256))
    spouse_last = db.Column(db.String(256))
    spouse_gender = db.Column(db.String(6))
    spouse_birthdate = db.Column(db.Date)
    spouse_email = db.Column(db.String(256))
    spouse_phone = db.Column(db.String(32))
    spouse_height_inches = db.Column(db.String(16))
    spouse_weight_lbs = db.Column(db.String(16))
    spouse_smoker = db.Column(db.String(8))
    spouse_street_address = db.Column(db.String(256))
    spouse_street_address2 = db.Column(db.String(256))
    spouse_city = db.Column(db.String(256))
    spouse_state = db.Column(db.String(2))
    spouse_zip = db.Column(db.String(5))
    # Child 1
    child1_first = db.Column(db.String(256))
    child1_last = db.Column(db.String(256))
    child1_birthdate = db.Column(db.Date)
    # Child 2
    child2_first = db.Column(db.String(256))
    child2_last = db.Column(db.String(256))
    child2_birthdate = db.Column(db.Date)
    # Child 3
    child3_first = db.Column(db.String(256))
    child3_last = db.Column(db.String(256))
    child3_birthdate = db.Column(db.Date)
    # Child 4
    child4_first = db.Column(db.String(256))
    child4_last = db.Column(db.String(256))
    child4_birthdate = db.Column(db.Date)
    # Child 5
    child5_first = db.Column(db.String(256))
    child5_last = db.Column(db.String(256))
    child5_birthdate = db.Column(db.Date)
    # Child 6
    child6_first = db.Column(db.String(256))
    child6_last = db.Column(db.String(256))
    child6_birthdate = db.Column(db.Date)
    occupation_class = db.Column(db.String(256))

    def get_smoker_boolean(self, value):
        if value == 'Y':
            return True
        elif value == 'N':
            return False
        else:
            return None

    def get_employee_data(self):
        from taa.services.enrollments import EnrollmentApplicationCoverageService
        employee_coverages = EnrollmentApplicationCoverageService().get_coverages_for_employee(self)
        return dict(
            first=self.employee_first,
            last = self.employee_last,
            ssn = self.employee_ssn,
            birthdate=self.format_date(self.employee_birthdate),
            email=self.employee_email,
            phone=self.employee_phone,
            gender=self.employee_gender.lower() if self.employee_gender else '',
            weight=self.employee_weight_lbs,
            height=self.employee_height_inches,
            is_smoker=self.get_smoker_boolean(self.employee_smoker),
            street_address=self.employee_street_address,
            street_address2=self.employee_street_address2,
            city=self.employee_city,
            state=self.employee_state,
            zip=self.employee_zip,
            existing_coverages=employee_coverages,
        )

    def get_spouse_data(self):
        from taa.services.enrollments import EnrollmentApplicationCoverageService
        spouse_coverages = EnrollmentApplicationCoverageService().get_coverages_for_spouse(self)
        return dict(
                first=self.spouse_first,
                last=self.spouse_last,
                ssn=self.spouse_ssn,
                birthdate=self.format_date(self.spouse_birthdate),
                email=self.spouse_email,
                phone=self.spouse_phone,
                gender=self.spouse_gender.lower() if self.spouse_gender else '',
                weight=self.spouse_weight_lbs,
                height=self.spouse_height_inches,
                is_smoker=self.get_smoker_boolean(self.spouse_smoker),
                street_address=self.spouse_street_address,
                street_address2=self.spouse_street_address2,
                city=self.spouse_city,
                state=self.spouse_state,
                zip=self.spouse_zip,
                existing_coverages=spouse_coverages,
            )

    def get_children_data(self):
        children = []
        from taa.services.enrollments import EnrollmentApplicationCoverageService
        children_coverages = EnrollmentApplicationCoverageService().get_coverages_for_children(self)
        for num in range(1, 6+1):
            if self.has_child(num):
                children.append(dict(
                    first=self.child_first(num),
                    last=self.child_last(num),
                    birthdate=self.format_date(self.child_birthdate(num)),
                    existing_coverages=children_coverages,
                ))
        return children

    def format_date(self, date):
        if not date:
            return ''
        return date.strftime('%m/%d/%Y')

    def has_child(self, num):
        return (
            self.child_first(num) and
            self.child_last(num) and
            self.child_birthdate(num)
        )

    def child_first(self, num):
        return getattr(self, 'child{}_first'.format(num))

    def child_last(self, num):
        return getattr(self, 'child{}_last'.format(num))

    def child_birthdate(self, num):
        return getattr(self, 'child{}_birthdate'.format(num))

class AgentSplitsSerializer(JsonSerializable):
    __json_hidden__ = ['case']

class AgentSplitsSetup(AgentSplitsSerializer, db.Model):
    """
    Model a case's agent commission splits
    """
    __tablename__ = 'agent_split_setups'

    id = db.Column(db.Integer, primary_key=True)
    # Case
    case_id = db.Column(db.Integer, db.ForeignKey('cases.id'), nullable=True, index=True)
    case = db.relationship('Case', backref='agent_splits')
    agent_id = db.Column(db.Integer, db.ForeignKey('agents.id'), nullable=True)
    product_id = db.Column(db.Integer, db.ForeignKey('products.id'))
    commission_subcount_code = db.Column(db.String, server_default="", default="")
    split_percentage = db.Column(db.Integer)


class SelfEnrollmentSerializer(JsonSerializable):
    __json_hidden__ = ['case', 'links', 'enrolling_agent']


class SelfEnrollmentSetup(SelfEnrollmentSerializer, db.Model):
    """
    Model a case's self-enrollment email and landing page templates.
    """
    __tablename__ = 'self_enrollment_setups'

    id = db.Column(db.Integer, primary_key=True)
    # Case
    case_id = db.Column(db.Integer, db.ForeignKey('cases.id'), nullable=True, index=True)
    # Type
    TYPE_CASE_TARGETED = 'case-targeted'
    TYPE_CASE_GENERIC = 'case-generic'
    self_enrollment_type = db.Column(db.Unicode(16), nullable=False)
    # Email
    use_email = db.Column(db.Boolean, nullable=False, server_default='FALSE')
    email_sender_name = db.Column(db.Unicode)
    email_sender_email = db.Column(db.Unicode)
    email_greeting_salutation = db.Column(db.Unicode)

    email_greeting_type = db.Column(db.Unicode(16))
    EMAIL_GREETING_FIRST_NAME = 'firstname'
    EMAIL_GREETING_FULL_NAME = 'fullname'
    EMAIL_GREETING_LAST_NAME = 'lastname'
    EMAIL_GREETING_TITLE_LAST = 'titlelast'
    EMAIL_GREETING_BLANK = 'blank'

    email_subject = db.Column(db.Unicode)
    email_message = db.Column(db.UnicodeText)
    # Landing page
    use_landing_page = db.Column(db.Boolean, nullable=False,
                                 server_default='TRUE')
    page_title = db.Column(db.Unicode)
    page_text = db.Column(db.UnicodeText)
    page_disclaimer = db.Column(db.UnicodeText)
    # Links
    links = db.relationship('SelfEnrollmentLink')
    # Metadata
    created_by = db.Column(db.Integer, db.ForeignKey('agents.id'),
                           nullable=True)
    created_date = db.Column(db.DateTime, nullable=False, default=db.func.now())
    updated_date = db.Column(db.DateTime, nullable=False, default=db.func.now())

    # Enrolling agent for self-enroll.
    enrolling_agent_id = db.Column(db.Integer, db.ForeignKey('agents.id'), nullable=True)
    enrolling_agent = db.relationship('Agent', foreign_keys=[enrolling_agent_id])

    @property
    def sent_count(self):
        if self.self_enrollment_type != self.TYPE_CASE_TARGETED:
            return 0
        if self.links is None:
            return 0
        return len(self.links)

    @property
    def enrolled_count(self):
        if self.self_enrollment_type != self.TYPE_CASE_TARGETED:
            return 0
        return len(filter(lambda x: x.get_enrollment_status() == 'enrolled',
                          self.case.census_records))

    @property
    def declined_count(self):
        if self.self_enrollment_type != self.TYPE_CASE_TARGETED:
            return 0
        return len(filter(lambda x: x.get_enrollment_status() != 'enrolled',
                          self.case.census_records))
