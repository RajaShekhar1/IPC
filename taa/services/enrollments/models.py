from datetime import datetime

from taa import db
from taa.helpers import JsonSerializable


class EnrollmentSerializer(JsonSerializable):
    __json_hidden__ = ['census_record', 'case']


class EnrollmentApplication(EnrollmentSerializer, db.Model):
    """Describes an application made for an enrollment"""
    
    __tablename__ = 'enrollment_applications'
    
    id = db.Column(db.Integer, primary_key=True)
    
    case_id = db.Column(db.Integer, db.ForeignKey('cases.id'), nullable=True)
    case = db.relationship('Case', backref=db.backref('enrollment_records', lazy='dynamic'))
    
    census_record_id = db.Column(db.Integer, db.ForeignKey('case_census.id'), nullable=False)
    census_record = db.relationship('CaseCensus', backref=db.backref('enrollment_applications', lazy='joined'))
    
    signature_time = db.Column(db.DateTime, server_default='NOW')
    signature_city = db.Column(db.UnicodeText)
    signature_state = db.Column(db.Unicode(2))
    
    identity_token = db.Column(db.UnicodeText)
    identity_token_type = db.Column(db.Unicode(64))
    
    APPLICATION_STATUS_ENROLLED = u'enrolled'
    APPLICATION_STATUS_DECLINED = u'declined'
    application_status = db.Column(db.Unicode(32))
    
    payment_mode = db.Column(db.Integer(), nullable=True)

    METHOD_INPERSON = u'in_person'
    METHOD_SELF_EMAIL = u'self_enroll_email'
    METHOD_PHONE = u'phone'
    method = db.Column(db.Unicode(32)) 
    
    # Agent
    agent_id = db.Column(db.Integer, db.ForeignKey('agents.id'), nullable=True)
    agent_code = db.Column(db.Unicode(16))
    agent_name = db.Column(db.Unicode(256))
    
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
    
    def get_signature_time_as_int(self):
        'useful for sorting'
        
    def did_enroll(self):
        return self.application_status == self.APPLICATION_STATUS_ENROLLED

    def did_process(self):
        return self.application_status != None

class EnrollmentApplicationCoverageSerializer(JsonSerializable):
    __json_hidden__ = ['enrollment']
    
class EnrollmentApplicationCoverage(EnrollmentApplicationCoverageSerializer, db.Model):
    
    id = db.Column(db.Integer, primary_key=True)
    
    enrollment_application_id = db.Column(db.Integer, db.ForeignKey('enrollment_applications.id'), nullable=False)
    enrollment = db.relationship('EnrollmentApplication', backref=db.backref('coverages'))
    
    product_id = db.Column(db.Integer, db.ForeignKey('products.id'))
    product = db.relationship('Product')
    
    APPLICANT_TYPE_EMPLOYEE = u'employee'
    APPLICANT_TYPE_SPOUSE = u'spouse'
    APPLICANT_TYPE_CHILD = u'children'
    applicant_type = db.Column(db.Unicode(32))
    
    # Coverage status
    COVERAGE_STATUS_ENROLLED = u'enrolled'
    COVERAGE_STATUS_DECLINED = u'declined'
    coverage_status = db.Column(db.Unicode(32))
    
    # Additional non-census data
    height_inches = db.Column(db.Integer)
    weight_pounds = db.Column(db.Integer)
    is_smoker = db.Column(db.Boolean)
    
    # Coverage Selected
    coverage_face_value = db.Column(db.Unicode(256))
    
    # All specified in dollars. No precision (num digits) or scale (num decimal digits) means no coercing
    weekly_premium = db.Column(db.Numeric)
    biweekly_premium = db.Column(db.Numeric)
    monthly_premium = db.Column(db.Numeric)
    semimonthly_premium = db.Column(db.Numeric)
    annual_premium = db.Column(db.Numeric)
    
    # SOH Question answers stored as a JSON array of 
    #  objects {question:"", answer:""}
    soh_answers = db.Column(db.UnicodeText)

    def get_annualized_premium(self):
        if self.annual_premium is not None:
            return self.annual_premium
        elif self.monthly_premium is not None:
            return self.monthly_premium * 12
        elif self.semimonthly_premium is not None:
            return self.monthly_premium * 24
        elif self.biweekly_premium is not None:
            return self.biweekly_premium * 26
        elif self.weekly_premium is not None:
            return self.weekly_premium * 52
        else:
            return 0.0

    def did_enroll(self):
        return self.coverage_status == self.COVERAGE_STATUS_ENROLLED
    
    
# enrollment_requests = Table('enrollment_requests', metadata,
#     Column('id', Integer, primary_key=True),
#     Column('enrollment_id', Integer, ForeignKey('enrollments.id'), nullable=False),
#     Column('token', String, nullable=False),
#     Column('expiration', Date, nullable=False),
# )
# 
# enrollment_elections = Table('enrollment_elections', metadata,
#     Column('id', Integer, primary_key=True),
#     Column('enrollment_id', Integer, ForeignKey('enrollments.id')),
#     Column('product_id', Integer, ForeignKey('products.id')),
#     
#     Column('coverage_taken', Boolean),
#     
#     Column('total_annual_premium', Numeric),
#     Column('employee_coverage', Numeric),
#     Column('employee_annual_premium', Numeric),
#     Column('spouse_coverage', Numeric),
#     Column('spouse_annual_premium', Numeric),
# 
#     Column('children_coverage', Numeric),
#     Column('children_annual_premium', Numeric),
#     
# )