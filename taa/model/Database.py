
from sqlalchemy import (
    Table, 
    Column, 
    Integer,
    Boolean,
    String, 
    MetaData, 
    ForeignKey,
    Date,
    DateTime,
    Numeric,
)

from taa import db
from taa.model.Enrollment import Enrollment, EnrollmentRequest, Case

metadata = MetaData()
agents = Table('agents', metadata, 
    Column('id', Integer, primary_key=True),
    Column('agent_code', String(5), nullable=False),
    Column('activated', Boolean, nullable=False),
    Column('password', String),
    Column('first', String),
    Column('last', String),
    Column('email', String),
    Column('phone', String),
)
products = Table('products', metadata, 
    Column('id', Integer, primary_key=True),
    Column('code', String, nullable=False),
    Column('name', String, nullable=False),
)

cases = Table('cases', metadata,
    Column('id', Integer, primary_key=True),
    Column('company_name', String, nullable=False),
    Column('situs_state', String(2), nullable=False),
    #Column('product_id', Integer, ForeignKey('products.product_id'), nullable=False),
    Column('agent_id', Integer, ForeignKey('agents.id')),
    Column('active', Boolean),
)

ENROLLMENT_TYPES = ['annual_with_start', 'specific']
case_enrollment_periods = Table('case_enrollment_periods', metadata,
    Column('id', Integer, primary_key=True),
    Column('period_type', String(32)),
    Column('start_date', DateTime),
    Column('end_date', DateTime),
)

case_products = Table('case_products', metadata,
    Column('case_id', Integer, ForeignKey('cases.id'), primary_key=True),
    Column('product_id', Integer, ForeignKey('products.id'), primary_key=True),
)

census_employees = Table('census_employees', metadata,
    Column('id', Integer, primary_key=True),
    Column('upload_date', DateTime),
    Column('employee_ssn', String(9)),
    Column('employee_first', String(256)),
    Column('employee_last', String(256)),
    Column('employee_birthdate', Date),
    
    Column('employee_street_address', String(256)),
    Column('employee_street_address2', String(256)),
    Column('employee_city', String(256)),
    Column('employee_state', String(2)),
    Column('employee_zip', String(9)),
    
    Column('employee_email', String(256)),

    Column('spouse_ssn', String(9)),
    Column('spouse_first', String(256)),
    Column('spouse_last', String(256)),
    Column('spouse_address', String(512)),
    Column('spouse_birthdate', Date),
)

enrollments = Table('enrollments', metadata,
    Column('id', Integer, primary_key=True),
    Column('case_id', Integer, ForeignKey('cases.id'), nullable=False),
    Column('application_date', DateTime),
    Column('employee_first', String, nullable=False),
    Column('employee_last', String, nullable=False),
    Column('employee_ssn', String(9), nullable=True),
    Column('employee_email', String, nullable=False),
    # The token is a special string submitted with the application (like a driver's license number)
    Column('identity_token', String),
    Column('census_record_id', Integer, ForeignKey('census_employees.id'), nullable=True)
)
enrollment_requests = Table('enrollment_requests', metadata,
    Column('id', Integer, primary_key=True),
    Column('enrollment_id', Integer, ForeignKey('enrollments.id'), nullable=False),
    Column('token', String, nullable=False),
    Column('expiration', Date, nullable=False),
)

enrollment_elections = Table('enrollment_elections', metadata,
    Column('id', Integer, primary_key=True),
    Column('enrollment_id', Integer, ForeignKey('enrollments.id')),
    Column('product_id', Integer, ForeignKey('products.id')),
    
    Column('coverage_taken', Boolean),
    
    Column('total_annual_premium', Numeric),
    Column('employee_coverage', Numeric),
    Column('employee_annual_premium', Numeric),
    Column('spouse_coverage', Numeric),
    Column('spouse_annual_premium', Numeric),

    Column('children_coverage', Numeric),
    Column('children_annual_premium', Numeric),
    
)

def create_products():
    
    Product(code='FPPTI', name='Family Protection ')

class Database(object):
    """
    Just a handy place to put all database calls
    """
    
    def __init__(self):
        self.engine = db.engine
        metadata.bind = self.engine
        
    def get_product(self, product_id):
        return products.select().where(products.c.id == product_id).execute().first()
    
    def get_product_by_code(self, product_code):
        return products.select().where(products.c.code == product_code).execute().first()
        
    def create_product(self, product_code, name):
        products.insert(values=dict(
            code=product_code,
            name=name,
        )).execute()
        return self.get_product_by_code(product_code)
       
    def save_enrollment(self, enrollment):
        result = enrollments.insert(values=dict(
            case_id=enrollment.case.id,
            employee_first=enrollment.employee_first,
            employee_last=enrollment.employee_last,
            employee_email=enrollment.employee_email,
        )).execute()
        enrollment.enrollment_id = result.inserted_primary_key[0]
    
    def get_enrollment_request_by_token(self, token):
        enrollment_row = enrollment_requests.select().where(enrollment_requests.c.token == token
                ).execute().first()
        if not enrollment_row:
            return None
        
        enrollment = self.get_enrollment(enrollment_row.id)
        
        enrollment_request = EnrollmentRequest(
                enrollment_row.id, 
                enrollment, 
                enrollment_row.expiration, 
                enrollment_row.token
        )
        return enrollment_request
    
    def get_enrollment(self, id):
        row = enrollments.select().where(enrollments.c.id == id).execute().first()
        if not row:
            return None
            
        case = self.get_case(row.id)
        
        return Enrollment(self, case=case, 
                          employee_first=row.employee_first, 
                          employee_last=row.employee_last, 
                          employee_email=row.employee_email
        )

    # Cases
    def save_case(self, case):
        result = cases.insert(values=dict(
            company_name=case.company_name,
            situs_state=case.situs_state,
        )).execute()
        case.id = result.inserted_primary_key[0]
        
        # save products
        for product in case.products:
            case_products.insert(
                values=dict(
                    case_id=case.id,
                    product_id=product.id,
                )).execute()
        
    def get_case(self, case_id):
        case_row = cases.select().where(cases.c.id == case_id).execute().first()
        if not case_row:
            return None
        
        return self._build_case(case_row)
    

    def get_all_cases(self):
        return (self._build_case(row) for row in cases.select().execute())
    
    def _build_case(self, case_row):
        return Case(case_row.id,
                    case_row.company_name,
                    case_row.situs_state,
                    self.get_product(case_row.product_id)
        )
    
    def save_enrollment_request(self, enrollment_req):
        result = enrollment_requests.insert(values=dict(
            enrollment_id=enrollment_req.enrollment.enrollment_id,
            token=enrollment_req.token,
            expiration=enrollment_req.expiration_date,
        )).execute()
        enrollment_req.enrollment_request_id = result.inserted_primary_key[0]
    
        
    def execute_sql(self, statement, params=()):
        return self.engine.execute(statement, params)
        