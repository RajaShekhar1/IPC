
from sqlalchemy import (
    create_engine,
    Table, 
    Column, 
    Integer,
    Boolean,
    String, 
    MetaData, 
    ForeignKey,
    Date,
)

from model.Enrollment import Enrollment, EnrollmentRequest, Case

metadata = MetaData()
agents = Table('agents', metadata, 
    Column('agent_id', Integer, primary_key=True),
    Column('agent_code', String(5), nullable=False),
    Column('activated', Boolean, nullable=False),
    Column('password', String),
    Column('first', String),
    Column('last', String),
    Column('email', String),
    Column('phone', String),
)
products = Table('products', metadata, 
    Column('product_id', Integer, primary_key=True),
    Column('code', String, nullable=False),
    Column('name', String, nullable=False),
)
cases = Table('cases', metadata,
    Column('case_id', Integer, primary_key=True),
    Column('company_name', String, nullable=False),
    Column('situs_state', String(2), nullable=False),
    Column('product_id', Integer, ForeignKey('products.product_id'), nullable=False),
    Column('agent_id', Integer, ForeignKey('agents.agent_id')),
)

enrollments = Table('enrollments', metadata,
    Column('enrollment_id', Integer, primary_key=True),
    Column('case_id', Integer, ForeignKey('cases.case_id'), nullable=False),
    Column('employee_first', String, nullable=False),
    Column('employee_last', String, nullable=False),
    Column('employee_email', String, nullable=False),
)
enrollment_requests = Table('enrollment_requests', metadata,
    Column('id', Integer, primary_key=True),
    Column('enrollment_id', Integer, ForeignKey('enrollments.enrollment_id'), nullable=False),
    Column('token', String, nullable=False),
    Column('expiration', Date, nullable=False),
)

class Database(object):
    """
    Just a handy place to put all database calls
    """
    
    def __init__(self, connection_string='sqlite:///TAA.db'):
        self.connection_string = connection_string
        self.engine = create_engine(self.connection_string)
        metadata.bind = self.engine
        
        # In general, it doesn't hurt to call this each time for now to simplify dev setup
        self.init_structure()
        
    def init_structure(self):
        """ 
        call this once to set up the database structure
        
        it will also add new tables, but will not add / remove columns from existing tables
        """
        metadata.create_all()
        
    def get_product(self, product_id):
        return products.select().where(products.c.product_id == product_id).execute().first()
    
    def get_product_by_code(self, product_code):
        return products.select().where(products.c.code == product_code).execute().first()
        
    def create_product(self, product_code, name):
        products.insert(values=dict(
            code=product_code,
            name=name,
        )).execute()
        return self.get_product_by_code(product_code)
        
    def save_case(self, case):
        result = cases.insert(values=dict(
            company_name=case.company_name,
            situs_state=case.situs_state,
            product_id=case.product.product_id,
        )).execute()
        case.case_id = result.inserted_primary_key[0]
        
    def save_enrollment(self, enrollment):
        result = enrollments.insert(values=dict(
            case_id=enrollment.case.case_id,
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
        
        enrollment = self.get_enrollment(enrollment_row.enrollment_id)
        
        enrollment_request = EnrollmentRequest(
                enrollment_row.id, 
                enrollment, 
                enrollment_row.expiration, 
                enrollment_row.token
        )
        return enrollment_request
    
    def get_enrollment(self, id):
        row = enrollments.select().where(enrollments.c.enrollment_id == id).execute().first()
        if not row:
            return None
            
        case = self.get_case(row.case_id)
        
        return Enrollment(self, case=case, 
                          employee_first=row.employee_first, 
                          employee_last=row.employee_last, 
                          employee_email=row.employee_email
        )
        
    def get_case(self, case_id):
        case_row = cases.select().where(cases.c.case_id == case_id).execute().first()
        if not case_row:
            return None
        
        return Case(case_row.case_id, 
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
        