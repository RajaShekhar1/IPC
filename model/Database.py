
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

from model.Enrollment import Enrollment

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
    Column('agent_id', Integer, ForeignKey('agents.agent_id'), nullable=False)
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
        
    def init_structure(self):
        """ 
        call this once to set up the database structure
        
        it will also add new tables, but will not add / remove columns from existing tables
        """
        metadata.create_all()
        
    def retrieve_enrollment(self, id):
        row = enrollments.select().where(enrollments.c.enrollment_id == id).execute().first()
        if row:
            case = cases.select().where(cases.c.case_id == row.case_id).execute().first()
            return Enrollment(self, case=case, 
                              employee_first=row.employee_first, 
                              employee_last=row.employee_last, 
                              employee_email=row.employee_email
            )
        
    def store_product(self, product):
        products.insert().execute(values=dict(
            code = product.code,
            name = product.name,
        ))
        
    def retrieve_product_by_code(self, product_code):
        row = products.select().where(products.c.code == product_code).execute().first()
        if not row:
            return None
        else:
            return Product(code=product_code, name=row.name)
        
    def execute_sql(self, statement, params=()):
        return self.engine.execute(statement, params)
        