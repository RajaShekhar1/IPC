
from taa import db
from taa.helpers import JsonSerializable


# The product selection for a given case 
case_products = db.Table('case_products', db.metadata,
    db.Column('case_id', db.Integer, db.ForeignKey('cases.id'), primary_key=True),
    db.Column('product_id', db.Integer, db.ForeignKey('products.id'), primary_key=True),
)

class CaseSerializer(JsonSerializable):
    __json_modifiers__ = {
        'products': lambda products, _: [p for p in products]
    }

class Case(CaseSerializer, db.Model):
    __tablename__ = 'cases'
    
    id = db.Column(db.Integer, primary_key=True)
    company_name = db.Column(db.String, nullable=False)
    situs_state = db.Column(db.String(2), nullable=False)
    situs_city = db.Column(db.String)
    agent_id = db.Column(db.Integer, db.ForeignKey('agents.id'), nullable=False)
    active = db.Column(db.Boolean, default=True)
    
    products = db.relationship('Product', secondary=case_products,
                               backref=db.backref('cases', lazy='dynamic'))
    
    def get_template_data(self):
        return dict(
            id=self.id,
            company=self.company_name,
            state=self.situs_state,
            product=self.products[0].name if self.products else ""
        )
    
    
    def get_product_names(self):
        return ','.join(p.name for p in self.products)

ENROLLMENT_TYPES = ['annual_with_start', 'specific']

class CaseEnrollmentPeriod(JsonSerializable, db.Model):
    
    __tablename__ = 'case_enrollment_periods'
    id = db.Column(db.Integer, primary_key=True)
    case_id = db.Column(db.Integer, db.ForeignKey('cases.id'))
    period_type = db.Column(db.String(32))
    start_date = db.Column(db.DateTime)
    end_date = db.Column(db.DateTime)
    
    case = db.relationship("Case", backref=db.backref("enrollment_periods"))


class CensusRecordSerializer(JsonSerializable):
    pass
    #__json_modifiers__ = {
    #    'products': lambda products, _: [p for p in products]
    #}
    
class CaseCensus(CensusRecordSerializer, db.Model):
    __tablename__ = 'case_census'

    id = db.Column(db.Integer, primary_key=True)
    case_id = db.Column(db.Integer, db.ForeignKey('cases.id'), nullable=False)
    upload_date = db.Column(db.DateTime)
    employee_ssn = db.Column(db.String(9))
    employee_first = db.Column(db.String(256))
    employee_last = db.Column(db.String(256))
    employee_birthdate = db.Column(db.Date)

    employee_street_address = db.Column(db.String(256))
    employee_street_address2 = db.Column(db.String(256))
    employee_city = db.Column(db.String(256))
    employee_state = db.Column(db.String(2))
    employee_zip = db.Column(db.String(5))

    employee_email = db.Column(db.String(256))

    spouse_ssn = db.Column(db.String(9))
    spouse_first = db.Column(db.String(256))
    spouse_last = db.Column(db.String(256))
    spouse_address = db.Column(db.String(512))
    spouse_birthdate = db.Column(db.Date)
    spouse_email = db.Column(db.String(256))
    