
from taa import db
from taa.helpers import JsonSerializable


# The product selection for a given case 
case_products = db.Table('case_products', db.metadata,
    db.Column('case_id', db.Integer, db.ForeignKey('cases.id'), primary_key=True),
    db.Column('product_id', db.Integer, db.ForeignKey('products.id'), primary_key=True),
)

class CaseSerializer(JsonSerializable):
    __json_modifiers__ = {
        'products': lambda products, _: [p for p in products],
        'enrollment_periods': lambda periods, _: [p for p in periods]
    }
    __json_hidden__ = ['census_records']

class Case(CaseSerializer, db.Model):
    """
    Model an agent's case describing enrollments at a company
    """
    __tablename__ = 'cases'
    
    id = db.Column(db.Integer, primary_key=True)
    company_name = db.Column(db.String, nullable=False)
    situs_state = db.Column(db.String(2), nullable=True)
    situs_city = db.Column(db.String)
    agent_id = db.Column(db.Integer, db.ForeignKey('agents.id'), nullable=False)
    active = db.Column(db.Boolean, default=False)

    enrollment_period_type = db.Column(db.String(16), nullable=True)
    
    products = db.relationship('Product', secondary=case_products,
                               backref=db.backref('cases', lazy='dynamic'))
    
    def get_template_data(self):
        return dict(
            id=self.id,
            company=self.company_name,
            state=self.situs_state,
            product=self.products[0].name if self.products else "",
            active=self.active
        )
    
    def get_product_names(self):
        return ','.join(p.name for p in self.products)


class PeriodSerializer(JsonSerializable):
    __json_hidden__ = ['case']

class CaseEnrollmentPeriod(PeriodSerializer, db.Model):
    
    __tablename__ = 'case_enrollment_periods'
    id = db.Column(db.Integer, primary_key=True)
    case_id = db.Column(db.Integer, db.ForeignKey('cases.id'))
    period_type = db.Column(db.String(32))
    __mapper_args__ = {
        'polymorphic_on': period_type,
        'with_polymorphic': '*',
    }
    start_date = db.Column(db.DateTime)
    end_date = db.Column(db.DateTime)
    
    case = db.relationship("Case", backref=db.backref("enrollment_periods"))
    

class CaseOpenEnrollmentPeriod(CaseEnrollmentPeriod):
    __mapper_args__ = {'polymorphic_identity': 'open_with_start'}

    def populate_data_dict(self, data):
        data['enrollment_period_type'] = 'open'
        data['open_period_start_date'] = self.start_date if self.start_date else ''
        return data

class CaseAnnualEnrollmentPeriod(CaseEnrollmentPeriod):
    __mapper_args__ = {'polymorphic_identity': 'annual_periods'}
    
    def populate_data_dict(self, data):
        data['enrollment_period_type'] = 'annual'
        if not 'annual_period_dates' in data:
            data['annual_period_dates'] = []
        data['annual_period_dates'].append({
            'period_start_date': self.start_date.strftime('%m/%d') if self.start_date else '',
            'period_end_date': self.end_date.strftime('%m/%d') if self.end_date else '',
        })
        return data

class CensusRecordSerializer(JsonSerializable):
    __json_hidden__ = ['census_records']
    
    
class CaseCensus(CensusRecordSerializer, db.Model):
    __tablename__ = 'case_census'

    id = db.Column(db.Integer, primary_key=True)
    case_id = db.Column(db.Integer, db.ForeignKey('cases.id'), nullable=False)
    case = db.relationship('Case', backref=db.backref('census_records'))
    
    upload_date = db.Column(db.DateTime)
    employee_ssn = db.Column(db.String(9))
    employee_first = db.Column(db.String(256))
    employee_last = db.Column(db.String(256))
    employee_gender = db.Column(db.String(6))
    employee_birthdate = db.Column(db.Date)
    employee_email = db.Column(db.String(256))
    employee_phone = db.Column(db.String(32))
    
    employee_street_address = db.Column(db.String(256))
    employee_street_address2 = db.Column(db.String(256))
    employee_city = db.Column(db.String(256))
    employee_state = db.Column(db.String(2))
    employee_zip = db.Column(db.String(5))

    spouse_ssn = db.Column(db.String(9))
    spouse_first = db.Column(db.String(256))
    spouse_last = db.Column(db.String(256))
    spouse_gender = db.Column(db.String(6))
    spouse_birthdate = db.Column(db.Date)
    spouse_email = db.Column(db.String(256))
    spouse_phone = db.Column(db.String(32))
    
    spouse_street_address = db.Column(db.String(256))
    spouse_street_address2 = db.Column(db.String(256))
    spouse_city = db.Column(db.String(256))
    spouse_state = db.Column(db.String(2))
    spouse_zip = db.Column(db.String(5))
    
    child1_first = db.Column(db.String(256))
    child1_last = db.Column(db.String(256))
    child1_birthdate = db.Column(db.Date)

    child2_first = db.Column(db.String(256))
    child2_last = db.Column(db.String(256))
    child2_birthdate = db.Column(db.Date)

    child3_first = db.Column(db.String(256))
    child3_last = db.Column(db.String(256))
    child3_birthdate = db.Column(db.Date)

    child4_first = db.Column(db.String(256))
    child4_last = db.Column(db.String(256))
    child4_birthdate = db.Column(db.Date)

    child5_first = db.Column(db.String(256))
    child5_last = db.Column(db.String(256))
    child5_birthdate = db.Column(db.Date)

    child6_first = db.Column(db.String(256))
    child6_last = db.Column(db.String(256))
    child6_birthdate = db.Column(db.Date)
    
    def get_employee_data(self):
        return dict(
            first=self.employee_first,
            last = self.employee_last,
            ssn = self.employee_ssn,
            birthdate=self.format_date(self.employee_birthdate),
            email=self.employee_email,
            phone=self.employee_phone,
            gender=self.employee_gender.lower() if self.employee_gender else '',
            
            street_address=self.employee_street_address,
            street_address2=self.employee_street_address2,
            city=self.employee_city,
            state=self.employee_state,
            zip=self.employee_zip,
        )

    def get_spouse_data(self):
        return dict(
            first=self.spouse_first,
            last=self.spouse_last,
            ssn=self.spouse_ssn,
            birthdate=self.format_date(self.spouse_birthdate),
            email=self.spouse_email,
            phone=self.spouse_phone,
            gender=self.spouse_gender.lower() if self.spouse_gender else '',
            street_address=self.spouse_street_address,
            street_address2=self.spouse_street_address2,
            city=self.spouse_city,
            state=self.spouse_state,
            zip=self.spouse_zip,
        )
    
    def get_children_data(self):
        children = []
        
        for num in range(1, 6+1):
            if self.has_child(num):
                children.append(dict(
                    first=self.child_first(num),
                    last=self.child_last(num),
                    birthdate=self.format_date(self.child_birthdate(num)),
                ))
        
        return children
    
    def format_date(self, date):
        if not date:
            return ""
        
        return date.strftime("%m/%d/%Y")
    
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