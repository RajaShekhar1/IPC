
from taa import db
from taa.helpers import JsonSerializable

class ProductJsonSerializable(JsonSerializable):
    __json_hidden__ = ['cases']

class Product(ProductJsonSerializable, db.Model):
    __tablename__ = 'products'

    id = db.Column(db.Integer, primary_key=True)
    code = db.Column(db.String, nullable=False)
    name = db.Column(db.String, nullable=False)
    
    product_type = db.Column(db.String(16), nullable=False, default=u'base', server_default=u'base')
    
    __mapper_args__ = {
        'polymorphic_on': product_type,
        'polymorphic_identity': u'base',
        'with_polymorphic': '*',
    }
    
# Relate custom products to agents
product_agents = db.Table('product_agents', db.metadata,
    db.Column('product_id', db.Integer, db.ForeignKey('products.id'), primary_key=True),
    db.Column('agent_id', db.Integer, db.ForeignKey('agents.id'), primary_key=True),
)

class CustomProductSerializer(ProductJsonSerializable):
    __json_hidden__ = ['cases']

class CustomGuaranteeIssueProduct(CustomProductSerializer, Product):
    __tablename__ = "products_custom_guaranteed_issue"
    
    id = db.Column(db.Integer, db.ForeignKey('products.id'), primary_key=True)
    base_product_id = db.Column(db.Integer, db.ForeignKey('products.id'))
    statement_of_health_bypass_type = db.Column(db.String(32))

    __mapper_args__ = {'polymorphic_identity': u'GI',
                       'inherit_condition': id == Product.id}
    
    agents = db.relationship('Agent', secondary=product_agents,
                               backref=db.backref('custom_products', lazy='dynamic'))
    
class BypassedSOHSerializer(JsonSerializable):
    __json_hidden__ = ['product']
    
class BypassedStatementOfHealthQuestion(BypassedSOHSerializer, db.Model):
    __tablename__ = "products_gi_bypass_questions"
    
    id = db.Column(db.Integer, primary_key=True)
    product_id = db.Column(db.Integer, db.ForeignKey('products.id'))
    product = db.relationship("Product", backref=db.backref('bypassed_soh_questions'))
    question_type_label = db.Column(db.Unicode)
    
class GICriteriaSerializer(JsonSerializable):
    __json_hidden__ = ['product']
    
class GuaranteeIssueCriteria(GICriteriaSerializer, db.Model):
    __tablename__ = "products_gi_criteria"
    
    id = db.Column(db.Integer, primary_key=True)
    
    # Custom product this applies to
    product_id = db.Column(db.Integer, db.ForeignKey('products.id'))
    product = db.relationship('Product', backref=db.backref('gi_criteria'))
    
    # To which applicants does this criteria apply
    applicant_type = db.Column(db.Unicode(16))
    
    guarantee_issue_amount = db.Column(db.Integer)
    
    # Age criteria: Exclusive range
    age_min = db.Column(db.Integer)
    age_max = db.Column(db.Integer)
    
    # Height criteria in inches: Exclusive range
    height_min = db.Column(db.Integer)
    height_max = db.Column(db.Integer)
    
    # Weight criteria in pounds: Exclusive range
    weight_min = db.Column(db.Integer)
    weight_max = db.Column(db.Integer)