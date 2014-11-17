
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
    
    # Guarantee Issue Custom Product Columns
    base_product_id = db.Column(db.Integer, db.ForeignKey('products.id'))
    guarantee_issue_amount = db.Column(db.Integer)
    
    # Age criteria: Exclusive range
    criteria_age_min = db.Column(db.Integer)
    criteria_age_max = db.Column(db.Integer)
    
    # Height criteria in inches: Exclusive range
    criteria_height_min = db.Column(db.Integer)
    criteria_height_max = db.Column(db.Integer)

    # Weight criteria in pounds: Exclusive range
    criteria_weight_min = db.Column(db.Integer)
    criteria_weight_max = db.Column(db.Integer)
    
    statement_of_health_bypass_type = db.Column(db.String(32))

    __mapper_args__ = {'polymorphic_identity': u'GI',
                       'inherit_condition': id == Product.id}

    agents = db.relationship('Agent', secondary=product_agents,
                               backref=db.backref('custom_products', lazy='dynamic'))
    
    