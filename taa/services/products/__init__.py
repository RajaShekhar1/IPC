from flask import abort

from taa.core import DBService, db

from models import Product, CustomGuaranteeIssueProduct

class ProductService(DBService):
    
    __model__ = Product

    BASE_PRODUCT_TYPE = u'base'
    GI_PRODUCT_TYPE = u'GI'
    
    def search(self, by_name=None, by_code=None, by_type=None):
        q = Product.query
        if by_name:
            q = q.filter(Product.name == by_name)
        if by_code:
            q = q.filter(Product.code == by_code)
        if by_type:
            q = q.filter(Product.product_type == by_type)
        
        return q.all()
        
    def get_base_products(self):
        return Product.query.filter(Product.product_type == self.BASE_PRODUCT_TYPE).all()
    
    def get_custom_products(self):
        return CustomGuaranteeIssueProduct.query.all()
    
    def create_custom_product(self, product_name):
        product = CustomGuaranteeIssueProduct(name=product_name, code='')
        return self.save(product)
        
    def get_products_by_codes(self, codes):
        return Product.query.filter(Product.code.in_(codes)).all()
    
    def get_product_by_code_or_400(self, code):
        product = Product.query.filter(Product.code == code).first()
        if not product:
            abort(400)
        
        return product
    
    def get_products_for_agent(self, agent):
        # TODO: Implement
        return self.all()
    
    def get_all_states(self):
        return _all_states
    
    def get_all_statecodes(self):
        return _all_statecodes
    
    def get_product_states(self):
        """Return the mapping of product codes to enabled states (statecode, state name, is_disabled) """

        return {
            'FPPTI': FPPTI_states,
            'FPPCI': FPPCI_states,
        }
    
    
FPPTI_states = [
    ("", ' ', False),
    ('AL', 'Alabama', False),
    ('AK', 'Alaska', False),
    ('AZ', 'Arizona', False),
    ('AR', 'Arkansas', False),
    ('CA', 'California', False),
    ('CO', 'Colorado', False),
    ('CT', 'Connecticut', True),
    ('DE', 'Delaware', False),
    ('DC', 'District of Columbia', True),
    ('FL', 'Florida', False),
    ('GA', 'Georgia', False),
    ('HI', 'Hawaii', False),
    ('ID', 'Idaho', False),
    ('IL', 'Illinois', False),
    ('IN', 'Indiana', True),
    ('IA', 'Iowa', False),
    ('KS', 'Kansas', False),
    ('KY', 'Kentucky', False),
    ('LA', 'Louisiana', False),
    ('ME', 'Maine', True),
    ('MD', 'Maryland', True),
    ('MA', 'Massachusetts', True),
    ('MI', 'Michigan', False),
    ('MN', 'Minnesota', True),
    ('MS', 'Mississippi', False),
    ('MO', 'Missouri', False),
    ('MT', 'Montana', False),
    ('NE', 'Nebraska', False),
    ('NV', 'Nevada', False),
    ('NH', 'New Hampshire', True),
    ('NJ', 'New Jersey', True),
    ('NM', 'New Mexico', False),
    ('NY', 'New York', True),
    ('NC', 'North Carolina', True),
    ('ND', 'North Dakota', True),
    ('OH', 'Ohio', False),
    ('OK', 'Oklahoma', False),
    ('OR', 'Oregon', False),
    ('PA', 'Pennsylvania', False),
    ('RI', 'Rhode Island', False),
    ('SC', 'South Carolina', False),
    ('SD', 'South Dakota', False),
    ('TN', 'Tennessee', False),
    ('TX', 'Texas', False),
    ('UT', 'Utah', False),
    ('VT', 'Vermont', True),
    ('VA', 'Virginia', False),
    ('WA', 'Washington', True),
    ('WV', 'West Virginia', False),
    ('WI', 'Wisconsin', False),
    ('WY', 'Wyoming', False)
]

FPPCI_states = [
    ("", ' ', False),
    ('AL', 'Alabama', False),
    ('AK', 'Alaska', False),
    ('AZ', 'Arizona', False),
    ('AR', 'Arkansas', False),
    ('CA', 'California', False),
    ('CO', 'Colorado', False),
    ('CT', 'Connecticut', True),
    ('DE', 'Delaware', False),
    ('DC', 'District of Columbia', True),
    ('FL', 'Florida', False),
    ('GA', 'Georgia', False),
    ('HI', 'Hawaii', False),
    ('ID', 'Idaho', False),
    ('IL', 'Illinois', False),
    ('IN', 'Indiana', False),
    ('IA', 'Iowa', False),
    ('KS', 'Kansas', False),
    ('KY', 'Kentucky', False),
    ('LA', 'Louisiana', False),
    ('ME', 'Maine', True),
    ('MD', 'Maryland', True),
    ('MA', 'Massachusetts', True),
    ('MI', 'Michigan', False),
    ('MN', 'Minnesota', True),
    ('MS', 'Mississippi', False),
    ('MO', 'Missouri', False),
    ('MT', 'Montana', False),
    ('NE', 'Nebraska', False),
    ('NV', 'Nevada', False),
    ('NH', 'New Hampshire', True),
    ('NJ', 'New Jersey', True),
    ('NM', 'New Mexico', False),
    ('NY', 'New York', True),
    ('NC', 'North Carolina', True),
    ('ND', 'North Dakota', True),
    ('OH', 'Ohio', False),
    ('OK', 'Oklahoma', False),
    ('OR', 'Oregon', False),
    ('PA', 'Pennsylvania', True),
    ('PR', 'Puerto Rico', True),
    ('RI', 'Rhode Island', False),
    ('SC', 'South Carolina', False),
    ('SD', 'South Dakota', False),
    ('TN', 'Tennessee', False),
    ('TX', 'Texas', False),
    ('UT', 'Utah', False),
    ('VT', 'Vermont', True),
    ('VA', 'Virginia', False),
    ('VI', 'Virgin Islands', False),
    ('WA', 'Washington', False),
    ('WV', 'West Virginia', False),
    ('WI', 'Wisconsin', False),
    ('WY', 'Wyoming', False)
]

_all_states = [
    dict(
        shortname=s[0],
        name=s[1]
    ) for s in FPPTI_states
]

_all_statecodes = [s['shortname'] for s in _all_states]
