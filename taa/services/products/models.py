from collections import defaultdict

import json

from taa import db
from taa.helpers import JsonSerializable
from taa.services.products.product_forms import ProductFormService
from taa.services.products.riders import RiderService
from decimal import Decimal

product_form_service = ProductFormService()


class ProductJsonSerializable(JsonSerializable):
    __json_hidden__ = ['cases', 'customized_products']

    def to_json(self):
        # Default serialized data
        data = super(ProductJsonSerializable, self).to_json()

        data['customer_short_name_display'] = self.get_short_name()
        data['brochure_name_display'] = self.get_brochure_name()
        data['brochure_url_display'] = self.get_brochure_url()

        # Add in some helper attributes

        # Base product type 'code'
        data['base_product_type'] = self.get_base_product_code()
        data['is_guaranteed_issue'] = self.is_guaranteed_issue()

        # Get replacement form text for the wizard
        data['replacement_paragraphs'] = self.get_replacement_paragraphs()

        if hasattr(self, 'case_id'):
            from taa.services.cases.models import case_products
            association = db.session.query(case_products).filter_by(case_id=self.case_id, product_id=self.id).first()
            if association is not None:
                data['ordinal'] = association.ordinal

        # Get rider information
        data['riders'] = RiderService().get_riders_for_product(self)

        return data


# Track who can't see products that are normally visible to all
product_restricted_agents = db.Table('product_restricted_agents', db.metadata,
                                     db.Column('product_id', db.Integer, db.ForeignKey('products.id'),
                                               primary_key=True),
                                     db.Column('agent_id', db.Integer, db.ForeignKey('agents.id'),
                                               primary_key=True),
                                     )
db.Index('ix_product_restricted_agents_agent', product_restricted_agents.c.agent_id)


class Product(ProductJsonSerializable, db.Model):
    TYPE_STATIC_BENEFIT = u'Static Benefit'
    TYPE_GROUP_CI = u'Group CI'
    TYPE_FPPCI = u'FPPCI'
    TYPE_FPPTI = u'FPPTI'
    TYPE_HI = u'HI'
    TYPE_ACC = u'ACC'

    __tablename__ = 'products'

    id = db.Column(db.Integer, primary_key=True)
    code = db.Column(db.String, nullable=False, index=True)
    name = db.Column(db.String, nullable=False)
    use_base_product_settings = db.Column(db.String, nullable=True)
    customer_short_name = db.Column(db.String, nullable=True)
    brochure_url = db.Column(db.Unicode(2000))
    brochure_name = db.Column(db.Unicode(256))
    is_fpp_gov = db.Column(db.Boolean, nullable=False, server_default='FALSE')

    # Monthly Flat Fee for Membership Products
    flat_fee = db.Column(db.Numeric, nullable=True, server_default='5')

    # Boolean that controls whether on not this can be enrolled by agents
    visible_to_agents = db.Column(db.Boolean, nullable=False, server_default='True')

    product_type = db.Column(db.String(16), nullable=False, default=u'base', server_default=u'base')

    restricted_agents = db.relationship('Agent', secondary=product_restricted_agents,
                                        backref=db.backref('restricted_products', lazy='dynamic'))

    template_id = db.Column(db.String(64), nullable=True)

    __mapper_args__ = {
        'polymorphic_on': product_type,
        'polymorphic_identity': u'base',
        'with_polymorphic': '*',
    }

    def get_base_product(self):
        # By default, all products are base products
        return self

    def get_base_product_code(self):
        return self.get_base_product().code if self.get_base_product() else ''

    def can_enroll(self):
        return True

    def is_guaranteed_issue(self):
        return False

    def is_base_product(self):
        return True

    def is_fpp(self):
        return self.get_base_product_code().lower().startswith('fpp')

    def does_generate_form(self):
        # Temporary solution to identify products that include output in PDFs
        return self.is_fpp() or self.get_base_product_code() in [Product.TYPE_GROUP_CI, Product.TYPE_STATIC_BENEFIT]

    def requires_dell_csv_submission(self):
        return self.get_base_product_code() in [Product.TYPE_HI, Product.TYPE_ACC]

    def is_base_fpp_gov(self):
        return self.get_base_product().is_fpp_gov if self.get_base_product() else self.is_fpp_gov

    def is_simple_coverage(self):
        return self.get_base_product_code() in [Product.TYPE_HI, Product.TYPE_ACC]

    def is_applicant_covered(self, applicant_type, coverage_tier):
        """
        Check if an applicant is covered by the coverage tier
        :param applicant_type:
        :param coverage_tier:
        """
        if self.is_static_benefit():
            return True
        if not self.is_simple_coverage():
            return False
        if coverage_tier == 'EE':
            return applicant_type == u'employee'
        elif coverage_tier == 'ES':
            return applicant_type in [u'employee', u'spouse']
        elif coverage_tier == 'EC':
            return applicant_type in [u'employee', u'children']
        elif coverage_tier == 'EF':
            return applicant_type in [u'employee', u'spouse', u'children']
        else:
            return False

    def should_use_base_product_settings(self):
        use_base_product_settings = getattr(self, "use_base_product_settings")
        if use_base_product_settings is not None:
            return json.loads(use_base_product_settings)
        return dict()

    def get_short_name(self):
        use_base_product_settings = self.should_use_base_product_settings()
        if use_base_product_settings.get("customer_short_name"):
            base_product = getattr(self, "base_product")
            return getattr(base_product, "customer_short_name")
        return getattr(self, "customer_short_name")

    def format_type(self):
        if self.is_guaranteed_issue():
            return self.base_product.name if self.base_product else '(Not Selected)'
        else:
            return 'Base Product'

    def get_replacement_paragraphs(self):
        """
        :return: a dictionary with statecodes as keys mapped to lists of HTML strings
        representing paragraphs of text to show up on the replacement form.
        """
        state_replacement_paragraphs = {}
        forms = product_form_service.get_replacement_forms_for_product(self.get_base_product_code())
        for replacement_form in forms:
            for statecode in replacement_form.statecodes:
                form = product_form_service.get_replacement_form(self.get_base_product_code(), statecode)
                if form:
                    state_replacement_paragraphs[statecode] = form.paragraphs

        return state_replacement_paragraphs

    def get_brochure_name(self):
        use_base_product_settings = self.should_use_base_product_settings()
        if use_base_product_settings.get("brochure_name"):
            base_product = getattr(self, "base_product")
            return getattr(base_product, "brochure_name")
        return getattr(self, "brochure_name")

    def get_brochure_url(self):
        use_base_product_settings = self.should_use_base_product_settings()
        if use_base_product_settings.get("brochure_url"):
            base_product = getattr(self, "base_product")
            return getattr(base_product, "brochure_url")
        return getattr(self, "brochure_url")

    def get_brochure_name(self):
        if self.brochure_name:
            return self.brochure_name

        if not self.is_base_product():
            return self.get_base_product().brochure_name

        return None

    def get_brochure_url(self):
        if self.brochure_url:
            return self.brochure_url

        # Will check a base product if necessary
        if not self.is_base_product():
            return self.get_base_product().brochure_url

        return None

    def are_rates_limited_to_GI(self):
        "Custom GI products can override this, but base products show all rates."
        return False

    def requires_occupation(self):
        return self.get_base_product_code() == 'HI' or self.get_base_product_code() == 'ACC'

    def requires_signature(self):
        return self.get_base_product_code() not in ['HI', 'ACC']

    def is_static_benefit(self):
        return self.get_base_product_code() == 'Static Benefit'

    def requires_paylogix_export(self, enrollment_record=None):
        requires_export = self.get_base_product() in ['Group CI', 'Static Benefit'] or self.is_fpp()
        if enrollment_record:
            requires_export = requires_export and enrollment_record.case.requires_paylogix_export
        return requires_export


# Relate custom products to agents - who can see these products
product_agents = db.Table('product_agents', db.metadata,
                          db.Column('product_id', db.Integer, db.ForeignKey('products.id'), primary_key=True),
                          db.Column('agent_id', db.Integer, db.ForeignKey('agents.id'), primary_key=True),
                          )
db.Index('ix_product_agents_agent', product_agents.c.agent_id)


class CustomProductSerializer(ProductJsonSerializable):
    __json_hidden__ = ['cases', 'customized_products', 'base_product']
    __json_modifiers__ = {
        'is_fpp_gov': lambda _, p: p.is_base_fpp_gov()
    }


class CustomGuaranteeIssueProduct(CustomProductSerializer, Product):
    __tablename__ = "products_custom_guaranteed_issue"

    id = db.Column(db.Integer, db.ForeignKey('products.id'), primary_key=True)
    base_product_id = db.Column(db.Integer, db.ForeignKey('products.id'))
    statement_of_health_bypass_type = db.Column(db.String(32))
    should_limit_rates_to_gi = db.Column(db.Boolean, nullable=False, server_default=db.text('FALSE'))

    __mapper_args__ = {'polymorphic_identity': u'GI',
                       'inherit_condition': id == Product.id}

    base_product = db.relationship('Product', primaryjoin=base_product_id == Product.id, backref='customized_products')
    agents = db.relationship('Agent', secondary=product_agents,
                             backref=db.backref('custom_products', lazy='dynamic'))

    def get_base_product(self):
        # Use the linked product
        return self.base_product

    def is_guaranteed_issue(self):
        return True

    def is_base_product(self):
        return False

    def are_rates_limited_to_GI(self):
        "A setting that restricts rates to GI levels."
        return self.should_limit_rates_to_gi


class BypassedSOHSerializer(JsonSerializable):
    __json_hidden__ = ['product']


class BypassedStatementOfHealthQuestion(BypassedSOHSerializer, db.Model):
    __tablename__ = "products_gi_bypass_questions"

    id = db.Column(db.Integer, primary_key=True)
    product_id = db.Column(db.Integer, db.ForeignKey('products.id'), index=True)
    product = db.relationship("Product", backref=db.backref('bypassed_soh_questions'))
    question_type_label = db.Column(db.Unicode)


class GICriteriaSerializer(JsonSerializable):
    __json_hidden__ = ['product']


class GuaranteeIssueCriteria(GICriteriaSerializer, db.Model):
    __tablename__ = "products_gi_criteria"

    id = db.Column(db.Integer, primary_key=True)

    # Custom product this applies to
    product_id = db.Column(db.Integer, db.ForeignKey('products.id'), index=True)
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
