from flask import abort
from flask_stormpath import current_user

from taa.core import DBService, db

from models import (
    Product, 
    CustomGuaranteeIssueProduct, 
    GuaranteeIssueCriteria, 
    BypassedStatementOfHealthQuestion,
)
from .statement_of_health import StatementOfHealthQuestionService
from .states import _all_states, _all_statecodes, FPPTI_states, FPPCI_states, \
    FPPTI_generic_states, FPPCI_generic_states, get_all_states
from .rates import get_product_rates_lookup
from .recommendations import get_product_recommendations

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
        return Product.query.filter(Product.product_type == self.BASE_PRODUCT_TYPE
            ).options(db.eagerload('restricted_agents')
            ).all()
    
    def get_custom_products(self):
        return CustomGuaranteeIssueProduct.query.options(db.eagerload('agents')).all()
    
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
    
    def get_if_allowed(self, product_id):
        from taa.services.agents import AgentService
        
        product = self.get_or_404(product_id)
        
        agent_service = AgentService()
        if agent_service.is_user_admin(current_user) or agent_service.is_user_home_office(current_user):
            return product
        elif agent_service.is_user_agent(current_user):
            agent = agent_service.get_agent_from_user(current_user)
            if not self.can_agent_view_product(agent, product):
                abort(401)
            
            return product
        
        abort(401)
    
    def edit_if_allowed(self, product_id):
        """Retrieve product for editing"""
        from taa.services.agents import AgentService

        product = self.get_or_404(product_id)
        
        agent_service = AgentService()
        if agent_service.is_user_admin(current_user) or agent_service.is_user_home_office(current_user):
            return product
        elif agent_service.is_user_agent(current_user):
            agent = agent_service.get_agent_from_user(current_user)
            if not self.can_agent_edit_product(agent, product):
                abort(401)
        
            return product

        abort(401)
        
        
    def can_agent_view_product(self, agent, product):
        """ 
        Can view base products that do not restrict this particular agent 
          and any GI products assigned to him.
        """
        return product in self.get_products_for_agent(agent)
    
    def can_agent_edit_product(self, agent, product):
        return False
        
    def get_all_enrollable_products(self):
        
        products = self.get_enrollable_base_products()
        
        # All custom products
        products += self.get_custom_products()
        return products
        
    def get_enrollable_base_products(self):
        return [p for p in self.get_base_products() if p.visible_to_agents] 
        
    def get_products_for_agent(self, agent):
        
        # Agents get all base products not explicitly restricted 
        products = [p for p in self.get_enrollable_base_products() if agent not in p.restricted_agents]
        
        # They also get any custom products added by the HO admin
        products += agent.custom_products
        
        return products
    
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
    
    
    def get_soh_labels(self):
        return [
            "Hospital 90 days",
            "Heart",
            "Cancer",
            "Respiratory",
            "Liver",
            "HIV/AIDS",
            "Ever been rejected",
        ]
    
    
    def update_product_agents(self, product, agents, **kwargs):
        from taa.services.agents import AgentService
        agent_service = AgentService()
        agents = agent_service.get_all(*[a['id'] for a in agents])
        product.agents = agents
        db.session.flush()
        
    def update_product_restricted_agents(self, product, restricted_agents, **kwargs):
        from taa.services.agents import AgentService
        agent_service = AgentService()
        product.restricted_agents = agent_service.get_all(*[a['id'] for a in restricted_agents])
        db.session.flush()
        
    def update_product_criteria(self, product, gi_criteria, **kwargs):
        
        # Remove existing criteria
        product.gi_criteria = []
        
        # Add the new criteria
        criteria_service = ProductCriteriaService()
        product.gi_criteria = [criteria_service.create(**criterion) for criterion in gi_criteria]
        db.session.flush()
        
    
    def update_product_bypassed_questions(self, product, bypassed_questions, **kwargs):
        # Remove existing questions
        soh_question_service = ProductSOHQuestionService()
        soh_question_service.query().filter_by(product_id=product.id).delete()
        db.session.flush()
        
        # Add the new questions
        for question in bypassed_questions:
            soh_question_service.create(**dict(
                question_type_label=question,
                product_id=product.id,
            ))
        
        db.session.flush()
        db.session.refresh(product)    
    
    
    def get_product_rates(self, product, demographics):
        lookup = get_product_rates_lookup(product)
        return lookup.get_all_rates(**demographics)
    
    def get_product_recommendations(self, product, demographics):
        return get_product_recommendations(product, **demographics)
    
    
    
class ProductCriteriaService(DBService):
    __model__ = GuaranteeIssueCriteria
    
class ProductSOHQuestionService(DBService):
    __model__ = BypassedStatementOfHealthQuestion
    



