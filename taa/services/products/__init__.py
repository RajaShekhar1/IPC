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
from .states import all_states, all_statecodes, get_all_states
from .statement_of_health import (
    FPPTI_generic_states, FPPCI_generic_states, GroupCI_generic_states,
    FPPGov_generic_states,
)
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
        
    def get_cases_using_product(self, product):
        from taa.services.cases import CaseService, Case
        return CaseService().query().filter(
                Case.products.any(Product.id == product.id)
            #).filter(Case.active == True
            ).all()
        
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
        return all_states
    
    def get_all_statecodes(self):
        return all_statecodes
    
    def get_product_states(self, products=None):
        """Return the mapping of product IDs to statecodes where we can enroll that product"""
        
        if not products:
            products = self.get_all_enrollable_products()
        
        # A hard-coded list of statecodes to turn off for a given product, even if we have a form for that state
        turned_off_statecodes = {
            'FPPTI': ['CT', 'DC', 'IN', 'ME', 'MD', 'MA', 'MN', 'NH', 'NJ', 'NY', 'NC', 'ND', 'PA', 'VT', 'WA'],
            'FPPCI': ['CT', 'DC', 'ME', 'MD', 'MA', 'MN', 'NH', 'NJ', 'NY', 'NC', 'ND', 'PA', 'PR', 'VT'],
            'Group CI': ["CA", "CO", "DC", "HI", "KS", "KY", "MO", "NE", "NC", "PA", "VA"],
        }
        # Keep the same as FPPTI
        turned_off_statecodes['FPP-Gov'] = turned_off_statecodes['FPPTI']
        
        product_states = {}
        for product in products:
            states_with_forms = StatementOfHealthQuestionService().get_states_with_forms_for_product(product)
            exclude_list = turned_off_statecodes.get(product.get_base_product_code(), [])
            
            product_states[product.id] = [s['statecode'] for s in states_with_forms 
                                          if s['statecode'] not in exclude_list]
            
        return product_states
    
    
    def get_soh_labels(self, products):
        """
        Maps product ids to the union of all category label strings found on that product's forms
         for all the products passed in
        """
        soh_service = StatementOfHealthQuestionService()
        return {p.id: soh_service.get_all_category_labels_for_product(p) 
                for p in products}
        
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
    



