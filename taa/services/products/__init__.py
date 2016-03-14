from flask import abort
from flask_stormpath import current_user
from taa.services.products.RatePlan import ApplicantQuery, APPLICANT_CHILD, ApplicantQueryOptions, \
    load_rate_plan_for_base_product, ApplicantDemographics, APPLICANT_SPOUSE, APPLICANT_EMPLOYEE

import recommendations
from taa.core import DBService, db
from models import (
    Product,
    CustomGuaranteeIssueProduct,
    GuaranteeIssueCriteria,
    BypassedStatementOfHealthQuestion,
)
from .statement_of_health import StatementOfHealthQuestionService
from .states import all_states, all_statecodes, get_all_states
from .rates import get_rates
from .payment_modes import get_payment_modes, is_payment_mode_changeable, get_full_payment_modes

from product_forms import ProductFormService


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
        return Product.query.filter(
            Product.product_type == self.BASE_PRODUCT_TYPE).options(
            db.eagerload('restricted_agents')).all()

    def get_custom_products(self):
        return CustomGuaranteeIssueProduct.query.options(
            db.eagerload('agents')).all()

    def create_custom_product(self, product_name, base_product_id):
        product = CustomGuaranteeIssueProduct(name=product_name, code='', base_product_id=base_product_id)
        return self.save(product)

    def get_cases_using_product(self, product):
        from taa.services.cases import CaseService, Case
        return CaseService().query().filter(
            Case.products.any(Product.id == product.id)
            ).all()

    def get_products_by_codes(self, codes):
        return Product.query.filter(Product.code.in_(codes)).all()

    def get_product_by_code_or_400(self, code):
        product = Product.query.filter(Product.code == code).first()
        if not product:
            abort(400)
        return product

    def is_valid_product_code(self, code):
        return code in [p.code for p in self.get_all_enrollable_products()]

    def is_valid_statecode(self, statecode):
        return statecode in self.get_all_statecodes()

    def is_valid_statecode_for_product(self, product_code, statecode):
        products = self.get_products_by_codes([product_code])
        if not products:
            return False

        return (self.is_valid_statecode(statecode)
               and statecode in self.get_product_states(products)[products[0].id]
        )

    def get_riders_for_product(self, product_code):
        pass

    def get_num_health_questions(self, product_code, statecode, applicant_type):
        products = self.get_products_by_codes([product_code])
        if not products:
            return 0
        product = products[0]

        soh_service = StatementOfHealthQuestionService()
        all_questions = soh_service.get_health_questions(product, statecode)

        return len(all_questions)

    def get_if_allowed(self, product_id):
        from taa.services.agents import AgentService

        product = self.get_or_404(product_id)

        agent_service = AgentService()
        if (agent_service.is_user_admin(current_user) or
                agent_service.is_user_home_office(current_user)):
            return product
        elif agent_service.is_user_agent(current_user):
            agent = agent_service.get_agent_from_user(current_user)
            if not self.can_agent_view_product(agent, product):
                abort(401)
            return product
        abort(401)

    def edit_if_allowed(self, product_id):
        """Retrieve product for editing."""
        from taa.services.agents import AgentService

        product = self.get_or_404(product_id)

        agent_service = AgentService()
        if (agent_service.is_user_admin(current_user) or
                agent_service.is_user_home_office(current_user)):
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
        # Right now, only HOAdmin users can edit products.
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
        """Return the mapping of product IDs to statecodes
        where we can enroll that product."""

        if not products:
            products = self.get_all_enrollable_products()

        product_states = {}
        for product in products:
            states_with_forms = StatementOfHealthQuestionService()\
                .get_states_with_forms_for_product(product)

            product_states[product.id] = [
                s['statecode'] for s in states_with_forms
            ]

        return product_states

    def get_soh_labels(self, products):
        """
        Maps product ids to the union of all category label strings found
        on that product's forms for all the products passed in.
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

    def update_product_restricted_agents(self, product, restricted_agents,
                                         **kwargs):
        from taa.services.agents import AgentService
        agent_service = AgentService()
        product.restricted_agents = agent_service.get_all(
            *[a['id'] for a in restricted_agents])
        db.session.flush()

    def update_product_criteria(self, product, gi_criteria, **kwargs):
        # Remove existing criteria
        product.gi_criteria = []

        # Add the new criteria
        criteria_service = ProductCriteriaService()
        product.gi_criteria = [criteria_service.create(
            **criterion) for criterion in gi_criteria]
        db.session.flush()

    def update_product_bypassed_questions(self, product, bypassed_questions,
                                          **kwargs):
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

    def get_rates(self, product, demographics, riders=None):

        if product.get_base_product_code() == 'Group CI':
            return get_rates(product, **demographics)
        else:
            # Use the new rates calculator for all the other products.
            rate_response = {}
            for applicant_type in ['employee', 'spouse', 'children']:

                rate_response[applicant_type] = {}

                # Don't give children option to choose by premium.
                if applicant_type != 'children':
                    rate_response[applicant_type]['bypremium'] = [
                        r for r in self.calc_rates_by_premium(product, applicant_type, demographics, riders)
                        if r is not None
                    ]

                rates_by_coverage = [r for r in self.calc_rates_by_face(product, applicant_type, demographics, riders) if r is not None]

                # Give children only some of the options for coverage
                if applicant_type == 'children':
                    rates_by_coverage = filter(lambda r: int(r['coverage']) <= 20000, rates_by_coverage)

                rate_response[applicant_type]['byface'] = rates_by_coverage

            return rate_response

    def get_recommendations(self, product, demographics):
        return recommendations.get_recommendations(product, **demographics)

    def build_applicant_query_for_demographics(self, product, applicant_type, demographics, riders):
        if applicant_type == 'children':
            applicant_type = APPLICANT_CHILD

        product_options = {}
        # For a sanity check, don't allow riders with Child applicant.
        if riders and applicant_type != APPLICANT_CHILD:
            product_options['riders'] = riders

        if applicant_type == APPLICANT_EMPLOYEE:
            demographics_age = demographics['employee_age']
        elif applicant_type == APPLICANT_SPOUSE:
            demographics_age = demographics['spouse_age']
        else:
            # FIXME: child age doesn't matter for now, but on future products it might.
            demographics_age = demographics.get('child_age', 0)

        return ApplicantQuery(
            applicant_type,
            product_options,
            demographics['statecode'],
            ApplicantDemographics({'age': demographics_age}),
            demographics['payment_mode'],
            rate_options=ApplicantQueryOptions({}),
        )

    def calc_rates_by_premium(self, product, applicant_type, demographics, riders):
        rate_plan = load_rate_plan_for_base_product(product.get_base_product_code())
        applicant_query = self.build_applicant_query_for_demographics(product, applicant_type, demographics, riders)
        if applicant_query.get_age() is None:
            return []
        return rate_plan.get_all_rates_by_premium(applicant_query)

    def calc_rates_by_face(self, product, applicant_type, demographics, riders):
        rate_plan = load_rate_plan_for_base_product(product.get_base_product_code())
        applicant_query = self.build_applicant_query_for_demographics(product, applicant_type, demographics, riders)
        if applicant_query.get_age() is None:
            return []
        return rate_plan.get_all_rates_by_coverage(applicant_query)


class ProductCriteriaService(DBService):
    __model__ = GuaranteeIssueCriteria


class ProductSOHQuestionService(DBService):
    __model__ = BypassedStatementOfHealthQuestion
