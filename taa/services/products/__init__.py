import decimal

from flask import abort
from flask_stormpath import current_user

from taa.services.products.plan_codes import PLAN_CODES_SIMPLE
from ..cases import Case, CaseCensus
from taa.services.products.rates import GILimitedRatesDecorator

from taa.services.products.RatePlan import ApplicantQuery, APPLICANT_CHILD, ApplicantQueryOptions, \
    load_rate_plan_for_base_product, ApplicantDemographics, APPLICANT_SPOUSE, APPLICANT_EMPLOYEE, COVERAGE_SELECTION_EE, \
    COVERAGE_SELECTION_ES, COVERAGE_SELECTION_EF, COVERAGE_SELECTION_EC

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


PLAN_CODE_COVERAGE_TIERS = [COVERAGE_SELECTION_EE, COVERAGE_SELECTION_ES, COVERAGE_SELECTION_EC,
                            COVERAGE_SELECTION_EF]

class ProductService(DBService):
    __model__ = Product

    BASE_PRODUCT_TYPE = u'base'
    GI_PRODUCT_TYPE = u'GI'
    EXCLUDED_BASE_PRODUCT_CODES = [
        u'Static Benefit',
    ]

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
        
    def get_product_states(self, products=None, case=None):
        """Return the mapping of product IDs to statecodes
        where we can enroll that product.
        """

        if not products:
            products = self.get_all_enrollable_products()

        product_states = {}
        for product in products:
            states_with_forms = StatementOfHealthQuestionService() \
                .get_states_with_forms_for_product(product)

            product_states[product.id] = [
                s['statecode'] for s in states_with_forms
                ]

        if case and case.product_settings:
            # Apply any overrides that are specified for this case.
            for product_id, overrides in case.product_settings.get('state_overrides', {}).items():
                if overrides:
                    product_states[int(product_id)] = overrides
            
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

    def get_rates(self, product, demographics, riders=None, rate_level=None, case_id=None):
        
        if product.get_base_product_code() == 'Group CI':
            product_rates = get_rates(product, **demographics)
        elif product.get_base_product_code() in PLAN_CODES_SIMPLE:
            # Set up the rate calculator to use 'tiers' of coverage options + rate levels.
            rate_response = {'employee': {
                'bytier': [{
                               'coverage_tier': tier,
                               'premium': self.calc_rate_for_tier(product, tier, rate_level, demographics),
                               'payment_mode': demographics['payment_mode']
                           }
                           for tier in PLAN_CODE_COVERAGE_TIERS
                           ]
            }}

            product_rates = rate_response
        elif product.get_base_product_code() in ['Static Benefit']:
            product_rates = {'employee': {'flat_fee': product.flat_fee}}
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

                rates_by_coverage = [r for r in self.calc_rates_by_face(product, applicant_type, demographics, riders)
                                     if r is not None]

                # Give children only some of the options for coverage
                if applicant_type == 'children':
                    rates_by_coverage = filter(lambda r: int(r['coverage']) <= 20000, rates_by_coverage)

                rate_response[applicant_type]['byface'] = rates_by_coverage

                # Limit to GI levels?
                if product.are_rates_limited_to_GI():
                    age = self.get_applicant_age(applicant_type, demographics)
                    smoker = self.get_applicant_smoker_status(applicant_type, demographics)
                    height = self.get_applicant_height(applicant_type, demographics)
                    weight = self.get_applicant_weight(applicant_type, demographics)
                    limit = GILimitedRatesDecorator.get_gi_limit_for_product(product, applicant_type, age, smoker,
                                                                             height, weight)

                    if not limit:
                        # We don't allow enrollment for this applicant type
                        rate_response[applicant_type] = {'bypremium': [], 'byface': []}
                    else:
                        if 'bypremium' in rate_response[applicant_type]:
                            rate_response[applicant_type]['bypremium'] = filter(lambda rate: rate['coverage'] <= limit,
                                                                                rate_response[applicant_type][
                                                                                    'bypremium'])
                        rate_response[applicant_type]['byface'] = filter(lambda rate: rate['coverage'] <= limit,
                                                                         rate_response[applicant_type]['byface'])

            product_rates = rate_response
            
        # Some cases limit the returned rates via age-banded max coverage and/or premiums.
        if case_id:
            from taa.services.cases import CaseService
            case = CaseService().get(case_id)
        else:
            case = None
            
        if not case:
            return product_rates
        
        return self.filter_rates_for_case(product_rates, product, demographics, case)
    
    

    def get_recommendations(self, product, demographics):
        return recommendations.get_recommendations(product, **demographics)

    def build_applicant_query_for_demographics(self, product, applicant_type, demographics, riders):
        if applicant_type == 'children':
            applicant_type = APPLICANT_CHILD

        product_options = {}
        # For a sanity check, don't allow riders with Child applicant.
        if riders and applicant_type != APPLICANT_CHILD:
            product_options['riders'] = riders

        return ApplicantQuery(
            applicant_type,
            product_options,
            demographics['statecode'],
            ApplicantDemographics({'age': self.get_applicant_age(applicant_type, demographics)}),
            demographics['payment_mode'],
            rate_options=ApplicantQueryOptions({}),
        )

    def get_applicant_age(self, applicant_type, demographics):
        if applicant_type == APPLICANT_EMPLOYEE:
            demographics_age = demographics['employee_age']
        elif applicant_type == APPLICANT_SPOUSE:
            demographics_age = demographics['spouse_age']
        else:
            # FIXME: child age doesn't matter for now, but on future products it might.
            demographics_age = demographics.get('child_age', 0)
        return demographics_age

    def get_applicant_smoker_status(self, applicant_type, demographics):
        mapping = {
            APPLICANT_EMPLOYEE: 'employee_smoker',
            APPLICANT_SPOUSE: 'spouse_smoker',
        }
        return self.get_demographics_value(mapping.get(applicant_type), demographics, default=False)

    def get_applicant_height(self, applicant_type, demographics):
        mapping = {
            APPLICANT_EMPLOYEE: 'employee_height',
            APPLICANT_SPOUSE: 'spouse_height',
        }
        return self.get_demographics_value(mapping.get(applicant_type), demographics, default=None)

    def get_applicant_weight(self, applicant_type, demographics):
        mapping = {
            APPLICANT_EMPLOYEE: 'employee_weight',
            APPLICANT_SPOUSE: 'spouse_weight',
        }
        return self.get_demographics_value(mapping.get(applicant_type), demographics, default=None)

    def get_demographics_value(self, key, demographics, default):
        if not key:
            return default
        else:
            return demographics.get(key)

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

    def calc_rate_for_tier(self, product, tier, rate_level, demographics):
        """For simple coverage options, a coverage tier and rate level determine premium"""

        # We always use the employee for this type of coverage.
        applicant_query = ApplicantQuery(
            applicant_type=APPLICANT_EMPLOYEE,
            product_options={},
            state=demographics['statecode'],
            demographics=ApplicantDemographics({}),
            mode=demographics['payment_mode'],
            rate_options=ApplicantQueryOptions({'coverage_tier': tier, 'rate_level': rate_level}),
        )

        rate_plan = load_rate_plan_for_base_product(product.get_base_product_code())
        return rate_plan.calculate_premium(applicant_query)

    def calc_rate_for_flat_fee(self, product, case):
        """
        :param product: Product to calculate the rate for
        :type product: Product
        :param case: Case to calculate the rate for
        :type case: Case
        """
        if case.payment_mode == 12:
            return product.flat_fee
        else:
            return round(product.flat_fee * 12 / case.payment_mode, 2)

    def filter_products_from_membership(self, case, census=None):
        """
        If this applicant (census record) has enrolled in a static benefit before, remove the static benefit
        from the list of products.
        """
        if census is None:
            return case.products
        from taa.services.enrollments import EnrollmentApplication
        applications = EnrollmentApplication.query.filter(EnrollmentApplication.census_record_id == census.id).all()

        # Exclude any static benefit products applicant has previously enrolled
        excluded_product_ids = set(filter(None, [
            product_id if self.get(product_id).get_base_product_code() in self.EXCLUDED_BASE_PRODUCT_CODES
            else None
            for application in applications
            for product_id in application.get_enrolled_product_ids()
            ]))
        return [p for p in case.products if p.id not in excluded_product_ids]

    def get_ordered_products_for_case(self, case_id, include_inactive=True):
        from taa.services.cases.models import case_products
        active_products = db.session.query(Product).join(case_products).join(Case).filter(Case.id == case_id).order_by(case_products.c.ordinal).all()

        if not include_inactive:
            return active_products
        
        # Also include any inactive products that have at one point been enrolled on this case
        from taa.services.enrollments import EnrollmentApplicationCoverage
        from taa.services.enrollments import EnrollmentApplication
        result_set = db.session.query(EnrollmentApplicationCoverage.product_id
                ).filter(EnrollmentApplicationCoverage.enrollment.has(EnrollmentApplication.case_id == case_id)
                ).group_by(EnrollmentApplicationCoverage.product_id
                ).order_by(EnrollmentApplicationCoverage.product_id
                ).all()
        
        all_product_ids = [r.product_id for r in result_set]
        
        other_products = [self.get(product_id)
                          for product_id in all_product_ids
                          if product_id not in [p.id for p in active_products]
                          ]
        
        return active_products + other_products

    def filter_products_by_enrollment_state(self, product_options, state, case):
        product_state_mapping = self.get_product_states(product_options, case)

        # Keep all group-level products (static benefit and group ci)
        # Filter out individual products that are not allowed in this state.
        def is_allowed_in_state(product, state):
            
            if product.is_static_benefit():
                return True
            
            if product.does_situs_state_determine_form():
                state_to_use = case.situs_state
            else:
                state_to_use = state
                
            return state_to_use in product_state_mapping.get(product.id, [])
            
        return filter(lambda p: is_allowed_in_state(p, state), product_options)

    def filter_rates_for_case(self, product_rates, product, demographics, case):
        # Enforce max coverage and max premium if provided
        from taa.services.cases import CaseService
        case_service = CaseService()
        limit_data = case_service.get_product_coverage_limit_data(case, product)
        if limit_data and 'max_coverage' in limit_data and limit_data['max_coverage']['is_enabled']:
            return MaxCoverageLimiter(case, limit_data['max_coverage'], product, demographics).filter_rates(product_rates)
        else:
            return product_rates
            

class MaxCoverageLimiter(object):
    def __init__(self, case, limit_data, product, demographics):
        self.case = case
        self.limit_data = limit_data
        self.product = product
        self.demographics = demographics
        
    def filter_rates(self, product_rates):
        
        for limit in self.limit_data['applicant_limits']:
            self.filter_rates_if_in_age_band(limit, product_rates)
        
        return product_rates
    
    def filter_rates_if_in_age_band(self, limit, product_rates):
        age_band = AgeBand(limit['min_age'], limit['max_age'])
        if limit['applicant_type'] == 'employee' and age_band.is_included(self.demographics['employee_age']):
    
            product_rates['employee']['byface'] = self.filter_by_limit(product_rates['employee']['byface'], limit)
            product_rates['employee']['bypremium'] = self.filter_by_limit(product_rates['employee']['bypremium'], limit)
             
        elif limit['applicant_type'] == 'spouse' and age_band.is_included(self.demographics['spouse_age']):
            
            product_rates['spouse']['byface'] = self.filter_by_limit(product_rates['spouse']['byface'], limit)
            product_rates['spouse']['bypremium'] = self.filter_by_limit(product_rates['spouse']['bypremium'], limit)
            
        elif limit['applicant_type'] == 'child' and any([age_band.is_included(c['age']) for c in self.demographics['children']]):
            if product_rates.get('children', {}).get('byface'):
                product_rates['children']['byface'] = self.filter_by_limit(product_rates['children']['byface'], limit)
            
            if product_rates.get('children', {}).get('bypremium'):
                product_rates['children']['bypremium'] = self.filter_by_limit(product_rates['children']['bypremium'], limit)

    def filter_by_limit(self, rate_list, limit):
        
        # Apply max coverage rule
        if limit.get('max_coverage'):
            max_coverage = int(limit['max_coverage'])
            rate_list = filter(lambda c: c['coverage'] <= max_coverage, rate_list)
            
        # Apply max premium rule
        if limit.get('max_premium'):
            max_premium = decimal.Decimal(str(limit['max_premium']))
            rate_list = filter(lambda c: decimal.Decimal(str(c['premium'])) <= self.normalize_max_premium(max_premium, c), rate_list)
        
        return rate_list

    def normalize_max_premium(self, max_premium, rate):
        "If a case has a changeable payment mode, we need to normalize the maximum premium value to the selected mode."
        if is_payment_mode_changeable(self.case.payment_mode):
            # Assume that max_premium was given in monthly frequency (12), and convert if necessary.
            requested_pay_frequency = int(rate['payment_mode'])
            return (12 * max_premium) / requested_pay_frequency
        else:
            # Nothing needed to be done.
            return max_premium


class AgeBand(object):
    def __init__(self, min_age, max_age):
        self.min_age = int(min_age)
        self.max_age = int(max_age)
        
    def is_included(self, age):
        if not age:
            return False
        return self.min_age <= int(age) and int(age) < self.max_age

class ProductCriteriaService(DBService):
    __model__ = GuaranteeIssueCriteria


class ProductSOHQuestionService(DBService):
    __model__ = BypassedStatementOfHealthQuestion
