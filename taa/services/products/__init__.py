from flask import abort
from flask_stormpath import current_user

from taa.core import DBService, db

from models import (
    Product, 
    CustomGuaranteeIssueProduct, 
    GuaranteeIssueCriteria, 
    BypassedStatementOfHealthQuestion,
)

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
        # Can view base products and any GI they are assigned
        return not product.is_guaranteed_issue() or (
            product.is_guaranteed_issue() and product in self.get_products_for_agent(agent)
        )
    
    def can_agent_edit_product(self, agent, product):
        return False
        
    def get_products_for_agent(self, agent):
        
        # For now, agents get base products (TODO: exclude for an arbitrary list of agents)
        products = self.get_base_products()
        
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
    
    
class ProductCriteriaService(DBService):
    __model__ = GuaranteeIssueCriteria
    
class ProductSOHQuestionService(DBService):
    __model__ = BypassedStatementOfHealthQuestion
    


class StatementOfHealthQuestionService(object):
    def form_for_state(self, product, state):

        base_product = product.get_base_product()
        
        if (base_product.code == "FPPTI" and state in FPPTI_generic_states) or (
                base_product.code == "FPPCI" and state in FPPCI_generic_states):
            return "Generic"
        else:
            form_state_lookup = self.form_dict.get(base_product.code)
            if form_state_lookup:
                return form_state_lookup.get(state, "Generic")
        
        return "Generic"
    
    def get_health_questions(self, product, state):
        form = application_forms_by_label.get(self.form_for_state(product, state))
        return form.questions
    
    form_dict = {
        #
        # comment out any forms not yet in Production
        #
        "FPPTI": {
            "CO": "WS-UST App R409-CO",
            # "CT": "WS-UST App R409-CT",
            # "DC": "WS-UST App R409-DC",
            "FL": "WS-UST App R409-FL",
            "IL": "WS-UST App R409-IL",
            #"ME": "WS-UST App R409-ME",
            #"MD": "WS-UST App R409-MD",
            #"MA": "WS-UST App R409-MA",
            #"MN": "WS-UST App R409-MN",
            "MO": "WS-UST App R409-MO",
            #"NH": "WS-UST App R409-NH",
            #"NC": "WS-UST App R409-NC",
            #"ND": "WS-UST App R409-ND",
            "OH": "WS-UST App R409-OH",
            "PA": "WS-UST App R409-PA",
            "VA": "WS-UST App R409-VA",
            "WI": "WS-UST App R409-WI",
            "Generic": "Generic"
        },
        "FPPCI": {
            "CO": "WS-UST App R409-CO",
            "IL": "WS-UST App R409-IL",
            "FL": "WS-UST App R409-FL",
            "MO": "WS-UST App R409-MO",
            "OH": "WS-UST App R409-OH",
            "VA": "WS-UST App R409-VA",
            "WI": "WS-UST App R409-WI",
            "Generic": "Generic"
        },
        # TODO: Need real data
        "Group CI": {
            "IN": "WS-UST App R409-IL",
        },
    }
    
from taa.helpers import JsonSerializable
class SOHQuestion(JsonSerializable):
    def __init__(self, label, question):
        self.label = label
        self.question = question

    def to_json(self):
        return dict(label=self.label, question_text=self.question)


class ApplicationForm(object):
    def __init__(self, label, questions):
        self.label = label
        self.questions = questions
    

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

FPPTI_generic_states = ["AL", "AK", "AZ", "AR", "CA", "DE", "GA", "HI", "ID", "IA", "KS", "KY", "LA", "MI", "MS", "MT", "NE", "NV", "NM", "OK", "OR", "RI", "SC", "SD", "TN", "TX", "UT", "VI", "WV", "WY"]
FPPCI_generic_states = ["AL", "AK", "AZ", "AR", "CA", "DE", "GA", "HI", "ID", "IN", "IA", "KS", "KY", "LA", "MI", "MS", "MT", "NE", "NV", "NM", "OK", "OR", "PR", "RI", "SC", "SD", "TN", "TX", "UT", "VI", "WV", "WY"]


_all_states = [
    dict(
        shortname=s[0],
        name=s[1]
    ) for s in FPPTI_states
]

_all_statecodes = [s['shortname'] for s in _all_states]


# Common SOH Questions used in most applications
hospitalized_question = SOHQuestion('Hospital 90 days',
                                    'Has any Applicant been hospitalized in the past 90 days?')
heart_question = SOHQuestion('Heart',
                             'In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Angina, heart attack, stroke, heart bypass surgery, angioplasty, coronary artery stenting, or coronary artery disease?')
cancer_question = SOHQuestion('Cancer',
                              'In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for any form of cancer to include leukemia or Hodgkin\'s Disease (excluding non-invasive, non-melanoma skin cancer)?')
respiratory_question = SOHQuestion('Respiratory',
                                   'In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Chronic obstructive pulmonary disease (COPD), emphysema, or any other chronic respiratory disorder, excluding asthma?')
liver_question = SOHQuestion('Liver',
                             'In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Alcoholism or drug or alcohol abuse, cirrhosis, hepatitis, or any other disease of the liver?')

aids_question = SOHQuestion('HIV/AIDS',
                            'Has any Applicant been diagnosed or treated by a physician, or tested positive for: Human Immunodeficiency Virus (HIV), Acquired Immune Deficiency Syndrome (AIDS), or AIDS-Related Complex (ARC)?')

ever_been_rejected_question = SOHQuestion('Ever been rejected',
                                          'Has any Applicant ever applied for and been rejected for life insurance?')

application_forms = [
    ApplicationForm('WS-UST App R409-MO', [
        hospitalized_question,
        heart_question,
        cancer_question,
        respiratory_question,
        liver_question,
        aids_question
    ]),
    ApplicationForm('WS-UST App R409-FL', [
        hospitalized_question,
        SOHQuestion('Heart',
                    'In the past 10 years, has the Applicant been hospitalized for, been diagnosed or treated by a licensed member of the medical profession, or taken prescription medication for Angina, heart attack, stroke, heart bypass surgery, angioplasty, coronary artery stenting, or coronary artery disease?'),
        SOHQuestion('Cancer',
                    'In the past 10 years, has the Applicant been hospitalized for, been diagnosed or treated by a licensed member of the medical profession, or taken prescription medication for any form of cancer to include leukemia or Hodgkin\'s Disease (excluding non-invasive, non-melanoma skin cancer)?'),
        SOHQuestion('Respiratory',
                    'In the past 10 years, has the Applicant been hospitalized for, been diagnosed or treated by a licensed member of the medical profession, or taken prescription medication for Chronic obstructive pulmonary disease (COPD), emphysema, or any other chronic respiratory disorder, excluding asthma?'),
        SOHQuestion('Liver',
                    'In the past 10 years, has the Applicant been hospitalized for, been diagnosed or treated by a licensed member of the medical profession, or taken prescription medication for Alcoholism or drug or alcohol abuse, cirrhosis, hepatitis, or any other disease of the liver?'),
        SOHQuestion('HIV/AIDS',
                    'Has the Applicant under this application of coverage tested positive for exposure to the HIV infection or been diagnosed as having ARC or AIDS caused by the HIV infection or other sickness or condition derived from such infection?'),
        ever_been_rejected_question
    ]),
    ApplicationForm('Generic', [
        hospitalized_question,
        heart_question,
        cancer_question,
        respiratory_question,
        liver_question,
        aids_question,
        ever_been_rejected_question
    ]),
    ApplicationForm('WS-UST App R409-PA', [
        hospitalized_question,
        heart_question,
        cancer_question,
        respiratory_question,
        liver_question,
        aids_question,
        ever_been_rejected_question
    ]),
    ApplicationForm('WS-UST App R409-CO', [
        hospitalized_question,
        heart_question,
        cancer_question,
        respiratory_question,
        liver_question,
        SOHQuestion('HIV/AIDS',
                    'Have you had or been told by a member of the medical profession that you have AIDS or HIV infection?'),
        ever_been_rejected_question
    ]),
    ApplicationForm('WS-UST App R409-IL', [
        hospitalized_question,
        heart_question,
        cancer_question,
        respiratory_question,
        liver_question,
        SOHQuestion('HIV/AIDS',
                    'Has any Applicant been diagnosed, tested, or treated by a physician for: Human Immunodeficiency Virus (HIV), Acquired Immune Deficiency Syndrome (AIDS), or AIDS-Related Complex (ARC)?'),
        ever_been_rejected_question
    ]),
    ApplicationForm('WS-UST App R409-WI', [
        hospitalized_question,
        heart_question,
        cancer_question,
        respiratory_question,
        liver_question,
        SOHQuestion('HIV/AIDS',
                    'Has any Applicant been diagnosed by a physician as having Human Immuno-deficiency Virus (HIV), Acquired Immune Deficiency Syndrome (AIDS), or AIDS-Related Complex (ARC)? (The applicant need not reveal HIV test results received from an anonymous counseling and testing site or the results of a home test kit.) '),
        ever_been_rejected_question
    ]),
    ApplicationForm('WS-UST App R409-VA', [
        hospitalized_question,
        heart_question,
        cancer_question,
        respiratory_question,
        liver_question,
        aids_question,
        ever_been_rejected_question
    ]),
    ApplicationForm('WS-UST App R409-OH', [
        hospitalized_question,
        heart_question,
        cancer_question,
        respiratory_question,
        liver_question,
        aids_question,
        ever_been_rejected_question
    ]),
]
application_forms_by_label = {a.label: a for a in application_forms}