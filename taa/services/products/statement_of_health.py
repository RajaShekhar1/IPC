
from .states import FPPTI_generic_states, FPPCI_generic_states

class StatementOfHealthQuestionService(object):
    def form_for_state(self, product, state):

        base_product = product.get_base_product()
        
        if (base_product.code == "FPPTI" and state in FPPTI_generic_states) or (
                base_product.code == "FPPCI" and state in FPPCI_generic_states):
            return "Generic"
        elif (base_product.code == "Group CI"):
            return "GroupCI"
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
        # TODO: Need real state to form mapping
        "Group CI": {
            "IN": "GroupCI",
            "Generic": "GroupCI",
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
    ApplicationForm('GroupCI', [
        SOHQuestion('Family Member History', 
                    "Have 2 or more family members (natural parents, brothers or sisters) both before age 60 been diagnosed with or died from the same condition: of cancer, heart disease, stroke or kidney disease; or, both before age 75, of colorectal cancer, Alzheimer's or Senile Dementia?"
        ),
        SOHQuestion('Ever Diagnosed or Treated',
                    'Has the proposed insured ever been diagnosed or treated for any of the following: Heart Attack, Angioplasty, Coronary Artery Bypass, Stroke, Transient Ischemic Attack, Cancer (excluding non-invasive, non-melanoma Skin Cancer), End-Stage Renal Disease, Liver Cirrhosis, Hepatitis B or C (including Carrier), Multiple Sclerosis, Paralysis, Diabetes (other than during pregnancy), Organ or Bone Marrow Transplant, Alzheimer\'s or Senile Dementia, HIV, AIDS, or AIDS-Related Complex (ARC)?'
        ),
        SOHQuestion("5yr Heart",
                    "In the last 5 (FIVE) years, has the proposed insured been diagnosed with or treated for any heart disease (including angina) or any kidney disease except non-chronic kidney stones or infections?",
        ),
        SOHQuestion("5yr Hypertension / Cholesterol",
                    "In the last 5 (FIVE) years, has the proposed insured been diagnosed with or treated for uncontrolled high blood pressure (hypertension) and/or uncontrolled elevated cholesterol?",
        ),
        SOHQuestion("5yr Lung / Colon",
                    "In the last 5 (FIVE) years, has the proposed insured been diagnosed with or treated for Lung disease requiring hospitalization, colitis, or Crohn's?",
        ),
        SOHQuestion("5yr Skin Cancer",
                    "In the last 5 (FIVE) years, has the proposed insured been diagnosed with or treated for any Skin Cancer or/and Precancerous Lesions/Tumors?"
        ),
        SOHQuestion("5yr HPV/HSV",
                    "In the last 5 (FIVE) years, has the proposed insured been diagnosed with or treated for any Human Papilomavirus (HPV), Herpes Simplex Virus (HSV), chlamydia, or gonorrhea?",
        ),
        SOHQuestion("Abnormal Results",
                    "In the past 2 (TWO) years, has the proposed insured been informed by a member of the medical profession of any abnormal test results or been advised to have any diagnostic tests or procedures which have not yet been completed?"
        ),
        SOHQuestion("Ever been rejected",
                    "Has the proposed insured ever applied for and been rejected for a Critical Illness, Cancer, Heart or Stroke insurance policy?",
        ),        
    ])
]
application_forms_by_label = {a.label: a for a in application_forms}