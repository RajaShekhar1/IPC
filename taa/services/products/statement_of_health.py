
from .states import all_statecodes, states_by_statecode

class StatementOfHealthQuestionService(object):
    def form_for_state(self, product, statecode):

        code = product.get_base_product_code()
        
        # Return the first form for this product that supports this state
        for form in product_forms.get(code, []):
            if statecode in form.statecodes:
                return form
        
        raise Exception("No form exists for product '%s' in state '%s'"%(code, statecode))
    
    def get_health_questions(self, product, state):
        form = self.form_for_state(product, state)
        return form.questions
    
    def get_states_with_forms_for_product(self, product):

        code = product.get_base_product_code()
        
        enabled_statecodes = set()
        for form in product_forms.get(code, []):
            for statecode in form.statecodes:
                enabled_statecodes.add(statecode)
            
        return sorted([states_by_statecode[sc] for sc in enabled_statecodes], key=lambda x: x['statecode'])

    def get_all_forms_used_for_product(self, product):
        code = product.get_base_product_code()
        return product_forms.get(code, [])

    def get_all_category_labels_for_product(self, product):
        # We want to keep them in the same order (mostly) as seen in the forms, but not included more than once
        used_category_labels = set()
        category_labels = []
        for form in self.get_all_forms_used_for_product(product):
            for index, question in enumerate(form.questions):
                if question.label not in used_category_labels:
                    used_category_labels.add(question.label)
                    if len(category_labels) <= index:
                        category_labels.append(question.label)
                    else:
                        category_labels.insert(index, question.label)
        
        return category_labels
        
from taa.helpers import JsonSerializable
class SOHQuestion(JsonSerializable):
    def __init__(self, label, question, skip_if_coverage_at_most=None):
        self.label = label
        self.question = question
        self.skip_if_coverage_at_most = skip_if_coverage_at_most

    def to_json(self):
        return dict(
            label=self.label, 
            question_text=self.question,
            skip_if_coverage_at_most=self.skip_if_coverage_at_most,
        )


class ApplicationForm(object):
    def __init__(self, label, statecodes, questions, is_generic=False):
        self.label = label
        self.statecodes = statecodes
        self.questions = questions
        self.is_generic = is_generic
    

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



# Some common forms between FPPTI and FPPCI
common_forms = {
    'WS-UST App R409-CO': ApplicationForm('WS-UST App R409-CO', ['CO'], [
        hospitalized_question,
        heart_question,
        cancer_question,
        respiratory_question,
        liver_question,
        SOHQuestion('HIV/AIDS',
                    'Have you had or been told by a member of the medical profession that you have AIDS or HIV infection?'),
        ever_been_rejected_question
    ]),
    'WS-UST App R409-IL': ApplicationForm('WS-UST App R409-IL', ['IL'], [
        hospitalized_question,
        heart_question,
        cancer_question,
        respiratory_question,
        liver_question,
        SOHQuestion('HIV/AIDS',
                    'Has any Applicant been diagnosed, tested, or treated by a physician for: Human Immunodeficiency Virus (HIV), Acquired Immune Deficiency Syndrome (AIDS), or AIDS-Related Complex (ARC)?'),
        ever_been_rejected_question
    ]),
    'WS-UST App R409-FL': ApplicationForm('WS-UST App R409-FL', ['FL'], [
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
    'WS-UST App R409-MO': ApplicationForm('WS-UST App R409-MO', ['MO'], [
        hospitalized_question,
        heart_question,
        cancer_question,
        respiratory_question,
        liver_question,
        aids_question
    ]),
    "WS-UST App R409-OH": ApplicationForm('WS-UST App R409-OH', ['OH'], [
        hospitalized_question,
        heart_question,
        cancer_question,
        respiratory_question,
        liver_question,
        aids_question,
        ever_been_rejected_question
    ]),
    "WS-UST App R409-VA":ApplicationForm('WS-UST App R409-VA', ['VA'], [
        hospitalized_question,
        heart_question,
        cancer_question,
        respiratory_question,
        liver_question,
        aids_question,
        ever_been_rejected_question
    ]),
    "WS-UST App R409-WI": ApplicationForm('WS-UST App R409-WI', ['WI'], [
        hospitalized_question,
        heart_question,
        cancer_question,
        respiratory_question,
        liver_question,
        SOHQuestion('HIV/AIDS',
                    'Has any Applicant been diagnosed by a physician as having Human Immuno-deficiency Virus (HIV), Acquired Immune Deficiency Syndrome (AIDS), or AIDS-Related Complex (ARC)? (The applicant need not reveal HIV test results received from an anonymous counseling and testing site or the results of a home test kit.) '),
        ever_been_rejected_question
    ]),

}


# Common Group CI questions
group_ci_family_member_history_question = SOHQuestion('Family Member History',
                                                      "Have 2 or more family members (natural parents, brothers or sisters) both before age 60 been diagnosed with or died from the same condition: of cancer, heart disease, stroke or kidney disease; or, both before age 75, of colorectal cancer, Alzheimer's or Senile Dementia?"
)
group_ci_diagnosed_question = SOHQuestion('Ever Diagnosed or Treated',
                                          'Has the proposed insured ever been diagnosed or treated for any of the following: Heart Attack, Angioplasty, Coronary Artery Bypass, Stroke, Transient Ischemic Attack, Cancer (excluding non-invasive, non-melanoma Skin Cancer), End-Stage Renal Disease, Liver Cirrhosis, Hepatitis B or C (including Carrier), Multiple Sclerosis, Paralysis, Diabetes (other than during pregnancy), Organ or Bone Marrow Transplant, Alzheimer\'s or Senile Dementia, HIV, AIDS, or AIDS-Related Complex (ARC)?'
)
group_ci_alternate_diagnosed_question = SOHQuestion('Ever Diagnosed or Treated',
                                                    'Has the proposed insured ever been diagnosed or treated for any of the following: Heart Attack, Angioplasty, Coronary Artery Bypass, Stroke, Transient Ischemic Attack, Cancer (excluding non-invasive, non-melanoma Skin Cancer), End-Stage Renal Disease, Liver Cirrhosis, Hepatitis B or C (including Carrier), Multiple Sclerosis, Paralysis, Diabetes (other than during pregnancy), Organ or Bone Marrow Transplant, or Alzheimer\'s or Senile Dementia?',
)

group_ci_heart_question = SOHQuestion("5yr Heart",
                                      "In the last 5 (FIVE) years, has the proposed insured been diagnosed with or treated for any heart disease (including angina) or any kidney disease except non-chronic kidney stones or infections?",
                                      skip_if_coverage_at_most=10000,
)
group_ci_hypertension_question = SOHQuestion("5yr Hypertension / Cholesterol",
                                             "In the last 5 (FIVE) years, has the proposed insured been diagnosed with or treated for uncontrolled high blood pressure (hypertension) and/or uncontrolled elevated cholesterol?",
                                             skip_if_coverage_at_most=10000,
)
group_ci_lung_question = SOHQuestion("5yr Lung / Colon",
                                     "In the last 5 (FIVE) years, has the proposed insured been diagnosed with or treated for Lung disease requiring hospitalization, colitis, or Crohn's?",
                                     skip_if_coverage_at_most=10000,
)
group_ci_skin_question = SOHQuestion("5yr Skin Cancer",
                                     "In the last 5 (FIVE) years, has the proposed insured been diagnosed with or treated for any Skin Cancer or/and Precancerous Lesions/Tumors?",
                                     skip_if_coverage_at_most=10000,
)
group_ci_hpv_question = SOHQuestion("5yr HPV/HSV",
                                    "In the last 5 (FIVE) years, has the proposed insured been diagnosed with or treated for any Human Papilomavirus (HPV), Herpes Simplex Virus (HSV), chlamydia, or gonorrhea?",
                                    skip_if_coverage_at_most=10000,
)
group_ci_abnormal_question = SOHQuestion("Abnormal Results",
                                         "In the past 2 (TWO) years, has the proposed insured been informed by a member of the medical profession of any abnormal test results or been advised to have any diagnostic tests or procedures which have not yet been completed?",
                                         skip_if_coverage_at_most=10000,
)

group_ci_ever_rejected_question = SOHQuestion("Ever been rejected",
                                              "Has the proposed insured ever applied for and been rejected for a Critical Illness, Cancer, Heart or Stroke insurance policy?",
                                              skip_if_coverage_at_most=10000,
)


product_forms = {
    'FPPTI': [
        common_forms['WS-UST App R409-CO'],
        common_forms['WS-UST App R409-FL'],
        common_forms['WS-UST App R409-IL'],
        common_forms['WS-UST App R409-MO'],
        common_forms['WS-UST App R409-OH'],
        common_forms['WS-UST App R409-VA'],
        common_forms['WS-UST App R409-WI'],
        ApplicationForm('WS-UST App R409-PA', ['PA'], [
            hospitalized_question,
            heart_question,
            cancer_question,
            respiratory_question,
            liver_question,
            aids_question,
            ever_been_rejected_question
        ]),
        
        ApplicationForm('Generic', 
                        [s for s in all_statecodes if s not in 
                        ['CO', 'FL', 'IL', 'MO', 'OH', 'PA', 'VA', 'WI']], [
            hospitalized_question,
            heart_question,
            cancer_question,
            respiratory_question,
            liver_question,
            aids_question,
            ever_been_rejected_question
        ], is_generic=True),
    ],
    'FPPCI': [
        common_forms['WS-UST App R409-CO'],
        common_forms['WS-UST App R409-FL'],
        common_forms['WS-UST App R409-IL'],
        common_forms['WS-UST App R409-MO'],
        common_forms['WS-UST App R409-OH'],
        common_forms['WS-UST App R409-VA'],
        common_forms['WS-UST App R409-WI'],
        ApplicationForm('Generic',
            [s for s in all_statecodes if s not in
              ['CO', 'FL', 'IL', 'MO', 'OH', 'VA', 'WI']], 
            [
                hospitalized_question,
                heart_question,
                cancer_question,
                respiratory_question,
                liver_question,
                aids_question,
                ever_been_rejected_question
            ], 
            is_generic=True),
    ],
    'Group CI': [
        
        ApplicationForm(
            'Group CI Generic', 
            ["AL", "AZ", "AR", "GA", "IN", "IA", "LA", "MA", "MI", "MS", "NE", "NV", "NM", "OK", "SC", "TX", "UT", "WI"], 
            [
                group_ci_family_member_history_question,
                group_ci_diagnosed_question,
                group_ci_heart_question,
                group_ci_hypertension_question,
                group_ci_lung_question,
                group_ci_skin_question,
                group_ci_hpv_question,
                group_ci_abnormal_question,
                group_ci_ever_rejected_question,
            ], 
            is_generic=True
        ),
        
        ApplicationForm('Group CI KY', ['KY'], [
            group_ci_family_member_history_question,
            group_ci_alternate_diagnosed_question,
            SOHQuestion('HIV/AIDS',
                        'Has the proposed insured tested positive for HIV?',
            ),
            group_ci_heart_question,
            group_ci_hypertension_question,
            group_ci_lung_question,
            group_ci_skin_question,
            group_ci_hpv_question,
            SOHQuestion("Abnormal Results",
                        "In the past 2 (TWO) years, has the proposed insured been advised by a member of the medical profession to have any diagnostic tests or procedures which have not yet been completed?",
                        skip_if_coverage_at_most=10000,
            ),
            group_ci_ever_rejected_question,
        ]),
        
        ApplicationForm('Group CI IL', ['IL'], [
            group_ci_family_member_history_question,
            group_ci_alternate_diagnosed_question,
            SOHQuestion('HIV/AIDS',
                        'Has the proposed insured ever been diagnosed or treated by a medical professional for AIDS?',
            ),
            
            group_ci_heart_question,
            group_ci_hypertension_question,
            group_ci_lung_question,
            group_ci_skin_question,
            group_ci_hpv_question,
            group_ci_abnormal_question,
            group_ci_ever_rejected_question,
        ]),

        ApplicationForm('Group CI MO', ['MO'], [
            group_ci_family_member_history_question,
            SOHQuestion("Ever Diagnosed or Treated",
                        "In the past 15 years, has the proposed insured been diagnosed or treated for any of the following: Heart Attack, Angioplasty, Coronary Artery Bypass, Stroke, Transient Ischemic Attack, Cancer (excluding non-invasive, non-melanoma Skin Cancer), End-Stage Renal Disease, Liver Cirrhosis, Hepatitis B or C (including Carrier), Multiple Sclerosis, Paralysis, Diabetes (other than during pregnancy), Organ or Bone Marrow Transplant, or Alzheimer's or Senile Dementia?",
            ),
            SOHQuestion('HIV/AIDS',
                        'In the past 10 years, has the proposed insured been positively diagnosed or treated for AIDS, HIV, or ARC?',
            ),

            # 'By a physician' is the difference with these
            SOHQuestion("5yr Heart",
                  "In the last 5 (FIVE) years, has the proposed insured been diagnosed with or treated by a physician for any heart disease (including angina) or any kidney disease except non-chronic kidney stones or infections?",
                  skip_if_coverage_at_most=10000,
            ),
            SOHQuestion("5yr Hypertension / Cholesterol",
                 "In the last 5 (FIVE) years, has the proposed insured been diagnosed with or treated by a physician for uncontrolled high blood pressure (hypertension) and/or uncontrolled elevated cholesterol?",
                 skip_if_coverage_at_most=10000,
            ),
            SOHQuestion("5yr Lung / Colon",
                 "In the last 5 (FIVE) years, has the proposed insured been diagnosed with or treated by a physician for Lung disease requiring hospitalization, colitis, or Crohn's?",
                 skip_if_coverage_at_most=10000,
            ),
            SOHQuestion("5yr Skin Cancer",
                 "In the last 5 (FIVE) years, has the proposed insured been diagnosed with or treated by a physician for any Skin Cancer or/and Precancerous Lesions/Tumors?",
                 skip_if_coverage_at_most=10000,
            ),
            SOHQuestion("5yr HPV/HSV",
                "In the last 5 (FIVE) years, has the proposed insured been diagnosed with or treated by a physician for any Human Papilomavirus (HPV), Herpes Simplex Virus (HSV), chlamydia, or gonorrhea?",
                skip_if_coverage_at_most=10000,
            ),
            SOHQuestion("Abnormal Results",
                 "In the past 2 (TWO) years, has the proposed insured been informed by a member of the medical profession of any abnormal test results or been advised to have any diagnostic tests or procedures which have not yet been completed?",
                 skip_if_coverage_at_most=10000,
            ),
            
        ]),
    ],
    
    'FPP-Gov': [
        ApplicationForm('FPP-Gov Generic', [s for s in all_statecodes if s not in
                                            ['CO', 'FL', 'IL', 'MO', 'OH', 'PA', 'VA', 'WI']], [
            hospitalized_question,
            heart_question,
            cancer_question,
            respiratory_question,
            liver_question,
            aids_question,
        ], is_generic=True),
    ],


}

# FPP-Gov uses FPPTI forms
product_forms['FPP-Gov'] = product_forms['FPPTI']

# For now, define these legacy variables so docusign code works
FPPTI_generic_states = []
for f in product_forms['FPPTI']:
    if f.is_generic:
        FPPTI_generic_states += f.statecodes

FPPCI_generic_states = []
for f in product_forms['FPPCI']:
    if f.is_generic:
        FPPCI_generic_states += f.statecodes

GroupCI_generic_states = []
for f in product_forms['Group CI']:
    if f.is_generic:
        GroupCI_generic_states += f.statecodes
        
FPPGov_generic_states = []
for f in product_forms['FPP-Gov']:
    if f.is_generic:
        FPPGov_generic_states += f.statecodes