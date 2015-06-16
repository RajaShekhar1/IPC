
from .states import all_statecodes, states_by_statecode

def get_template_id_for_product_state(base_product_code, statecode):

    form = StatementOfHealthQuestionService().form_for_product_code_and_state(base_product_code, statecode)

    if not form.docusign_template_id:
        raise Exception("Form %s for product %s state %s has no template ID"%(form.label, base_product_code, statecode))

    return form.docusign_template_id


class StatementOfHealthQuestionService(object):
    def form_for_state(self, product, statecode):
        return self.form_for_product_code_and_state(product.get_base_product_code(), statecode)

    def form_for_product_code_and_state(self, base_product_code, statecode):
        # Return the first form for this product that supports this state.
        #  so, the order matters for the product_forms dict; state-specific should be listed first.
        for form in product_forms.get(base_product_code, []):
            if statecode in form.statecodes:
                return form

        raise Exception("No form exists for product '%s' in state '%s'"%(base_product_code, statecode))

    def get_health_questions(self, product, state):
        form = self.form_for_state(product, state)
        return form.questions
    
    def get_states_with_forms_for_product(self, product):

        code = product.get_base_product_code()
        
        enabled_statecodes = set()
        for form in product_forms.get(code, []):
            if not form.docusign_template_id:
                # Do not allow enrollment in a form if the template ID is not set
                continue

            for statecode in form.statecodes:
                enabled_statecodes.add(statecode)

        # Transform the set into a list of state objects sorted alphabetically by statecode.
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
    def __init__(self, label, statecodes, questions, is_generic=False, docusign_template_id=None):
        self.label = label
        self.statecodes = statecodes
        self.questions = questions
        self.is_generic = is_generic
        self.docusign_template_id = docusign_template_id
    

# Common SOH Questions used in most applications

aids_question = SOHQuestion('HIV/AIDS',
                            'Has any Applicant been diagnosed or treated by a member of the medical profession, or tested positive for: Human Immunodeficiency Virus (HIV), Acquired Immune Deficiency Syndrome (AIDS), or AIDS-Related Complex (ARC)?')

CA_CT_aids_question = SOHQuestion('HIV/AIDS', 'Has any Applicant been diagnosed or treated by a member of the medical profession for Acquired Immune Deficiency Syndrome (AIDS), or AIDS-Related Complex (ARC)?')
ever_been_rejected_question = SOHQuestion('Ever been rejected',
                                          'Has any Applicant ever applied for and been rejected for life insurance?')


hospitalized_question = SOHQuestion('Hospital 90 days',
                                    'Has any Applicant been hospitalized in the past 90 days?')
heart_question = SOHQuestion('Heart',
                             'In the past 5 years, has any Applicant been hospitalized for, been diagnosed or treated by a member of the medical profession or taken prescription medication for Angina, heart attack, stroke, heart bypass surgery, angioplasty, coronary artery stenting, or coronary artery disease?')
cancer_question = SOHQuestion('Cancer',
                              'In the past 5 years, has any Applicant been hospitalized for, been diagnosed or treated by a member of the medical profession or taken prescription medication for any form of cancer to include leukemia or Hodgkin\'s Disease (excluding non-invasive, non-melanoma skin cancer)?')
respiratory_question = SOHQuestion('Respiratory',
                                   'In the past 5 years, has any Applicant been hospitalized for, been diagnosed or treated by a member of the medical profession or taken prescription medication for Chronic obstructive pulmonary disease (COPD), emphysema, or any other chronic respiratory disorder, excluding asthma?')
liver_question = SOHQuestion('Liver',
                             'In the past 5 years, has any Applicant been hospitalized for, been diagnosed or treated by a member of the medical profession or taken prescription medication for Alcoholism or drug or alcohol abuse, cirrhosis, hepatitis, or any other disease of the liver?')

# FPP question lists
fpp_generic_soh_list = [
    aids_question,
    ever_been_rejected_question,
    hospitalized_question,
    heart_question,
    cancer_question,
    respiratory_question,
    liver_question,
]

ca_ct_soh_list = [CA_CT_aids_question] + [q for q in fpp_generic_soh_list[1:]]
fl_soh_list = [
    SOHQuestion('HIV/AIDS', 'Has any Applicant tested positive for exposure to the HIV infection or been diagnosed as having ARC or AIDS caused by the HIV infection or other sickness or condition derived from such infection?'),
    ever_been_rejected_question,
    hospitalized_question,
    SOHQuestion('Heart', 'In the past 5 years, has any Applicant been hospitalized for, been diagnosed or treated by a licensed member of the medical profession or taken prescription medication for Angina, heart attack, stroke, heart bypass surgery, angioplasty, coronary artery stenting, or coronary artery disease?'),
    SOHQuestion('Cancer', 'In the past 5 years, has any Applicant been hospitalized for, been diagnosed or treated by a licensed member of the medical profession or taken prescription medication for any form of cancer to include leukemia or Hodgkin\'s Disease (excluding non-invasive, non-melanoma skin cancer)?'),
    SOHQuestion('Respiratory', 'In the past 5 years, has any Applicant been hospitalized for, been diagnosed or treated by a licensed member of the medical profession or taken prescription medication for Chronic obstructive pulmonary disease (COPD), emphysema, or any other chronic respiratory disorder, excluding asthma?'),
    SOHQuestion('Liver', 'In the past 5 years, has any Applicant been hospitalized for, been diagnosed or treated by a licensed member of the medical profession or taken prescription medication for Alcoholism or drug or alcohol abuse, cirrhosis, hepatitis, or any other disease of the liver?'),
]

# Some common forms between FPPTI and FPPCI
fpp_de_sd_vi_form = ApplicationForm('DE, SD & VI FPP', ['DE', 'SD', 'VI'], fpp_generic_soh_list, docusign_template_id='5CCB952F-AEF5-4017-B46F-B1770B430DE5')
fpp_ca_form = ApplicationForm('CA', ['CA'], ca_ct_soh_list, docusign_template_id='D3CFC594-2E46-427D-904A-6DBEF6B6207D')
fpp_ct_form = ApplicationForm('CT', ['CT'], ca_ct_soh_list, docusign_template_id='79D81EE3-25A3-4967-A58B-9D9B9716836F')
fpp_dc_form = ApplicationForm('DC', ['DC'], fpp_generic_soh_list, docusign_template_id='138851E3-3B66-47DC-89DA-D953C6F618A5')
fpp_fl_form = ApplicationForm('FL', ['FL'], fl_soh_list, docusign_template_id='6047EF30-473B-4148-A674-34C94E194ED2')
fpp_nd_form = ApplicationForm('ND', ['ND'], fpp_generic_soh_list, docusign_template_id='5C8FE7F1-6DCC-46F5-BA03-C5F16FB8F50B')


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


states_without_FPPTI_only = ['IN']
states_without_FPPCI_only = ['CT', 'PA']
states_without_FPP = ['NJ', 'NY', 'VT', 'WA']
# It would be nice to not have this list, but for now update this if more custom forms are added / removed.
states_with_custom_fpp_forms = ['DE', 'SD', 'VI', 'CA', 'CT', 'DC', 'FL', 'ND']

TEMPLATE_ID_FPP_GENERIC = 'E26A7761-1ACF-4993-A2A1-2D021B79E68C'


product_forms = {
    'FPPTI': [
        fpp_de_sd_vi_form,
        fpp_ca_form,
        fpp_ct_form,
        fpp_dc_form,
        fpp_fl_form,
        fpp_nd_form,

        ApplicationForm('Generic',
                        [s for s in all_statecodes if s not in
                        # These states do not do the TI product or have a custom form.
                        states_without_FPP + states_without_FPPTI_only+states_with_custom_fpp_forms], [
            aids_question,
            ever_been_rejected_question,
            hospitalized_question,
            heart_question,
            cancer_question,
            respiratory_question,
            liver_question,
        ], is_generic=True, docusign_template_id=TEMPLATE_ID_FPP_GENERIC),
    ],
    'FPPCI': [
        fpp_de_sd_vi_form,
        fpp_ca_form,
        fpp_dc_form,
        fpp_fl_form,
        fpp_nd_form,

        ApplicationForm('Generic',
            [s for s in all_statecodes if s not in
                        # These states do not do the CI product or have a custom form.
                        states_without_FPP + states_without_FPPCI_only + states_with_custom_fpp_forms],
            [
                aids_question,
                ever_been_rejected_question,
                hospitalized_question,
                heart_question,
                cancer_question,
                respiratory_question,
                liver_question
            ], 
            is_generic=True, docusign_template_id=TEMPLATE_ID_FPP_GENERIC),
    ],

    # FPP-Gov is handled below by copying FPP-TI

    'Group CI': [

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
        ], docusign_template_id='533B6385-6BD0-4815-B95B-FBC2FBE33577'),

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
            is_generic=True,
            docusign_template_id='B57234AB-5EA5-48D4-984F-D3BF07793B9B',
        ),
    ],



}

# FPP-Gov uses FPPTI forms
product_forms['FPP-Gov'] = product_forms['FPPTI']
