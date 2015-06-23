
from .states import all_statecodes



class ProductFormService(object):

    def get_application_form_template_id(self, base_product_code, statecode):
        return self.form_for_product_code_and_state(base_product_code, statecode)

    def get_replacement_template_id(self, base_product_code, statecode):
        form = self.get_replacement_form(base_product_code, statecode)
        return form.docusign_template_id

    def form_for_state(self, product, statecode):
        return self.form_for_product_code_and_state(product.get_base_product_code(), statecode)

    def form_for_product_code_and_state(self, base_product_code, statecode):
        # Return the first application form for this product that supports this state.
        #  so, the order matters for the product_forms dict; state-specific should be listed first.
        for form in self.get_all_application_forms().get(base_product_code, []):
            if statecode in form.statecodes:
                return form

        return None

    def get_all_application_forms(self):
        return get_product_application_forms()

    def get_replacement_form(self, base_product_code, statecode):
        # Return the first application form for this product that supports this state.
        #  so state-specific should be listed first.
        for form in get_replacement_forms().get(base_product_code, []):
            if statecode in form.statecodes:
                return form

        raise Exception("No form exists for product '%s' in state '%s'"%(base_product_code, statecode))

    def get_replacement_forms_for_product(self, base_product_code):
        return get_replacement_forms().get(base_product_code, [])


# Application forms
class ApplicationForm(object):
    def __init__(self, label, statecodes, questions, is_generic=False, docusign_template_id=None):
        self.label = label
        self.statecodes = statecodes
        self.questions = questions
        self.is_generic = is_generic
        self.docusign_template_id = docusign_template_id


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

def get_product_application_forms():
    app_forms = {
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
    app_forms['FPP-Gov'] = app_forms['FPPTI']

    return app_forms



class ReplacementForm(object):
    def __init__(self, statecodes, docusign_template_id, paragraphs):
        self.statecodes = statecodes
        self.docusign_template_id = docusign_template_id
        self.paragraphs = paragraphs


generic_fpp_replacement_form = ReplacementForm(
    statecodes=['AK', 'AL', 'AR', 'AZ', 'CO', 'IA', 'KS', 'KY', 'LA', 'MD', 'ME', 'MS', 'MT',
                'NC', 'NE', 'NH', 'NJ', 'NM', 'OH', 'OR', 'RI', 'SC', 'TX', 'UT', 'VA', 'VT',
                'WI', 'WV'],
    docusign_template_id='7286ACB9-8B08-43BB-99EB-C7A37B8B8F2A',
    paragraphs="""\
You are contemplating the purchase of a life insurance policy or annuity contract. In some cases this purchase may involve discontinuing or changing an existing policy or contract. If so, a replacement is occurring. Financed purchases are also considered replacements.
A replacement occurs when a new policy or contract is purchased and, in connection with the sale, you discontinue making premium payments on the existing policy or contract, or an existing policy or contract is surrendered, forfeited, assigned to the replacing insurer, or otherwise terminated or used in a financed purchase.
A financed purchase occurs when the purchase of a new life insurance policy involves the use of funds obtained by the withdrawal or surrender of or by borrowing some or all of the policy values, including accumulated dividends, of an existing policy to pay all or part of any premium or payment due on the new policy. A financed purchase is a replacement.
You should carefully consider whether a replacement is in your best interests. You will pay acquisition costs and there may be surrender costs deducted from your policy or contract. You may be able to make changes to your existing policy or contract to meet your insurance needs at less cost. A financed purchase will reduce the value of your existing policy and may reduce the amount paid upon the death of the insured.
We want you to understand the effects of replacements before you make your purchase decision and ask that you answer the following questions and consider the questions below.
Make sure you know the facts. Contact your existing company or its agent for information about the old policy or contract. If you request one, an in force illustration, policy summary or available disclosure documents must be sent to you by the existing insurer. Ask for and retain all sales material used by the agent in the sales presentation. Be sure that you are making an informed decision.\
""".split('\n')
)

GA_fpp_replacement_form = ReplacementForm(
    statecodes=['GA'],
    docusign_template_id='3D0498C5-B843-4C6A-A3D1-1E6317F2EDA3',
    paragraphs="""\
Are you thinking about buying a new policy and discontinuing or changing an existing policy? If you are, your decision could be a good one or a mistake. You will not know for sure unless you make a careful comparison of your existing policy and the proposed policy.
Make sure you understand the facts. Georgia law gives you the right to obtain a policy summary statement from your existing insurer at any time. Ask the company or agent that sold you your existing policy to give you information about it.
The reverse side contains a checklist of some of the items you should consider in making your decision. TAKE TIME TO READ IT.
Do not let one agent or insurer prevent you from obtaining information from another agent or insurer which may be to your advantage.
Hear both sides before you decide. This way you can be sure you are making a decision that is in your best interest.
We are required to notify your existing company that you may be replacing their policy.

ITEMS TO CONSIDER
<ol>\
<li>If the policy coverages are basically similar, premiums for a new policy may be higher because rates increase as your age increases.</li>\
<li>Cash values and dividends, if any, may grow slower under a new policy initially because of the initial costs of issuing a policy.</li>\
<li>Your present insurance company may be able to make a change on terms which may be more favorable than if you replace existing insurance with new insurance.</li>\
<li>If you borrow against an existing policy to pay premiums on a new policy, death benefits payable under your existing policy will be reduced by the amount of any unpaid loan, including unpaid interest.</li>\
<li>Current interest rates are not guaranteed. Guaranteed interest rates are usually considerably lower than current rates. What rates are guaranteed?</li>\
<li>Are premiums guaranteed or subject to change  up or down?</li>\
<li>Participating policies pay dividends that may materially reduce the cost of insurance over the life of the contract. Dividends, however, are not guaranteed.</li>\
<li>CAUTION, you are urged not to take action to terminate, assign, or alter your existing life insurance coverage until after you have been issued the new policy, examined it and have found it to be acceptable to you.</li>\
</ol>\
and
REMEMBER, you have thirty (30) days following receipt of any individual life insurance policy to examine its contents. If you are not satisfied with it for any reason, you have the right to return it to the insurer at its home or branch office or to the agent through whom it was purchased, for a full refund of premium.
""".split('\n')
)





def get_replacement_forms():
    fpp_forms = [
        GA_fpp_replacement_form,
        generic_fpp_replacement_form,
    ]

    return {
        'FPPTI':fpp_forms,
        'FPPCI':fpp_forms,
        'FPP-Gov': fpp_forms,
    }