
from .states import all_statecodes


class ProductFormService(object):

    def get_application_form_template_id(self, base_product_code, statecode):
        form = self.form_for_product_code_and_state(base_product_code, statecode)
        return form.docusign_template_id

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

    def get_spouse_questions(self):
        return [
            fpp_spouse_treated_6_months,
            fpp_spouse_disabled_6_months,
        ]


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
        self.is_spouse_only = False

    def to_json(self):
        return dict(
            label=self.label,
            question_text=self.question,
            skip_if_coverage_at_most=self.skip_if_coverage_at_most,
        )


class SpouseGIQuestion(JsonSerializable):
    def __init__(self, label, question, is_ignored=False):
        self.label = label
        self.question_text = question
        self.is_ignored = is_ignored
        self.is_spouse_only = True

# FPP Spouse Questions
fpp_spouse_treated_6_months = SpouseGIQuestion(
    "Spouse Treated 6 Months",
    "During the prior 6 months, other than for routine medical care, has your spouse been diagnosed or treated by a member of the medical profession in a hospital or any other medical facility?",
    is_ignored=True,
)
fpp_spouse_disabled_6_months = SpouseGIQuestion("Spouse Disabled 6 Months", 'Has <span data-bind="$root.spouse().name"></span> been <a href="#modal-disabled-definition" data-toggle="modal">disabled</a> in the prior 6 months or received disability payments?')


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

        # FPP-White (FPP-Gov), FPP-Blue, and FPP-Gray are handled below by copying FPP-TI

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
        'ACC': [
            ApplicationForm('Generic', ACC_STATECODES,
                            [], is_generic=True, docusign_template_id=TEMPLATE_ID_FPP_GENERIC)
        ],
        'HI': [
            ApplicationForm('Generic', HI_STATECODES,
                            [], is_generic=True, docusign_template_id=TEMPLATE_ID_FPP_GENERIC)
        ],
    }

    # FPP-White (FPP-Gov), FPP-Blue, and FPP-Gray use FPPTI forms
    app_forms['FPP-Gov'] = app_forms['FPPTI']
    app_forms['FPPTIY'] = app_forms['FPPTI']
    app_forms['FPPTIB'] = app_forms['FPPTI']

    return app_forms


HI_STATECODES = [
    'AL',
    'AK',
    'AZ',
    'AR',
    'DE',
    'FL',
    'GA',
    'IL',
    'IN',
    'IA',
    'KS',
    'KY',
    'LA',
    'ME',
    'MI',
    'MS',
    'MO',
    'MT',
    'NE',
    'NV',
    'NC',
    'OH',
    'OK',
    'OR',
    'PA',
    'RI',
    'SC',
    'TN',
    'TX',
    'UT',
    'VA',
    'WV',
    'WI',
]

ACC_STATECODES = [
    'AL',
    'AK',
    'AZ',
    'AR',
    'CO',
    'DE',
    'DC',
    'FL',
    'GA',
    'HI',
    'ID',
    'IL',
    'IN',
    'IA',
    'KS',
    'KY',
    'LA',
    'MI',
    'MS',
    'MO',
    'MT',
    'NE',
    'NV',
    'NM',
    'NC',
    'ND',
    'OH',
    'OK',
    'OR',
    'RI',
    'SC',
    'SD',
    'TN',
    'TX',
    'UT',
    'VT',
    'VA',
    'WV',
    'WI',
    'WY',
]

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
Make sure you know the facts. Contact your existing company or its agent for information about the old policy or contract. If you request one, an in force illustration, policy summary or available disclosure documents must be sent to you by the existing insurer. Ask for and retain all sales material used by the agent in the sales presentation. Be sure that you are making an informed decision.
\
""".split('\n')
)

CA_fpp_replacement_form = ReplacementForm(
    statecodes=['CA'],
    docusign_template_id='C3F52784-1C3C-454F-B268-58062E1D0F38',
    paragraphs="""\
Are you thinking about buying a new life insurance policy or annuity and discontinuing or changing an existing one? If you are, your decision could be a good one - or a mistake. You will not know for sure unless you make a careful comparison of your existing benefits and the proposed benefits.
Make sure you understand the facts. You should ask the company or agent that sold you your existing policy to give you information about it.
Hear both sides before you decide. This way you can be sure you are making a decision that is in your best interest.
We are required by law to notify your existing company that you may be replacing their policy.
\
""".split('\n')
)

DE_fpp_replacement_form = ReplacementForm(
    statecodes=['DE'],
    docusign_template_id='F581E793-5BB8-4B5D-8DC3-156E9A307D4A',
    paragraphs="""\
It is in your best interest to get all the facts before making a decision. Make sure you fully understand both the proposed new policy and your existing insurance. New policies may contain provisions which limit benefits during the initial period of the contract, in particular, the suicide and incontestable clauses.
To assist you in evaluating the proposed and the existing insurance, Delaware Insurance Regulation 30 requires that the insurer advising or recommending replacement:
Provide the consumer, not later than the date the policy or contract is delivered, a concise summary of the policy or contracts to be issued.
Allow a 20 day period following the delivery of the policy during which time the consumer may surrender the new policy for a full refund.
Advise the present insurance company(s) of the pending replacement.
This same regulation requires your present insurer to provide, on your request, a similar summary describing your present insurance. This information will be provided if you request it using the form at the end of this application enrollment.
IT IS SELDOM WISE TO TERMINATE YOUR EXISTING POLICY UNTIL YOUR NEW POLICY HAS BEEN ISSUED AND YOU HAVE EXAMINED IT AND FOUND IT TO BE ACCEPTABLE.
\
""".split('\n')
)

FL_fpp_replacement_form = ReplacementForm(
    statecodes=['FL'],
    docusign_template_id='04ADEC90-34F7-4329-B323-5E608DA51302',
    paragraphs="""\
A decision to buy a new policy and discontinue or change an existing policy may be a wise choice or a mistake.
Get all the facts. Make sure you fully understand both the proposed policy and your existing policy or policies. New policies may contain clauses which limit or exclude coverage of certain events in the initial period of the contract, such as the suicide and incontestable clauses which may have already been satisfied in your existing policy or policies.
Your best source of facts on the proposed policy is the proposed company and its agent. The best source on your existing policy is the existing company and its agent.
Hear from both before you make your decision. This way you can be sure your decision is in your best interest.
If you indicate that you intend to replace or change an existing policy, Florida regulations require notification of the company that issued the policy.
Florida regulations give you the right to receive a written Comparative Information Form which summarizes your policy values. You may indicate whether or not you wish to receive a Comparative Information Form from the proposed company and your existing insurer or insurers by placing your initials in the appropriate box at the conclusion of this enrollment.
DO NOT TAKE ACTION TO TERMINATE YOUR EXISTING POLICY UNTIL YOUR NEW POLICY HAS BEEN ISSUED AND YOU HAVE EXAMINED IT AND FOUND IT ACCEPTABLE.
\
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
\
""".split('\n')
)

ID_fpp_replacement_form = ReplacementForm(
    statecodes=['ID'],
    docusign_template_id='6B692851-B782-455D-A246-F8A022665602',
    paragraphs="""\
Are you thinking about buying a new life insurance policy or annuity and discontinuing or changing an existing one? If you are, your decision could be a good one - or a mistake. You will not know for sure unless you make a careful comparison of your existing benefits and the proposed benefits. Make sure you understand the facts. You should ask for the advice of the company or agent that sold you your existing policy to give you information concerning any proposed replacement.
As a general rule, there are disadvantages to dropping your existing life insurance or annuities. Hear both sides before you decide. That way you can be sure you are making a decision that is in your best interest.
Idaho law requires your existing company to be notified that you may be replacing their policy.
\
""".split('\n')
)

IL_fpp_replacement_form = ReplacementForm(
    statecodes=['IL'],
    docusign_template_id='3CB0CF26-86AA-41E3-BF79-9DDA87ED75FF',
    paragraphs="""\
Are you thinking about buying a new life insurance policy or annuity and discontinuing or changing an existing one? If you are, your decision could be a good one - or a mistake. You will not know for sure unless you make a careful comparison of your existing benefits and the proposed benefits.
Make sure you understand the facts. You should ask the insurance producer or company that sold you your existing policy to give you information about it.
Hear both sides before you decide. This way you can be sure you are making a decision that is in your best interest.
We are required by law to notify your existing company that you may be replacing their policy.
\
""".split('\n')
)

IN_fpp_replacement_form = ReplacementForm(
    statecodes=['IN'],
    docusign_template_id='15F43418-C806-4195-AC69-683E22B44157',
    paragraphs="""\
If you are thinking about discontinuing or changing an existing life insurance policy or annuity contract and buying a replacement, your decision could be a good one - or possibly a mistake. Make sure that you understand the facts. You should:
<ul>\
<li>Make a careful comparison of your existing policy and the proposed policy.</li>\
<li>Ask the company or agent that sold you your existing policy to provide you with complete information about it.</li>\
<li>Consider both sides before you decide.</li>\
<li>Determine what you want your insurance program to do.</li>\
<li>Consider your present health. You may have had a change which could affect your insurability, so make sure to continue your present policy until a new policy is delivered to you and accepted by you.</li>\
</ul>\
Indiana Department of Insurance Regulation, 760 IAC 1-16.1 requires that the company making the replacement notify your existing insurance company that you may be replacing your existing policy. (You have the right, within 20 days after delivery of a replacement policy, to return it to the company and to claim an unconditional refund of all premiums paid on it.)
\
""".split('\n')
)

MA_fpp_replacement_form = ReplacementForm(
    statecodes=['MA'],
    docusign_template_id='AA0D8B97-FEA8-4B92-AA12-43F0E481A1F1',
    paragraphs="""\
IMPORTANT NOTICE REQUIRED BY THE COMMISSIONER OF INSURANCE
READ CAREFULLY BEFORE PROCEEDING
This Notice is required by the Commissioner of Insurance because you have indicated that you are buying a new life insurance policy or annuity and discontinuing or changing an existing one. Such a decision could be a good one, or a mistake. You will not know for sure until you make a careful comparison of your existing policy and the proposed replacement policy. Premiums alone are not determinative of low cost. Take the time to obtain and understand the facts.
We are required by law to notify your existing company that you may be replacing their policy.
Consider both sides before you decide. This way you can be sure you are making a decision that is in your best interest.
Cash Value Insurance: To make a comparison of cash value policies (policies with loan or surrender values in addition to death protection), consideration must be given to each policy's cash values, premiums, coverage amounts and dividends, if any, over the life of the policy.
To simplify this task, you may wish to request from your existing insurance company and the company issuing the replacement policy yield index figures for five, ten and 20 years. The yield index is a percentage that represents an estimate of the interest rate the insurer projects you will earn on the savings portion of the cash value policy. The policy with the higher yield index will generally be the better buy.
The Yield Index Committee of the National Association of Insurance Commissioners in 1986 devised a method for calculating a yield index. In order to request this yield index information, merely check the box below and your request will be forwarded to both insurance companies.
You can also compare the cash values and/or surrender values listed in the replacing company's policy summary for the first five policy years with those in your current policy for the next five years. Low cash values or surrender values in early policy years are often the result of high expenses associated with issuing a new policy. If the replacement policy has low values in its early years, it will usually take longer for it to provide you with benefits that equal or exceed the benefits of your existing policy. In some cases, the replacement policy may never provide benefits equal to those in your present policy.

Term Insurance: If you are replacing your present insurance policy with term insurance (policies that provide death protection only), it makes sense to shop for a low- cost policy. Costs for term insurance vary widely and substantial savings may be realized by comparison shopping. Premiums alone are not always determinative of low cost since some policies pay dividends and others do not. You may wish to request interest- adjusted cost indices for five, ten and 20 years from several insurance companies including your existing insurer to help you compare term insurance premiums. The policy with the lower index numbers is usually the better buy.
\
""".split('\n')
)

MI_fpp_replacement_form = ReplacementForm(
    statecodes=['MI'],
    docusign_template_id='7BD30243-4AC5-4682-A68A-9C7469C58DF9',
    paragraphs="""\
You are contemplating the purchase of a life insurance policy or annuity contract. In some cases this purchase may involve discontinuing or changing an existing policy or contract. If so, a replacement is occurring. Financed purchases are also considered replacements.
A replacement occurs when a new policy or contract is purchased and, in connection with the sale, you discontinue making premium payments on the existing policy or contract, or an existing policy or contract is surrendered, forfeited, assigned to the replacing insurer, or otherwise terminated or used in a financed purchase.
A financed purchase occurs when the purchase of a new life insurance policy involves the use of funds obtained by the withdrawal or surrender of or by borrowing some or all of the policy values, including accumulated dividends, of an existing policy to pay all or part of any premium or payment due on the new policy. A financed purchase is a replacement.
You should carefully consider whether a replacement is in your best interests. You will pay acquisition costs and there may be surrender costs deducted from your policy or contract. You may be able to make changes to your existing policy or contract to meet your insurance needs at less cost. A financed purchase will reduce the value of your existing policy and may reduce the amount paid upon the death of the insured.
We want you to understand the effects of replacements before you make your purchase decision and ask that you answer the following questions and consider the questions below.
Make sure you know the facts. Contact your existing company or its agent for information about the old policy or contract. If you request one, an in force illustration, policy summary or available disclosure documents must be sent to you by the existing insurer. Ask for and retain all sales material used by the agent in the sales presentation. Be sure that you are making an informed decision.
\
""".split('\n')
)

MN_fpp_replacement_form = ReplacementForm(
    statecodes=['MN'],
    docusign_template_id='A0E37A85-7E4F-448B-8BEE-7CB633D71BB7',
    paragraphs="""\
REPLACEMENT is any transaction where, in connection with the purchase of new insurance or a new annuity, you lapse, surrender, convert to paid-up insurance, place on extended term, or borrow all or part of the policy loan values on an existing insurance policy or an annuity. (See reverse side for definitions.)
IF YOU INTEND TO REPLACE COVERAGE
In connection with the purchase of this insurance or annuity, if you have replaced or intend to replace your present life insurance coverage or annuity(ies), you should be certain that you understand all the relevant factors involved.
You should be aware that you may be required to provide evidence of insurability and
(1) If your health condition has changed since the application was taken on your present policies, you may be required to pay additional premiums under the new policy, or be denied coverage.
(2) Your present occupation or activities may not be covered or could require additional premiums.
(3) The incontestable and suicide clause will begin anew in a new policy. This could result in a claim under the new policy being denied that would otherwise have been paid.
(4) Current law may not require your present insurer(s) to refund any premiums.
(5) It is to your advantage to obtain information regarding your existing policies or annuity contracts from the insurer or agent from whom you purchased the policy or annuity contract.

(If you are purchasing an annuity, clauses (1), (2), and (3) above would not apply to the new annuity contract.)

THE INSURANCE OR ANNUITY I INTEND TO PURCHASE FROM 5STAR LIFE INSURANCE COMPANY MAY REPLACE OR ALTER EXISTING LIFE INSURANCE POLICY(IES) OR ANNUITY CONTRACT(S).
\
""".split('\n')
)

MO_fpp_replacement_form = ReplacementForm(
    statecodes=['MO'],
    docusign_template_id='12A979B9-95D9-4B4B-9EEF-9E38DEA64483',
    paragraphs="""\
Replacing Your Life Insurance Policy or Annuity?
Are you thinking about buying a new policy and discontinuing or changing an existing policy? If you are, your decision could be a good one - or a mistake. You will not know for sure unless you make a careful comparison of your existing policy and the proposed policy.
Make sure you understand the facts. Ask the company or insurance producer that sold you your existing policy to provide you with a policy summary statement.
The reverse side contains a check list of some of the items you should consider in making our decision. TAKE TIME TO READ IT.
Do not let one insurance producer or insurer prevent you from obtaining information from another insurance producer or insurer which may be to your advantage.
Hear both sides before you decide. This way you can be sure you are making a decision that is in your best interest.
We are required to notify your existing company that you may be replacing their policy.

ITEMS TO CONSIDER
<ol>
<li>If the policy coverages are basically similar, premiums for a new policy may be higher because rates increase as your age increases.</li>\
<li>Cash values and dividends, if any, may grow slower under a new policy initially because of the initial costs of issuing a policy.</li>\
<li>Your present insurance company may be able to make a change on terms which may be more favorable than if you replace existing insurance with new insurance.</li>\
<li>If you borrow against an existing policy to pay premiums on a new policy, death benefits payable under your existing policy will be reduced by the amount of any unpaid loan, including unpaid interest.</li>\
<li>Current interest rates are not guaranteed. Guaranteed interest rates are usually considerably lower than current rates. What rates are guaranteed?</li>\
<li>Are premiums guaranteed or subject to change - up or down?</li>\
<li>Participating policies pay dividends that may materially reduce the cost of insurance over the life of the contract. Dividends, however, are not guaranteed.</li>\
<li>CAUTION, you are urged not to take action to terminate, assign or alter your existing life insurance coverage until after you have been issued the new policy, examined it and have found it to be acceptable to you; and</li>\
<li>REMEMBER, you have twenty (20) days following receipt to examine the contents of any individual life insurance policy or annuity. If you are not satisfied with it for any reason, you have the right to return it to the insurer at its home or branch office, or to the insurance producer through whom it was purchased, for a full refund of premium.</li>\
</ol>
\
""".split('\n')
)

NV_fpp_replacement_form = ReplacementForm(
    statecodes=['NV'],
    docusign_template_id='5CD85DC9-F8DB-4A78-8E6E-B0DB14749CB9',
    paragraphs="""\
You have been offered a policy to replace all or part of your existing policy of life insurance.
Before you replace your existing policy, you should consider whether you could suffer a Financial Loss under the new policy because of your Age or the condition of your Health. You should also consider whether you will pay more for premiums because of your age or health.
You Will incur additional costs to acquire the new policy, including the payment of commissions to the agent advocating the replacement of your existing policy.
To make an informed decision about the replacement of your policy, you should discuss the provisions of your existing policy with your agent or the company which issued it to determine whether your policy can be changed to meet your present needs.
Your new policy provides 10 days for you to decide whether you wish to keep it.
The agent who is offering to replace your existing policy is required to obtain your signature on this notice. Also, he will be notifying your existing insurance company that you are considering the replacement of your policy.
\
""".split('\n')
)

OK_fpp_replacement_form = ReplacementForm(
    statecodes=['OK'],
    docusign_template_id='9C4D288B-8D77-4DDC-8D49-DE1313443FB3',
    paragraphs="""\
NOTICE TO APPLICANTS REGARDING REPLACEMENT OF LIFE INSURANCE OR AN ANNUITY. THIS NOTICE IS FOR YOUR BENEFIT AND IS REQUIRED BY LAW.
<ol>\
<li>If you are urged to purchase life insurance and to surrender, lapse, or in any other way change the status of existing life insurance, the agent is required to give you this notice.</li>\
<li>It may not be advantageous to drop or change existing life insurance in favor of new life insurance, whether issued by the same or a different insurance company. Some of the disadvantages are:<br>\
a. The amount of the annual premium under an existing policy may be lower than that under a new policy having the same or similar benefits.<br>\
b. Generally, the initial costs of life insurance policies are charged against the cash value increases in the earlier policy years, the replacement of an old policy could result in the policyholder sustaining the burden of these costs twice.<br>\
c. The incontestable and suicide clauses begin anew in a new policy. This could result in a claim under a new policy being denied by the company which would have been paid under the old policy.<br>\
d. Existing policies may have more favorable provisions than new policies in such areas as settlement options and disability benefits.<br>\
e. An existing policy may have a reserve value in addition to any cash value which may be of some benefit to the insured.<br>\
f. The insurance company carrying your current insurance policy can often make a desired change on terms which would be more favorable than if existing insurance is replaced with new insurance.</li>\
<li>It may not be advantageous to change an existing policy to reduce paid-up or extended term insurance or to borrow against its loan value beyond your expected ability or intention to repay in order to obtain funds for premiums on a new policy.</li>\
<li>There may be a situation in which a replacement policy is advantageous. You may want to receive the comments of the present insurance company before deciding this important financial matter.</li>
\
</ol>
\
""".split('\n')
)

PA_fpp_replacement_form = ReplacementForm(
    statecodes=['PA'],
    docusign_template_id='A739D21A-F30C-4915-9927-1178A52F726F',
    paragraphs="""\
You have indicated that you intend to replace existing life insurance or annuity coverage in connection with the purchase of our life insurance or annuity policy. As a result, we are required to send you this notice. Please read it carefully.
Whether it is to your advantage to replace your existing insurance or annuity coverage, only you can decide. It is in your best interest, however, to have adequate information before a decision to replace your present coverage becomes final so that you may understand the essential features of the proposed policy and your existing insurance or annuity coverage.
You may want to contact your existing life insurance or annuity company or its agent for additional information and advice or discuss your purchase with other advisors. Your existing company will provide this information to you. The information you receive should be of value to you in reaching a final decision.
If either the proposed coverage or the existing coverage you intend to replace is participating, you should be aware that dividends may materially reduce the cost of insurance and are an important factor to consider. Dividends, however, are not guaranteed.
You should recognize that a policy which has been in existence for a period of time may have certain advantages to you over a new policy. If the policy coverages are basically similar, the premiums for a new policy may be higher because rates increase as your age increases. Under your existing policy, the period of time during which the issuing company could contest the policy because of a material misrepresentation or omission concerning the medical information requested in your application, or deny coverage for death caused by suicide, may have expired or may expire earlier than it will under the proposed policy. Your existing policy may have options which are not available under the policy being proposed to you or may not come into effect under the proposed policy until a later time during your life. Also, your proposed policy's cash values and dividends, if any, may grow slower initially because the company will incur the cost of issuing your new policy. On the other hand, the proposed policy may offer advantages which are more important to you.
If you are considering borrowing against your existing policy to pay the premiums on the proposed policy, you should understand that in the event of your death, the amount of any unpaid loan, including unpaid interest, will be deducted from the benefits of your existing policy thereby reducing your total insurance coverage.
After we have issued your policy, you will have 20 days from the date the new policy is received by you to notify us you are cancelling the policy issued on your application and you will receive back all payments you made to us.
You are urged not to take action to terminate or alter your existing life insurance or annuity coverage until you have been issued the new policy, examined it and have found it acceptable to you.
\
""".split('\n')
)

TN_fpp_replacement_form = ReplacementForm(
    statecodes=['TN'],
    docusign_template_id='D0A05387-6A06-4BA7-91CF-F69BD429A3E7',
    paragraphs="""\
Are you thinking about buying a new life insurance policy and discontinuing or changing an existing one? If you are, your decision could be a good one - or a mistake. You will not know for sure unless you make a careful comparison of your existing benefits and the proposed benefits.
Make sure you understand the facts. You should ask the company or agent that sold you your existing policy to give you information about it. You are urged not to take action to terminate, assign or alter your existing life insurance coverage until you have been issued the new policy, examined it and have found it acceptable.
Hear both sides before you decide. This way you can be sure you are making a decision that is in your best interest.
IF YOU SHOULD FAIL TO QUALIFY FOR THE LIFE INSURANCE FOR WHICH YOU HAVE APPLIED, YOU MAY FIND YOURSELF UNABLE TO PURCHASE OTHER LIFE INSURANCE OR ABLE TO PURCHASE IT ONLY AT SUBSTANTIALLY HIGHER RATES.
We are required by law to notify your existing company that you may be replacing their policy.

\
""".split('\n')
)

# blank template to gracefully fail if in one of these states until we determine application logic to not require replacement for these states
empty_fpp_replacement_form = ReplacementForm(
    statecodes=['CT', 'DC', 'ND', 'VI'],
    docusign_template_id='0F15241A-B176-4E5B-9E59-0809053994DC',
    paragraphs="""\
Your state does not require completion of a replacement certificate.  Please select "N" for the following two questions:\
""".split('\n')
)

# blank template to gracefully fail if in one of these states until we determine application logic to not allow replacement for these states
NoNo_fpp_replacement_form = ReplacementForm(
    statecodes=['KS', 'KY'],
    docusign_template_id='0F15241A-B176-4E5B-9E59-0809053994DC',
    paragraphs="""\

<span class=\"error">STOP!  Your state does not accept insurance replacements.  We cannot continue this application. <br><br>\
Please cancel this enrollment application and consult with your enrollment professional.<br><br>\
If you proceed, your application WILL be declined for coverage.</span>
<br><br><br><br>
\
""".split('\n')
)

SD_fpp_replacement_form = ReplacementForm(
    statecodes=['SD'],
    docusign_template_id='272C42C9-A20D-4205-BEEF-5AFC23720251',
    paragraphs="""\
In order to complete this application, you will complete a REPLACEMENT OF LIFE INSURANCE OR ANNUITY notice at the end of this application interview.  It will require you to provide the following informationin order to submit this application:
NAME OF INSURED
ADDRESS and PHONE OF INSURED
NAME OF EXISTING INSURER
EXISTING POLICY NUMBER
NAME OF EXISTING AGENT
TYPE OF EXCHANGE OF EXISTING COVERAGE (Example: Lapsed policy, loan, or surrender)
REPLACEMENT POLICY NUMBER (or other identifying number [application or receipt number or unique personally identifiable number or company assigned number])
GENERIC DESCRIPTION OF REPLACING POLICY

Please note your application will not be able to be completed without these items.
\
""".split('\n')
)


def get_replacement_forms():
    fpp_forms = [
        CA_fpp_replacement_form,
        DE_fpp_replacement_form,
        FL_fpp_replacement_form,
        GA_fpp_replacement_form,
        ID_fpp_replacement_form,
        IL_fpp_replacement_form,
        IN_fpp_replacement_form,
        MA_fpp_replacement_form,
        MI_fpp_replacement_form,
        MN_fpp_replacement_form,
        MO_fpp_replacement_form,
        NV_fpp_replacement_form,
        OK_fpp_replacement_form,
        PA_fpp_replacement_form,
        TN_fpp_replacement_form,
        SD_fpp_replacement_form,
        NoNo_fpp_replacement_form,
        empty_fpp_replacement_form,
        generic_fpp_replacement_form,
    ]

    return {
        'FPPTI': fpp_forms,
        'FPPCI': fpp_forms,
        'FPP-Gov': fpp_forms,
        'FPPTIB': fpp_forms,
        'FPPTIY': fpp_forms,
        'ACC': fpp_forms,
        'HI': fpp_forms,
    }
