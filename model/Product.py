from datetime import datetime

from dateutil.parser import parse
from dateutil.relativedelta import relativedelta

from model.RateTable import (
    build_FPPTI_rate_table,
    build_FPPCI_rate_table,
    
)

from model.Recommendations import (
    FPPTI_recommendations,
    FPPCI_recommendations, 
    Recommendations,
)

#
# generic state lists used here and also in Docusign.config
#
FPPTI_generic_states = ["AL", "AK", "AZ", "AR", "CA", "DE", "GA", "HI", "ID", "IA", "KS", "KY", "LA", "MI", "MS", "MT", "NE", "NV", "NM", "OK", "OR", "RI", "SC", "SD", "TN", "TX", "UT", "VI", "WV", "WY"]
FPPCI_generic_states = ["AL", "AK", "AZ", "AR", "CA", "DE", "GA", "HI", "ID", "IN", "IA", "KS", "KY", "LA", "MI", "MS", "MT", "NE", "NV", "NM", "OK", "OR", "PR", "RI", "SC", "SD", "TN", "TX", "UT", "VI", "WV", "WY"]

FPP_health_questions_by_form = {
        "Generic": [
            "Has any Applicant been hospitalized in the past 90 days?",
            "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Angina, heart attack, stroke, heart bypass surgery, angioplasty, coronary artery stenting, or coronary artery disease?",
            "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for any form of cancer to include leukemia or Hodgkin's Disease (excluding non-invasive, non-melanoma skin cancer)?",
            "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Chronic obstructive pulmonary disease (COPD), emphysema, or any other chronic respiratory disorder, excluding asthma?",
            "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Alcoholism or drug or alcohol abuse, cirrhosis, hepatitis, or any other disease of the liver?",
            "Has any Applicant been diagnosed or treated by a physician, or tested positive for: Human Immunodeficiency Virus (HIV), Acquired Immune Deficiency Syndrome (AIDS), or AIDS-Related Complex (ARC)?",
            "Has any Applicant ever applied for and been rejected for life insurance?",
        ],
        "WS-UST App R409-CO": [
            "Has any Applicant been hospitalized in the past 90 days?",
            "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Angina, heart attack, stroke, heart bypass surgery, angioplasty, coronary artery stenting, or coronary artery disease?",
            "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for any form of cancer to include leukemia or Hodgkin's Disease (excluding non-invasive, non-melanoma skin cancer)?",
            "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Chronic obstructive pulmonary disease (COPD), emphysema, or any other chronic respiratory disorder, excluding asthma?",
            "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Alcoholism or drug or alcohol abuse, cirrhosis, hepatitis, or any other disease of the liver?",
            "Have you had or been told by a member of the medical profession that you have AIDS or HIV infection?",
            "Has any Applicant ever applied for and been rejected for life insurance?"
        ],
        "WS-UST App R409-IL": [
            "Has any Applicant been hospitalized in the past 90 days? ",
            "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Angina, heart attack, stroke, heart bypass surgery, angioplasty, coronary artery stenting, or coronary artery disease?",
            "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for any form of cancer to include leukemia or Hodgkin's Disease (excluding non-invasive, non-melanoma skin cancer)?",
            "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Chronic obstructive pulmonary disease (COPD), emphysema, or any other chronic respiratory disorder, excluding asthma?",
            "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Alcoholism or drug or alcohol abuse, cirrhosis, hepatitis, or any other disease of the liver?",
            "Has any Applicant been diagnosed, tested, or treated by a physician for: Human Immunodeficiency Virus (HIV), Acquired Immune Deficiency Syndrome (AIDS), or AIDS-Related Complex (ARC)?",
            "Has any Applicant ever applied for and been rejected for life insurance?"
        ],
        "WS-UST App R409-OH": [
            "Has any Applicant been hospitalized in the past 90 days? ",
            "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Angina, heart attack, stroke, heart bypass surgery, angioplasty, coronary artery stenting, or coronary artery disease?",
            "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for any form of cancer to include leukemia or Hodgkin's Disease (excluding non-invasive, non-melanoma skin cancer)?",
            "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Chronic obstructive pulmonary disease (COPD), emphysema, or any other chronic respiratory disorder, excluding asthma?",
            "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Alcoholism or drug or alcohol abuse, cirrhosis, hepatitis, or any other disease of the liver?",
            "Has any Applicant been diagnosed or treated by a physician, or tested positive for: Human Immunodeficiency Virus (HIV), Acquired Immune Deficiency Syndrome (AIDS), or AIDS-Related Complex (ARC)?",
            "Has any Applicant ever applied for and been rejected for life insurance?"
        ],
        "WS-UST App R409-VA": [
            "Has any Applicant been hospitalized in the past 90 days? ",
            "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Angina, heart attack, stroke, heart bypass surgery, angioplasty, coronary artery stenting, or coronary artery disease?",
            "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for any form of cancer to include leukemia or Hodgkin's Disease (excluding non-invasive, non-melanoma skin cancer)?",
            "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Chronic obstructive pulmonary disease (COPD), emphysema, or any other chronic respiratory disorder, excluding asthma?",
            "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Alcoholism or drug or alcohol abuse, cirrhosis, hepatitis, or any other disease of the liver?",
            "Has any Applicant been diagnosed or treated by a physician, or tested positive for: Human Immunodeficiency Virus (HIV), Acquired Immune Deficiency Syndrome (AIDS), or AIDS-Related Complex (ARC)?",
            "Has any Applicant ever applied for and been rejected for life insurance?"
        ],
        "WS-UST App R409-PA": [
            "Has any Applicant been hospitalized in the past 90 days? ",
            "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Angina, heart attack, stroke, heart bypass surgery, angioplasty, coronary artery stenting, or coronary artery disease?",
            "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for any form of cancer to include leukemia or Hodgkin's Disease (excluding non-invasive, non-melanoma skin cancer)?",
            "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Chronic obstructive pulmonary disease (COPD), emphysema, or any other chronic respiratory disorder, excluding asthma?",
            "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Alcoholism or drug or alcohol abuse, cirrhosis, hepatitis, or any other disease of the liver?",
            "Has any Applicant been diagnosed or treated by a physician, or tested positive for: Human Immunodeficiency Virus (HIV), Acquired Immune Deficiency Syndrome (AIDS), or AIDS-Related Complex (ARC)?",
            "Has any Applicant ever applied for and been rejected for life insurance?"
        ],
        "WS-UST App R409-WI": [
            "Has any Applicant been hospitalized in the past 90 days? ",
            "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Angina, heart attack, stroke, heart bypass surgery, angioplasty, coronary artery stenting, or coronary artery disease?",
            "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for any form of cancer to include leukemia or Hodgkin's Disease (excluding non-invasive, non-melanoma skin cancer)?",
            "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Chronic obstructive pulmonary disease (COPD), emphysema, or any other chronic respiratory disorder, excluding asthma?",
            "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Alcoholism or drug or alcohol abuse, cirrhosis, hepatitis, or any other disease of the liver?",
            "Has any Applicant been diagnosed by a physician as having Human Immuno-deficiency Virus (HIV), Acquired Immune Deficiency Syndrome (AIDS), or AIDS-Related Complex (ARC)? (The applicant need not reveal HIV test results received from an anonymous counseling and testing site or the results of a home test kit.) ",
            "Has any Applicant ever applied for and been rejected for life insurance?"
        ],
        "WS-UST App R409-FL": [
            "Has any Applicant been hospitalized in the past 90 days? ",
            "In the past 10 years, has the Applicant been hospitalized for, been diagnosed or treated by a licensed member of the medical profession, or taken prescription medication for Angina, heart attack, stroke, heart bypass surgery, angioplasty, coronary artery stenting, or coronary artery disease?",
            "In the past 10 years, has the Applicant been hospitalized for, been diagnosed or treated by a licensed member of the medical profession, or taken prescription medication for any form of cancer to include leukemia or Hodgkin's Disease (excluding non-invasive, non-melanoma skin cancer)?",
            "In the past 10 years, has the Applicant been hospitalized for, been diagnosed or treated by a licensed member of the medical profession, or taken prescription medication for Chronic obstructive pulmonary disease (COPD), emphysema, or any other chronic respiratory disorder, excluding asthma?",
            "In the past 10 years, has the Applicant been hospitalized for, been diagnosed or treated by a licensed member of the medical profession, or taken prescription medication for Alcoholism or drug or alcohol abuse, cirrhosis, hepatitis, or any other disease of the liver?",
            "Has the Applicant under this application of coverage tested positive for exposure to the HIV infection or been diagnosed as having ARC or AIDS caused by the HIV infection or other sickness or condition derived from such infection?",
            "Has any Applicant ever applied for and been rejected for life insurance?"
        ],
        "WS-UST App R409-MO": [
            "Has any Applicant been hospitalized in the past 90 days? ",
            "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Angina, heart attack, stroke, heart bypass surgery, angioplasty, coronary artery stenting, or coronary artery disease?",
            "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for any form of cancer to include leukemia or Hodgkin's Disease (excluding non-invasive, non-melanoma skin cancer)?",
            "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Chronic obstructive pulmonary disease (COPD), emphysema, or any other chronic respiratory disorder, excluding asthma?",
            "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Alcoholism or drug or alcohol abuse, cirrhosis, hepatitis, or any other disease of the liver?",
            "Has any Applicant been diagnosed or treated by a physician, or tested positive for: Human Immunodeficiency Virus (HIV), Acquired Immune Deficiency Syndrome (AIDS), or AIDS-Related Complex (ARC)?"
        ]
}


def get_product_by_code(product_code):
    
    products_by_code = {
        "FPPTI": Product(
            "FPPTI",
            "Family Protection Plan - Terminal Illness", 
            build_FPPTI_rate_table(),
            Recommendations(FPPTI_recommendations)
        ),
        "FPPCI":Product(
            "FPPCI",
            "Family Protection Plan - Critical Illness",
            build_FPPCI_rate_table(),
            Recommendations(FPPCI_recommendations),
        ),
    }
    
    product = products_by_code.get(product_code)
    if product:
        return product
    else:
        return None


class Product(object):
    
    def __init__(self, code, name, rate_table, recommendations):
        self.code = code
        self.name = name
        self.id = None

        # Rate table data
        self.rate_table = rate_table
        self.recommendations = recommendations

    def form_for_state(self,state):
        form_dict = {
            #
            # comment out any forms not yet in Production
            #
            "FPPTI": {
                "CO": "WS-UST App R409-CO",
                #"CT": "WS-UST App R409-CT",
                #"DC": "WS-UST App R409-DC",
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
                "Generic":  "Generic"
            },
            "FPPCI": {
                "CO": "WS-UST App R409-CO",
                "IL": "WS-UST App R409-IL",
                "FL": "WS-UST App R409-FL",
                "MO": "WS-UST App R409-MO",
                "OH": "WS-UST App R409-OH",
                "VA": "WS-UST App R409-VA",
                "WI": "WS-UST App R409-WI",
                "Generic":  "Generic"
            }
        }

        if (self.code == "FPPTI" and state in FPPTI_generic_states) or (self.code == "FPPCI" and state in FPPCI_generic_states):
            form_name = "Generic"
        else:
            form_name = form_dict.get(self.code).get(state)
        
        if form_name:
            return form_name
        else:
            # print error to console, but use "generic" in lieu of failing
            print "**Failed form lookup for state", state, " on product", self.code
            return "Generic"
        
    
    def get_health_questions(self, state):
        return FPP_health_questions_by_form.get(self.form_for_state(state))
        
    def get_employee_rates(self, emp_age):
        if emp_age:
            return {
                'weekly_bypremium': self.rate_table.get_coverages_by_weekly_premium(emp_age),
                'weekly_byface': self.rate_table.get_weekly_premiums_by_coverage(emp_age),
            }
        else:
            return {}
    
    def get_spouse_rates(self, spouse_age):
        if spouse_age:
            return {
                'weekly_bypremium': self.rate_table.get_coverages_by_weekly_premium(spouse_age),
                'weekly_byface': self.rate_table.get_weekly_premiums_by_coverage(spouse_age),
            }
        else:
            return {}
        
    def get_children_rates(self, num_children):
        if num_children > 0:
            return {
                "weekly_byface": self.rate_table.get_weekly_child_premiums(),
            }
        else:
            return {}
    
    def get_weekly_premiums_by_coverage(self, age):
        return self.rate_table.get_weekly_premiums_by_coverage(age)
    
    def get_coverages_by_weekly_premium(self, age):
        return self.rate_table.get_coverages_by_weekly_premium(age)
    
    # Recommended rates - Good, Better, Best
    def get_recommended_coverages(self, employee_age, spouse_age, num_children):
        # Just uses employee_age for now
        return self.recommendations.lookup_recommended_coverages(
            employee_age=employee_age,
            spouse_age=spouse_age,
            num_children=num_children,
        )


# Utility age computation
def get_age_from_birthday(birthday_str):
    
    birth_date = parse(birthday_str)
    
    years = 0
    loop_date = datetime.today()
    
    loop_date += relativedelta(years=-1)
    while loop_date >= birth_date:
        years += 1
        loop_date += relativedelta(years=-1)
    
    return years

