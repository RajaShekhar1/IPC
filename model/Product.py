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
    
    def get_health_questions(self):
        
        return [
            "Has any Applicant been hospitalized in the past 90 days?",
            "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Angina, heart attack, stroke, heart bypass surgery, angioplasty, coronary artery stenting, or coronary artery disease?",
            "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for any form of cancer to include leukemia or Hodgkin's Disease (excluding non-invasive, non-melanoma skin cancer)?",
            "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Chronic obstructive pulmonary disease (COPD), emphysema, or any other chronic respiratory disorder, excluding asthma?",
            "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Alcoholism or drug or alcohol abuse, cirrhosis, hepatitis, or any other disease of the liver?",
            "Has any Applicant been diagnosed or treated by a physician, or tested positive for: Human Immunodeficiency Virus (HIV), Acquired Immune Deficiency Syndrome (AIDS), or AIDS-Related Complex (ARC)?",
            "Has any Applicant ever applied for and been rejected for life insurance?",
        ]
        
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

