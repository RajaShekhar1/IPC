from datetime import datetime

from dateutil.parser import parse
from dateutil.relativedelta import relativedelta

from taa.model.RateTable import (
    build_FPPTI_rate_table,
    build_FPPCI_rate_table,
    
)
from taa.model.Recommendations import (
    FPPTI_recommendations,
    FPPCI_recommendations, 
    Recommendations,
)




def get_product_by_code(product_code):
    
    products_by_code = {
        "FPPTI": ProductData(
            "FPPTI",
            "Family Protection Plan - Terminal Illness", 
            build_FPPTI_rate_table(),
            Recommendations(FPPTI_recommendations)
        ),
        "FPPCI":ProductData(
            "FPPCI",
            "Family Protection Plan - Critical Illness",
            build_FPPCI_rate_table(),
            Recommendations(FPPCI_recommendations),
        ),
        # TODO: Get real pricing tables and recommendations
        "Group CI": ProductData(
            product_code,
            "Group Critical Illness",
            # TODO here
            build_FPPCI_rate_table(), 
            Recommendations(FPPCI_recommendations),
        ),
        "FPP-Gov": ProductData(
            product_code,
            "Family Protection Plan - Gov",
            # TODO here
            build_FPPTI_rate_table(),
            Recommendations(FPPTI_recommendations),
        ),

    }
    
    product = products_by_code.get(product_code)
    if product:
        return product
    else:
        return None


class ProductData(object):
    
    def __init__(self, code, name, rate_table, recommendations):
        self.code = code
        self.name = name
        self.id = None

        # Rate table data
        self.rate_table = rate_table
        self.recommendations = recommendations

    
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

