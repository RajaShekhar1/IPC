import csv
from datetime import datetime
from dateutil.parser import parse
from dateutil.relativedelta import relativedelta


def get_product(product_type):
    products_by_type = {
        "FPPTI": FFPTerminalIllnessProduct,
    }
    
    product_class = products_by_type.get(product_type)
    if product_class:
        return product_class()
    else:
        return None


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


# Functions to parse the rate tables
def build_byface_table(csv_path):
    lines = [l for l in csv.reader(open(csv_path, 'rU'))]
    weekly_coverage_options = [int(o) for o in lines[0][1:]]
    ages = [int(r[0]) for r in lines[1:]]
    table = {}
    for i, age in enumerate(ages):
        for j, coverage in enumerate(weekly_coverage_options):
            val = lines[1:][i][j + 1]
            table[(age, int(coverage))] = float(val) if val.strip() != "" else None

    return table, weekly_coverage_options


def build_bypremium_table(csv_path):
    lines = [l for l in csv.reader(open(csv_path, 'rU'))]
    weekly_premium_options = [int(o) for o in lines[0][1:]]
    ages = [int(r[0]) for r in lines[1:]]
    table = {}
    for i, age in enumerate(ages):
        for j, premium in enumerate(weekly_premium_options):
            val = lines[1:][i][j + 1]
            table[(age, int(premium))] = int(val) if val.strip() != "" else None

    return table, weekly_premium_options

def build_recommendation_table(csv_path):
    lines = [l for l in csv.DictReader(open(csv_path, 'rU'))]
    
    EMP_COLUMNS = dict(good='emp1_cov', better='emp2_cov', best='emp3_cov')
    SPOUSE_COLUMNS = dict(good='sp1_cov', better='sp2_cov', best='sp3_cov')
    CHILDREN_COLUMNS = dict(good='ch1_cov', better='ch2_cov', best='ch3_cov')
    
    table = {}
    for line in lines:
        age = int(line['age'])
        table[age] = {
            'good': {
                'employee': line.get(EMP_COLUMNS['good']),
                'spouse': line.get(SPOUSE_COLUMNS['good']),
                'children': line.get(CHILDREN_COLUMNS['good']),
            },
            'better': {
                'employee': line.get(EMP_COLUMNS['better']),
                'spouse': line.get(SPOUSE_COLUMNS['better']),
                'children': line.get(CHILDREN_COLUMNS['better']),
            },
            'best': {
                'employee': line.get(EMP_COLUMNS['best']),
                'spouse': line.get(SPOUSE_COLUMNS['best']),
                'children': line.get(CHILDREN_COLUMNS['best']),
            },
        }
        
    return table


class FFPTerminalIllnessProduct(object):
    
    def get_employee_rates(self, emp_age):
        if emp_age:
            return {
                'weekly_bypremium': self.get_coverages_by_weekly_premium(emp_age),
                'weekly_byface': self.get_weekly_premiums_by_coverage(emp_age),
            }
        else:
            return {}
    
    def get_spouse_rates(self, spouse_age):
        if spouse_age:
            spouse_rates = {
                'weekly_bypremium': self.get_coverages_by_weekly_premium(spouse_age),
                'weekly_byface': self.get_weekly_premiums_by_coverage(spouse_age),
            }
        else:
            return {}
        
    def get_children_rates(self, num_children):
        if num_children > 0:
            return {
                "weekly_byface": self.get_weekly_child_premiums(),
            }
        else:
            return {}
    
    # Recommended rates - Good, Better, Best
    def get_recommended_coverages(self, employee_age, spouse_age, num_children):
        # Just uses employee_age for now
        return self.recommendation_lookup.get(employee_age, self.get_default_recommendation())
        
    def get_default_recommendation(self):
        # In case the lookup table is incomplete
        return {
            'good': {
                'employee': 50000,
                'spouse': 50000,
                'children': None,
            },
            'better': {
                'employee': 100000,
                'spouse': 100000,
                'children': 10000,
            },
            'best': {
                'employee': 150000,
                'spouse': 150000,
                'children': 20000,
            },
        }
    
    def get_weekly_premiums_by_coverage(self, age):
        premiums = []
        for coverage in self.weekly_coverage_options:
            premium = self.get_weekly_premium_by_coverage(age, coverage)
            if premium:
                premiums.append(dict(coverage=coverage, premium=premium))
        return premiums

    def get_coverages_by_weekly_premium(self, age):
        coverages = []
        for weekly_premium in self.weekly_premium_options:
            coverage = self.get_coverage_by_weekly_premium(age, weekly_premium)
            if coverage:
                coverages.append(dict(premium=weekly_premium, coverage=coverage))

        return coverages
    
    def get_weekly_child_premiums(self):
        return [
            {'premium': 1.15, 'coverage': 10000},
            {'premium': 2.30, 'coverage': 20000},
        ]

    def get_coverage_by_weekly_premium(self, age, weekly_rate):
        return self.weekly_by_premium_lookup.get((age, weekly_rate))

    def get_weekly_premium_by_coverage(self, age, coverage_amount):
        return self.weekly_by_coverage_lookup.get((age, coverage_amount))

    # class-level lookup tables
    weekly_by_premium_lookup, weekly_premium_options = build_bypremium_table("model/rates/FPPTI-bypremium.csv")
    weekly_by_coverage_lookup, weekly_coverage_options = build_byface_table("model/rates/FPPTI-byface.csv")

    recommendation_lookup = build_recommendation_table("model/rates/FPPTI_suggested_rates.csv")




