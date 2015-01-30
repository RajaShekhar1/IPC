import csv


def get_product_rates_lookup(product):
    
    tables_by_code = {
        "FPPTI": build_FPPTI_rate_table(),
        "FPPCI": build_FPPCI_rate_table(),
        
        # TODO: Get real pricing tables and recommendations
        "FPP-Gov": build_FPPTI_rate_table(),
    }
    
    if product.get_base_product_code() != "Group CI":
        return ProductRates(product, tables_by_code[product.get_base_product_code()])
    else: 
        return GroupCIRates(product, 
                            smoker_rates=build_GroupCI_smoking_rate_table(), 
                            nonsmoker_rates=build_GroupCI_nonsmoking_rate_table())

class ProductRates(object):
    def __init__(self, product, rate_table):
        self.product = product
        self.rate_table = rate_table
        
    def get_all_rates(self, **demographics):
        rates = dict(
            employee={},
            spouse={},
            children={},
        )
        
        if demographics.get('employee_age'):
            age = demographics.get('employee_age')
            rates['employee'] = {
                'weekly_bypremium': self.rate_table.get_coverages_by_weekly_premium(age),
                'weekly_byface': self.rate_table.get_weekly_premiums_by_coverage(age),
            }
        
        if demographics.get('spouse_age'):
            age = demographics.get('spouse_age')
            rates['spouse'] = {
                'weekly_bypremium': self.rate_table.get_coverages_by_weekly_premium(age),
                'weekly_byface': self.rate_table.get_weekly_premiums_by_coverage(age),
            }

        if demographics.get('num_children') > 0:
            rates['children'] = {
                "weekly_byface": self.rate_table.get_weekly_child_premiums(),
            }
        
        return rates


class GroupCIRates(object):
    def __init__(self, product, smoker_rates, nonsmoker_rates):
        self.product = product
        self.smoker_rates = smoker_rates
        self.nonsmoker_rates = nonsmoker_rates
        
    def get_all_rates(self, **demographics):
        rates = dict(
            employee={},
            spouse={},
            children={},
        )
        
        emp_rates_table = self.smoker_rates if demographics['employee_smoker'] == 'true' else self.nonsmoker_rates
        rates['employee']['weekly_byface'] = emp_rates_table.get_premiums_by_coverage_for_age(demographics['employee_age'])
        #rates['employee']['weekly_bycoverage'] = []
        
        if demographics.get('spouse_age'):
            sp_rates_table = self.smoker_rates if demographics['spouse_smoker'] == 'true' else self.nonsmoker_rates
            rates['spouse']['weekly_byface'] = sp_rates_table.get_premiums_by_coverage_for_age(demographics['spouse_age'])
            #rates['spouse']['weekly_bycoverage'] = []
        
        # TODO: Check these
        rates['children'] = {
            'weekly_byface': [
                {'premium': 1.15, 'coverage': 10000},
                {'premium': 2.30, 'coverage': 20000},
            ]
        }
        
        return rates
    
class CoverageByAgePremiumLookup(object):
    def __init__(self, all_coverage_options, lookup_table):
        self.all_coverage_options = all_coverage_options
        self.lookup_table = lookup_table
    
    def get_premiums_by_coverage_for_age(self, age):
        premiums = []
        for coverage in self.all_coverage_options:
            premium = self.get_premium_by_age_and_coverage(age, coverage)
            if premium:
                premiums.append(dict(coverage=coverage, premium=premium))
        return premiums

    def get_premium_by_age_and_coverage(self, age, coverage_amount):
        return self.lookup_table.get((age, coverage_amount))
    
class RateTable(object):
    def __init__(self, all_weekly_premium_options, all_weekly_coverage_options,
                 weekly_by_premium_lookup, weekly_by_coverage_lookup,
                 child_premiums):
        
        self.weekly_premium_options = all_weekly_premium_options
        self.weekly_by_premium_lookup = weekly_by_premium_lookup
        self.weekly_coverage_options = all_weekly_coverage_options
        self.weekly_by_coverage_lookup = weekly_by_coverage_lookup
        self.child_premiums = child_premiums
        
    def get_weekly_premiums_by_coverage(self, age):
        premiums = []
        for coverage in self.weekly_coverage_options:
            premium = self.get_weekly_premium_by_coverage(age, coverage)
            if premium:
                premiums.append(dict(coverage=coverage, premium=premium))
        return premiums
    
    def get_coverages_by_weekly_premium(self, age):
        coverages = []
        for weekly_premium in self.get_all_weekly_premium_options():
            coverage = self.get_coverage_by_weekly_premium(age, weekly_premium)
            if coverage:
                coverages.append(dict(premium=weekly_premium, coverage=coverage))
        
        return coverages
    
    def get_weekly_child_premiums(self):
        return self.child_premiums
        
    def get_all_weekly_premium_options(self):
        return self.weekly_premium_options
    
    def get_all_weekly_coverage_options(self):
        return self.weekly_coverage_options
    
    def get_weekly_premium_by_coverage(self, age, coverage_amount):
        return self.weekly_by_coverage_lookup.get((age, coverage_amount))

    def get_coverage_by_weekly_premium(self, age, weekly_rate):
        return self.weekly_by_premium_lookup.get((age, weekly_rate))


def load_age_lookup_table(csv_path):
    lines = [l for l in csv.reader(open(csv_path, 'rU'))]
    
    headers = [int(x.replace(',', '')) for x in lines[0][1:]]
    data_rows = lines[1:]
    ages = [int(r[0]) for r in data_rows]

    table = {}
    for i, age in enumerate(ages):
        for j, header in enumerate(headers):
            val = lines[1:][i][j + 1]
            table[(age, int(header))] = float(val) if val.strip() != "" else None

    return table, headers


FPPTI_weekly_by_premium_lookup, all_weekly_premium_options = load_age_lookup_table("taa/services/products/data_files/FPPTI-bypremium.csv")
FPPTI_weekly_by_coverage_lookup, all_weekly_coverage_options = load_age_lookup_table("taa/services/products/data_files/FPPTI-byface.csv")
FPPCI_weekly_by_premium_lookup, FPPCI_all_weekly_premium_options = load_age_lookup_table("taa/services/products/data_files/FPPCI-bypremium.csv")
FPPCI_weekly_by_coverage_lookup, FPPCI_all_weekly_coverage_options = load_age_lookup_table("taa/services/products/data_files/FPPCI-byface.csv")

GROUP_CI_non_smoking_weekly_by_coverage_lookup, GROUP_CI_non_smoking_all_weekly_coverage_options = load_age_lookup_table(
    "taa/services/products/data_files/CIEMP-rates---NonSmoking-Weekly.csv")
GROUP_CI_smoking_weekly_by_coverage_lookup, GROUP_CI_smoking_all_weekly_coverage_options = load_age_lookup_table(
    "taa/services/products/data_files/CIEMP-rates---Smoking-Weekly.csv")


def build_FPPTI_rate_table():
    
    child_premiums = [
        {'premium': 1.15, 'coverage': 10000},
        {'premium': 2.30, 'coverage': 20000},
    ]
    
    return RateTable(
        all_weekly_premium_options=all_weekly_premium_options,
        all_weekly_coverage_options=all_weekly_coverage_options,
        weekly_by_premium_lookup=FPPTI_weekly_by_premium_lookup,
        weekly_by_coverage_lookup=FPPTI_weekly_by_coverage_lookup,
        child_premiums = child_premiums,
    )

def build_FPPCI_rate_table():
    
    child_premiums = [
        {'premium': 1.15, 'coverage': 10000},
        {'premium': 2.30, 'coverage': 20000},
    ]
    
    return RateTable(
        all_weekly_premium_options=all_weekly_premium_options,
        all_weekly_coverage_options=all_weekly_coverage_options,
        weekly_by_premium_lookup=FPPCI_weekly_by_premium_lookup,
        weekly_by_coverage_lookup=FPPCI_weekly_by_coverage_lookup,
        child_premiums = child_premiums,
    )

def build_GroupCI_smoking_rate_table():
    # TODO: Check these 
    child_premiums = [
        {'premium': 1.15, 'coverage': 10000},
        {'premium': 2.30, 'coverage': 20000},
    ]
    
    return CoverageByAgePremiumLookup(GROUP_CI_smoking_all_weekly_coverage_options, GROUP_CI_smoking_weekly_by_coverage_lookup)

def build_GroupCI_nonsmoking_rate_table():
    return CoverageByAgePremiumLookup(GROUP_CI_non_smoking_all_weekly_coverage_options,
                                      GROUP_CI_non_smoking_weekly_by_coverage_lookup)
