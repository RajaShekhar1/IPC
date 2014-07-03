import csv


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
        return self.weekly_premium_options
    
    def get_weekly_premium_by_coverage(self, age, coverage_amount):
        return self.weekly_by_coverage_lookup.get((age, coverage_amount))

    def get_coverage_by_weekly_premium(self, age, weekly_rate):
        return self.weekly_by_premium_lookup.get((age, weekly_rate))


def load_age_lookup_table(csv_path):
    lines = [l for l in csv.reader(open(csv_path, 'rU'))]
    
    headers = [int(x) for x in lines[0][1:]]
    data_rows = lines[1:]
    ages = [int(r[0]) for r in data_rows]

    table = {}
    for i, age in enumerate(ages):
        for j, header in enumerate(headers):
            val = lines[1:][i][j + 1]
            table[(age, int(header))] = float(val) if val.strip() != "" else None

    return table, headers


FPPTI_weekly_by_premium_lookup, all_weekly_premium_options = load_age_lookup_table("model/rates/FPPTI-bypremium.csv")
FPPTI_weekly_by_coverage_lookup, all_weekly_coverage_options = load_age_lookup_table("model/rates/FPPTI-byface.csv")
FPPCI_weekly_by_premium_lookup, all_weekly_premium_options = load_age_lookup_table("model/rates/FPPCI-bypremium.csv")
FPPCI_weekly_by_coverage_lookup, all_weekly_coverage_options = load_age_lookup_table("model/rates/FPPCI-byface.csv")
    
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

