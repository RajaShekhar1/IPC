import csv


def get_product_recommendations(product, **demographics):
    if product.get_base_product_code() == 'FPP-TI':
        rec = Recommendations(FPPTI_recommendations)
    elif product.get_base_product_code() == 'FPP-CI':
        rec = Recommendations(FPPCI_recommendations)
    elif product.get_base_product_code() == 'Group CI':
        if demographics['employee_smoker']:
            rec = Recommendations(GROUP_CI_smoker_recommendations)
        else:
            rec = Recommendations(GROUP_CI_nonsmoker_recommendations)
    elif product.get_base_product_code() == 'FPP-Gov':
        rec = Recommendations(FPPGOV_recommendations)
    else:
        # TODO: should this be an error? use TI for now
        rec = Recommendations(FPPTI_recommendations)
    
    return rec.lookup_recommended_coverages(
        employee_age=demographics['employee_age'],
        spouse_age=demographics.get('spouse_age'),
        num_children=demographics.get('num_children')
    )


# Recommended coverages tables
class Recommendations(object):
    def __init__(self, lookup):
        self.recommendation_lookup = lookup
    
    def lookup_recommended_coverages(self, employee_age, spouse_age,
                                     num_children):
        return self.recommendation_lookup.get(employee_age,
                                              self.get_default_recommendation())

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

FPPTI_recommendations = build_recommendation_table(
    'taa/services/products/data_files/FPPTI_suggested_rates.csv')
FPPCI_recommendations = build_recommendation_table(
    'taa/services/products/data_files/FPPCI_suggested_rates.csv')
GROUP_CI_smoker_recommendations = build_recommendation_table(
    'taa/services/products/data_files/CIEMP_smoker_suggested_rates.csv')
GROUP_CI_nonsmoker_recommendations = build_recommendation_table(
    'taa/services/products/data_files/CIEMP_NONsmoker_suggested_rates.csv')
FPPGOV_recommendations = build_recommendation_table(
    'taa/services/products/data_files/FPPGOV_suggested_rates.csv')