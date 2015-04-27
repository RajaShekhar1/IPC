import csv
import os

__all__ = ['get_recommendations']

DATA_DIR = 'taa/services/products/data_files'
DEFAULT_RECOMMENDATIONS = {
    'good': {'employee': 50000, 'spouse': 50000, 'children': None},
    'better': {'employee': 100000, 'spouse': 100000, 'children': 10000},
    'best': {'employee': 150000, 'spouse': 150000, 'children': 20000}
}
EMP_COLUMNS = {'good': 'emp1_cov', 'better': 'emp2_cov', 'best': 'emp3_cov'}
SPOUSE_COLUMNS = {'good': 'sp1_cov', 'better': 'sp2_cov', 'best': 'sp3_cov'}
CHILDREN_COLUMNS = {'good': 'ch1_cov', 'better': 'ch2_cov', 'best': 'ch3_cov'}


def get_recommendations(product, **demographics):
    product_code = product.get_base_product_code()
    # Compute key for recommendations lookup table
    if product_code == 'Group CI':
        key = (product_code,
               'smoker' if demographics['is_smoker'] else 'nonsmoker')
    elif product_code not in RECOMMENDATIONS:
        # TODO: should this be an error? use TI for now
        key = 'FPPTI'
    else:
        key = product_code
    return lookup(key,
                  demographics['employee_age'],
                  demographics.get('spouse_age'),
                  demographics.get('num_children'))


def lookup(product, employee_age, spouse_age=None, num_children=None):
    return RECOMMENDATIONS[product].get(employee_age, DEFAULT_RECOMMENDATIONS)


def build(csv_path):
    table = {}
    for line in csv.DictReader(open(csv_path, 'rU')):
        age = int(line['age'])
        for rating in ['good', 'better', 'best']:
            if age not in table:
                table[age] = {}
            table[age][rating] = {
                'employee': line.get(EMP_COLUMNS[rating]),
                'spouse': line.get(SPOUSE_COLUMNS[rating]),
                'children': line.get(CHILDREN_COLUMNS[rating])
            }
    return table


RECOMMENDATIONS = {
    'FPPCI':
        build(os.path.join(DATA_DIR, 'FPPCI_suggested_rates.csv')),
    'FPP-Gov':
        build(os.path.join(DATA_DIR, 'FPPGOV_suggested_rates.csv')),
    'FPPTI':
        build(os.path.join(DATA_DIR, 'FPPTI_suggested_rates.csv')),
    ('Group CI', 'nonsmoker'):
        build(os.path.join(DATA_DIR, 'CIEMP_NONsmoker_suggested_rates.csv')),
    ('Group CI', 'smoker'):
        build(os.path.join(DATA_DIR, 'CIEMP_smoker_suggested_rates.csv'))
}