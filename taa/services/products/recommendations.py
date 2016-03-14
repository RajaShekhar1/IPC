import csv
import os

__all__ = ['get_recommendations']

DATA_DIR = 'taa/services/products/data_files'
DEFAULT_RECOMMENDATIONS = [
    {'name':'good', 'coverages': {'employee': 50000, 'spouse': 50000, 'children': None}},
    {'name':'better', 'coverages': {'employee': 100000, 'spouse': 100000, 'children': 10000}},
    {'name':'best', 'coverages': {'employee': 150000, 'spouse': 150000, 'children': 20000}}
]
EMP_COLUMNS = {'good': 'emp1_cov', 'better': 'emp2_cov', 'best': 'emp3_cov'}
SPOUSE_COLUMNS = {'good': 'sp1_cov', 'better': 'sp2_cov', 'best': 'sp3_cov'}
CHILDREN_COLUMNS = {'good': 'ch1_cov', 'better': 'ch2_cov', 'best': 'ch3_cov'}

RECOMMENDATIONS = None


def get_recommendations(product, **demographics):
    init_from_data_files()

    product_code = product.get_base_product_code()

    # Compute key for recommendations lookup table
    if product_code == 'Group CI':
        key = (product_code,
               'smoker' if demographics['employee_smoker'] else 'nonsmoker')
    elif product_code not in RECOMMENDATIONS:
        # TODO: should this be an error? use TI for now
        key = 'FPPTI'
    else:
        key = product_code

    if product.are_rates_limited_to_GI():
        # We must use a computed function rather than the lookup table.
        return compute_gi_limited_recommendations(product, demographics)

    return lookup(key,
                  demographics['employee_age'],
                  demographics.get('spouse_age')
                  )



def lookup(product, employee_age, spouse_age=None):
    init_from_data_files()
    return RECOMMENDATIONS[product].get(employee_age, DEFAULT_RECOMMENDATIONS)



def compute_gi_limited_recommendations(product, demographics):
    from rates import get_rates
    rates = get_rates(product, **demographics)

    top_emp = get_top_coverage(rates.get('employee'))
    middle_emp = get_middle_coverage(rates.get('employee'))
    bottom_emp = get_bottom_coverage(rates.get('employee'))
    top_sp = get_top_coverage(rates.get('spouse'))
    middle_sp = get_middle_coverage(rates.get('spouse'))
    bottom_sp = get_bottom_coverage(rates.get('spouse'))

    top_ch = get_top_coverage(rates.get('children'))
    bottom_ch = get_bottom_coverage(rates.get('children'))

    # We need to get the top, middle, and bottom values to fill the grid of recommendations.
    return {
        'good': {'employee': middle_emp, 'spouse': bottom_sp, 'children': None},
        'better': {'employee': middle_emp, 'spouse': middle_sp, 'children': bottom_ch},
        'best': {'employee': top_emp, 'spouse': top_sp, 'children': top_ch}
    }


def get_top_coverage(rate_list):
    sorted_coverages = get_sorted_coverages(rate_list)
    if not sorted_coverages:
        return None
    # The highest will be at the end
    return sorted_coverages[-1]


def get_bottom_coverage(rate_list):
    sorted_coverages = get_sorted_coverages(rate_list)
    if not sorted_coverages:
        return None
    # The lowest coverage will be the first one
    return sorted_coverages[0]


def get_middle_coverage(rate_list):
    """
    Get the middle coverage defined as:
    the first coverage that is greater or equal to 50% of the top coverage.
    """

    top_coverage = get_top_coverage(rate_list)
    if not top_coverage:
        return None

    half_top_coverage = int(top_coverage / 2.0)

    filtered_rates = filter(lambda c: c >= half_top_coverage, get_sorted_coverages(rate_list))
    if not filtered_rates:
        return None

    return filtered_rates[0]


def get_sorted_coverages(rate_list):
    if not rate_list:
        return []

    # Only look at the coverage list
    sorted_rates = sorted(rate_list.get('byface', []), key=lambda x: x['coverage'])
    if not sorted_rates:
        return []

    return [rate['coverage'] for rate in sorted_rates]


def init_from_data_files():
    global RECOMMENDATIONS
    if RECOMMENDATIONS is None:
        RECOMMENDATIONS = {
            'FPPCI':
                build(os.path.join(DATA_DIR, 'FPPCI_suggested_rates.csv')),
            'FPP-Gov':
                build(os.path.join(DATA_DIR, 'FPPGOV_suggested_rates.csv')),
            'FPPTIB':
                build(os.path.join(DATA_DIR, 'FPPGOV_suggested_rates.csv')),
            'FPPTIY':
                build(os.path.join(DATA_DIR, 'FPPGOV_suggested_rates.csv')),
            'FPPTI':
                build(os.path.join(DATA_DIR, 'FPPTI_suggested_rates.csv')),
            ('Group CI', 'nonsmoker'):
                build(os.path.join(DATA_DIR, 'CIEMP_NONsmoker_suggested_rates.csv')),
            ('Group CI', 'smoker'):
                build(os.path.join(DATA_DIR, 'CIEMP_smoker_suggested_rates.csv'))
        }

def build(csv_path):
    table = {}
    for line in csv.DictReader(open(csv_path, 'rU')):
        age = int(line['age'])
        for rating in ['good', 'better', 'best']:
            if age not in table:
                table[age] = []

            table[age].append(
                {'name': rating,
                 'coverages': {
                    'employee': line.get(EMP_COLUMNS[rating]),
                    'spouse': line.get(SPOUSE_COLUMNS[rating]),
                    'children': line.get(CHILDREN_COLUMNS[rating])
                }
            })

    return table
