import csv


class Recommendations(object):
    def __init__(self, lookup):
        self.recommendation_lookup = lookup
    
    def lookup_recommended_coverages(self, employee_age, spouse_age, num_children):
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

    