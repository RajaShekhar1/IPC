

# Some Utility classes, can move these later
class Gender(object):
    def __init__(self, gender):
        self._gender = gender
        
    def __eq__(self, other):
        return self._gender == other._gender
    
MALE = Gender('m')
FEMALE = Gender('f')

class AgeBand(object):
    def __init__(self, lower, upper):
        """Inclusive"""
        self._lower = lower 
        self._upper = upper
    
    def __eq__(self, other):
        return self._lower == other._lower and self._upper == other._upper 
    
    def __lt__(self, other):
        return self._upper < other._lower
    
    def __gt__(self, other):
        return self._lower < other._lower
    
# Recommended rates - Good, Better, Best
def get_recommended_rates(gender, age_band, marital_status, include_spouse, num_children):
    
    pass

def lookup_weekly_rates(age):
    """
    Returns a list of (weekly-rate, coverage_amount) tuples for a given age
    """

def lookup_weekly_coverages(age):
    """
    Returns a list of (coverage_amount, weekly_rate) tuples for a given age
    """
    
    