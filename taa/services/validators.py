from taa.services import LookupService
import re
from datetime import datetime

from taa.services.products.payment_modes import is_payment_mode

class RequiredIfAnyInGroupValidator(object):
    def __init__(self, group_fields, message=None):
        self.group_fields = group_fields
        self.message = message

    def __call__(self, field, record):
        # If any of the given fields have a value, require this field
        if any(group_field.get_column_from_record(record)
               for group_field in self.group_fields):
            return required_validator(field, record, self.message)
        return True, None, None

def required_validator( field, record, message=None):
    data = field.get_column_from_record(record)
    if not data:
        message = message if message else "Required Data Missing"
        return False, "missing_data", message
    return True, None, None

def ssn_validator( field, record):
    ssn_pattern = re.compile('^\d{9}$')
    ssn = field.get_column_from_record(record)
    if not ssn:
        # Allow blank unless combined with required validator
        return True, None, None
    elif not ssn_pattern.match(ssn):
        return False, "invalid_ssn", "Invalid SSN"
    return True, None, None

def payment_mode_validator( field, record):
    payment_mode = field.get_column_from_record(record)
    if not payment_mode:
        # If product is not set and it is not required, we can return true
        return True, None, None
    if not is_payment_mode(name=payment_mode):
        return False, "invalid_mode", "Invalid payment mode"
    return True, None, None

def product_validator( field, record):
    product_service = LookupService("ProductService")
    # Needs a database call to check if product exists
    product_code = field.get_column_from_record(record)
    if not product_service.is_valid_product_code(product_code):
        return False, "invalid_product", "Product code not found"
    return True, None, None

def api_token_validator( field, record):
    api_token_service = LookupService("ApiTokenService")
    api_token = field.get_column_from_record(record)
    if not api_token_service.is_valid_token(api_token):
        return False, "invalid_token", "Invalid API token provided"
    return True, None, None

def case_token_validator( field, record):
    case_service = LookupService("CaseService")
    case_token = field.get_column_from_record(record)
    if not case_service.is_valid_case_token(case_token):
        return False, "invalid_token", "Invalid Case token provided"
    return True, None, None


def gender_validator( field, record):
    gender = field.get_column_from_record(record)
    if not gender:
        # Allow blank unless combined with required validator
        return True, None, None
    if gender not in ['m', 'f', '', ' ', None]:
        return False, "invalid_gender", "Gender must be 'Male' or 'Female'"
    return True, None, None

def birthdate_validator( field, record):
    date = field.get_column_from_record(record)
    if not date:
        # Allow blank unless combined with required validator
        return True, None, None
    else:
        pass
    if not isinstance(date, datetime):
        return False, "invalid_date", "Invalid date"
    if date > datetime.now():
        # The preprocessor currently keeps this from happening, but I will leave
        #  it in here in case that changes
        return False, "invalid_date", "Future date is not allowed for a birthday"
    return True, None, None

def email_validator( field, record):
    email = field.get_column_from_record(record)
    if not email:
        # Allow blank unless combined with required validator
        return True, None, None
    if '@' not in email and len(email) < 3:
        return False, "invalid_email", 'Invalid email'
    return True, None, None

def coverage_validator( field, record):
    coverage_pattern = re.compile('^[0-9]+$')
    coverage = field.get_column_from_record(record)
    if not coverage:
        return True, None, None
    if not coverage_pattern.match(coverage):
        return False, "invalid_coverage", "Invalid coverage format"
    return True, None, None

def premium_validator( field, record):
    premium_pattern = re.compile('^[0-9]+\.[0-9][0-9]$')
    premium = field.get_column_from_record(record)
    if not premium:
        return True, None, None
    if not premium_pattern.match(premium):
        return False, "invalid_premium", "Invalid premium format"
    return True, None, None

def zip_validator( field, record):
    zip_pattern = re.compile('^\d{5,}$')
    zip = field.get_column_from_record(record)
    if not zip:
        # Allow blank unless combined with required validator
        return True, None, None
    if not zip_pattern.match(zip):
        return False, "invalid_zip", "Invalid ZIP code"
    return True, None, None

def state_validator( field, record):
    state = field.get_column_from_record(record)
    if not state:
        return True, None, None

    from taa.services.products import ProductService
    ps = ProductService()
    if not state or not len(state) == 2 or not state.upper() in ps.get_all_statecodes():
        return False, "invalid_state", "Invalid US State. Must be two-letter abbreviation, got '{}'.".format(state)
    return True, None, None

def question_answered_validator( field, record):
        answer = field.get_column_from_record(record)
        if not answer:
            return True, None, None
        if answer.upper() not in ["Y", "N"]:
            return False, "invalid_question", "Questions must be answered with Y or N"
        return True, None, None
