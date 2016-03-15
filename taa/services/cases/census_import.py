import dateutil.parser
from datetime import datetime
import re
import csv
import StringIO
from taa.services import RequiredFeature


class CensusRecordField(object):
    """
    Defines a column for the uploaded CSV census data
    """
    def __init__(self, csv_column_name, database_name, preprocessor, validators,
                 post_processors=None):
        self.csv_column_name = csv_column_name
        self.database_name = database_name
        self.preprocessor = preprocessor or (lambda x: x)
        self.validators = validators or []
        self.post_processors = post_processors or []

    def validate(self, parser, record):
        all_valid = True
        for validator in self.validators:
            is_valid, error_message = validator(self, record)
            if not is_valid:
                parser.error_record_field(error_message,
                                          self.csv_column_name,
                                          parser.get_line_number(),
                                          record)
                all_valid = False
        return all_valid

    def get_column_from_record(self, record):
        return record.get(self.csv_column_name, u'')

    def preprocess(self, data, record):
        return self.preprocessor(data, record)

    def postprocess(self, field_data, all_data):
        new_data = field_data
        for postprocessor in self.post_processors:
            new_data = postprocessor(self, new_data, all_data)
        return new_data

    def add_validator(self, validator):
        self.validators.append(validator)

##
# Validators
##

def required_validator(field, record, message=None):
    data = field.get_column_from_record(record)
    if not data:
        message = message if message else "Required Data Missing"
        return False, message
    return True, None


ssn_pattern = re.compile('^\d{9}$')


def ssn_validator(field, record):
    ssn = field.get_column_from_record(record)
    if not ssn:
        # Allow blank unless combined with required validator
        return True, None
    elif not ssn_pattern.match(ssn):
        return False, "Invalid SSN"
    return True, None


def gender_validator(field, record):
    gender = field.get_column_from_record(record)
    if not gender:
        # Allow blank unless combined with required validator
        return True, None,
    if gender not in ['male', 'female', 'm', 'f']:
        return False, "Gender must be 'Male' or 'Female'"
    return True, None


def birthdate_validator(field, record):
    date = field.get_column_from_record(record)
    if not date:
        # Allow blank unless combined with required validator
        return True, None
    else:
        pass
    if not isinstance(date, datetime):
        return False, "Invalid date"
    if date > datetime.now():
        # The preprocessor currently keeps this from happening, but I will leave
        #  it in here in case that changes
        return False, "Future date is not allowed for a birthday"
    return True, None


def email_validator(field, record):
    email = field.get_column_from_record(record)
    if not email:
        # Allow blank unless combined with required validator
        return True, None
    if '@' not in email and len(email) < 3:
        return False, 'Invalid email'
    return True, None


zip_pattern = re.compile('^\d{3,5}$')


def zip_validator(field, record):
    zip = field.get_column_from_record(record)
    if not zip:
        # Allow blank unless combined with required validator
        return True, None
    if not zip_pattern.match(zip):
        return False, "Invalid ZIP code"
    return True, None


def state_validator(field, record):
    state = field.get_column_from_record(record)
    if not state:
        return True, None

    from taa.services.products import ProductService
    ps = ProductService()
    if not state or not len(state) == 2 or not state in ps.get_all_statecodes():
        return False, "Invalid US State. Must be two-letter abbreviation."

    return True, None


class RequiredIfAnyInGroupValidator(object):
    def __init__(self, group_fields, message=None):
        self.group_fields = group_fields
        self.message = message

    def __call__(self, field, record):
        # If any of the given fields have a value, require this field
        if any(group_field.get_column_from_record(record)
               for group_field in self.group_fields):
            return required_validator(field, record, self.message)
        return True, None

##
# Data preprocessors
##

def preprocess_string(data, record=None):
    if data is None:
        return u''
    return unicode(data, 'utf-8').strip()


def preprocess_product_code(data, record=None):
    val = preprocess_string(data, record)

    # Convert CIEMP to Group CI, the internal code for this product.
    if val == 'CIEMP':
        return "Group CI"

    return val

def postprocess_spouse_last(field, data, record):
    "Automatically populate a spouse or child last name if blank"
    employee_last = preprocess_string(
        CensusRecordParser.employee_last.get_column_from_record(record))
    # Use one of the required spouse fields
    spouse_birthdate = record.get(
        CensusRecordParser.spouse_birthdate.csv_column_name)
    if not data and employee_last and spouse_birthdate:
        return employee_last
    return data


def postprocess_children_last(field, data, record):
    """Automatically populate child last names if blank"""
    employee_last = preprocess_string(
        CensusRecordParser.employee_last.get_column_from_record(record))
    p = re.compile('CH(\d)_LAST')
    match = p.match(field.csv_column_name)
    if match:
        child_num = match.groups()[0]
        csv_first_col_name = 'CH{}_FIRST'.format(child_num)
        csv_birthdate_col_name = 'CH{}_BIRTHDATE'.format(child_num)
        first = record.get(csv_first_col_name)
        birthdate = record.get(csv_birthdate_col_name)

        # Use one of the required spouse fields
        spouse_birthdate = record.get(
            CensusRecordParser.spouse_birthdate.csv_column_name)
        if (not data) and employee_last and (first or birthdate):
            return employee_last
    return data


def preprocess_date(data, record):
    if data is None or data == '':
        return None
    try:
        d = dateutil.parser.parse(data)
        if d >= datetime.today():
            # This can happen when you try to parse 2-digit years (excel issue?)
            # Solution should be OK, but if someone puts a future date in
            # (like for an expected child?) it doesn't work, and also won't
            # work for 100+ year-old people. Which can't apply for life
            # insurance, I think.
            d = datetime(d.year - 100, d.month, d.day)
    except ValueError:
        # Can't be parsed as a date; return as-is and let validation
        # handle the error
        return data
    return d


def preprocess_zip(data, record):
    if data is None:
        return u''
    # Just want first five characters
    return unicode(data, 'utf-8').strip().replace('-', '')[:5]


def preprocess_gender(data, record):
    data = data.lower()
    if data == 'f':
        return 'female'
    elif data == 'm':
        return 'male'
    return data


def preprocess_numbers(data, record):
    if data is None:
        return ''
    return ''.join(c for c in unicode(data, 'utf-8') if c.isdigit())


def preprocess_y_n(data, record):
    if data is None or data == '':
        return ''
    if str(data).lower() in ['y', 'true', 'yes']:
        return 'Y'
    else:
        return 'N'

class CensusRecordParser(object):

    file_import_service = RequiredFeature('FileImportService')

    # Construct the fields and wire up the correct validation
    # Employee
    employee_first = CensusRecordField('EMP_FIRST', 'employee_first', preprocess_string, [required_validator])
    employee_last = CensusRecordField('EMP_LAST', 'employee_last', preprocess_string, [required_validator])
    employee_ssn = CensusRecordField('EMP_SSN', 'employee_ssn', preprocess_numbers, [required_validator, ssn_validator])
    employee_gender = CensusRecordField('EMP_GENDER', 'employee_gender', preprocess_gender, [gender_validator])
    employee_birthdate = CensusRecordField('EMP_BIRTHDATE', 'employee_birthdate', preprocess_date, [required_validator, birthdate_validator])
    employee_email = CensusRecordField('EMP_EMAIL', 'employee_email', preprocess_string, [email_validator])
    employee_phone = CensusRecordField('EMP_PHONE', 'employee_phone', preprocess_string, [])
    employee_address1 = CensusRecordField('EMP_ADDRESS1', 'employee_street_address', preprocess_string, [])
    employee_address2 = CensusRecordField('EMP_ADDRESS2', 'employee_street_address2', preprocess_string, [])
    employee_city = CensusRecordField('EMP_CITY', 'employee_city', preprocess_string, [])
    employee_state = CensusRecordField('EMP_STATE', 'employee_state', preprocess_string, [state_validator])
    employee_zip = CensusRecordField('EMP_ZIP', 'employee_zip', preprocess_zip, [zip_validator])
    employee_height_inches = CensusRecordField('EMP_HEIGHT_IN', 'employee_height_inches', preprocess_string, [])
    employee_weight_lbs = CensusRecordField('EMP_WEIGHT_LBS', 'employee_weight_lbs', preprocess_string, [])
    employee_smoker = CensusRecordField('EMP_SMOKER_Y_N', 'employee_smoker', preprocess_y_n, [])
    employee_occupation_class = CensusRecordField('EMP_OCCUPATION', 'occupation_class', preprocess_string, [])
    # Spouse
    spouse_first = CensusRecordField('SP_FIRST', 'spouse_first', preprocess_string, [])
    spouse_last = CensusRecordField('SP_LAST', 'spouse_last', preprocess_string, [], [postprocess_spouse_last])
    spouse_ssn = CensusRecordField('SP_SSN', 'spouse_ssn', preprocess_numbers, [ssn_validator])
    spouse_gender = CensusRecordField('SP_GENDER', 'spouse_gender', preprocess_gender, [gender_validator])
    spouse_birthdate = CensusRecordField('SP_BIRTHDATE', 'spouse_birthdate', preprocess_date, [birthdate_validator])
    spouse_email = CensusRecordField('SP_EMAIL', 'spouse_email', preprocess_string, [email_validator])
    spouse_phone = CensusRecordField('SP_PHONE', 'spouse_phone', preprocess_string, [])
    spouse_address1 = CensusRecordField('SP_ADDRESS1', 'spouse_street_address', preprocess_string, [])
    spouse_address2 = CensusRecordField('SP_ADDRESS2', 'spouse_street_address2', preprocess_string, [])
    spouse_city = CensusRecordField('SP_CITY', 'spouse_city', preprocess_string, [])
    spouse_state = CensusRecordField('SP_STATE', 'spouse_state', preprocess_string, [state_validator])
    spouse_zip = CensusRecordField('SP_ZIP', 'spouse_zip', preprocess_zip, [zip_validator])
    spouse_height_inches = CensusRecordField('SP_HEIGHT_IN', 'spouse_height_inches', preprocess_string, [])
    spouse_weight_lbs = CensusRecordField('SP_WEIGHT_LBS', 'spouse_weight_lbs', preprocess_string, [])
    spouse_smoker = CensusRecordField('SP_SMOKER_Y_N', 'spouse_smoker', preprocess_y_n, [])
    # Add group validation requirement. If any field in the group is given,
    # all must be present
    spouse_fields = [spouse_first, spouse_birthdate]
    for field in spouse_fields:
        validator = RequiredIfAnyInGroupValidator(
            spouse_fields,
            message="{} is required if any of the following are"
                    "provided: {}".format(field.csv_column_name,
                                          ', '.join([f.csv_column_name
                                                     for f in spouse_fields
                                                     if f is not field])
                                          ))
        # If any in group provided, all must be valid
        field.add_validator(validator)
        # Also require this field if the SSN was provided
        field.add_validator(RequiredIfAnyInGroupValidator(
            [spouse_ssn],
            message="{} is required if {} is provided".format(
                field.csv_column_name, spouse_ssn.csv_column_name)
        ))
    all_possible_fields = [
        employee_first,
        employee_last,
        employee_ssn,
        employee_gender,
        employee_birthdate,
        employee_email,
        employee_phone,
        employee_address1,
        employee_address2,
        employee_city,
        employee_state,
        employee_zip,
        employee_height_inches,
        employee_weight_lbs,
        employee_smoker,
        employee_occupation_class,
        spouse_first,
        spouse_last,
        spouse_ssn,
        spouse_birthdate,
        spouse_gender,
        spouse_email,
        spouse_phone,
        spouse_address1,
        spouse_address2,
        spouse_city,
        spouse_state,
        spouse_zip,
        spouse_height_inches,
        spouse_weight_lbs,
        spouse_smoker,
    ]
    MAX_CHILDREN = 6
    for num in range(1, MAX_CHILDREN + 1):
        child_first = CensusRecordField('CH{}_FIRST'.format(num),
                                        'child{}_first'.format(num),
                                        preprocess_string, [])
        child_last = CensusRecordField('CH{}_LAST'.format(num),
                                       'child{}_last'.format(num),
                                       preprocess_string, [],
                                       [postprocess_children_last])
        child_birthdate = CensusRecordField('CH{}_BIRTHDATE'.format(num),
                                            'child{}_birthdate'.format(num),
                                            preprocess_date,
                                            [birthdate_validator])
        # Require child_first if child birthdate given
        child_first.add_validator(
            RequiredIfAnyInGroupValidator([child_birthdate]))
        all_possible_fields += [child_first, child_last, child_birthdate]

    def __init__(self):
        self.errors = []
        self.valid_data = []
        self.used_ssns = set()
        self.line_number = 0

    def process_file(self, file_data, error_if_matching=None):
        headers, records, dialect = self._process_file_stream(file_data)
        self.validate_header_row(headers, records)
        # Don't do any more processing if missing important headers
        if self.errors:
            return
        preprocessed_records = (self.preprocess_record(record)
                                for record in records)
        # Reset internal counters
        self.line_number = 0
        self.valid_data = []
        self.used_ssns = set()
        self.error_if_matching = error_if_matching or {}
        for record in preprocessed_records:
            self.line_number += 1
            if self.validate_record(headers, record):
                # Run any post-processing
                self.postprocess_record(record)
                self.valid_data.append(record)

    def validate_record(self, headers, record):
        is_valid = True
        for field in self.all_possible_fields:
            is_valid &= field.validate(self, record)
        # Do not allow duplicate employee SSNs in a single upload
        ssn = record.get(self.employee_ssn.csv_column_name)
        if ssn and ssn in self.used_ssns:
            is_valid = False
            self.error_record_field(
                "Duplicate SSN in census file",
                self.employee_ssn.csv_column_name,
                self.line_number,
                record
            )
        elif ssn:
            self.used_ssns.add(ssn)
        # Some modes require us to throw an error if an existing record exists
        # in the database (matched on SSN). Check that here.
        if self.error_if_matching and ssn in self.error_if_matching:
            self.error_record_field(
                "A census record exists that matches this SSN. This is not "
                "allowed when uploading in 'Add New Records' mode.",
                self.employee_ssn.csv_column_name,
                self.line_number,
                record
            )
        return is_valid

    def get_line_number(self):
        return self.line_number

    def get_valid_data(self):
        if self.errors:
            # Right now, it is all or nothing, so return empty list if any
            # errors occurred
            return []
        return self.valid_data

    def validate_header_row(self, headers, records):
        if len(records) == 0:
            self.error_message(
                "The uploaded CSV file did not appear to have a valid header "
                "row. Please see the sample data file for formatting examples."
            )
        missing_headers = self._get_missing_headers(headers)
        if missing_headers:
            missing_msg = ', '.join(missing_headers)
            self.error_message("The following required columns are missing "
                               "from the uploaded file: {}".format(missing_msg))

    def get_error_headers(self, field_name):
        headers = ['EMP_FIRST', 'EMP_LAST']
        if field_name not in headers:
            headers.append(field_name)
        return headers

    def _get_missing_headers(self, headers):
        required_headers = [
            'EMP_SSN',
            'EMP_FIRST',
            'EMP_LAST',
            'EMP_GENDER',
            'EMP_BIRTHDATE',
            'EMP_EMAIL',
        ]
        return {h for h in required_headers if h not in headers}

    def error_message(self, message):
        self.errors.append(dict(
            message=message,
            records=[],
        ))

    def error_record_field(self, message, field_name, line_number, data):
        self.errors.append(dict(
            message=message,
            records=[data],
            line_number=line_number,
            headers=self.get_error_headers(field_name),
            field_name=field_name,
        ))

    def preprocess_record(self, record):
        data = {}
        for column in record:
            field = self.get_field_from_csv_column(column)
            if not field:
                continue
            data[column] = field.preprocess(record[column], record)
        return data

    def postprocess_record(self, record):
        for field in self.all_possible_fields:
            if field.csv_column_name not in record:
                val = None
            else:
                val = record[field.csv_column_name]
            record[field.csv_column_name] = field.postprocess(val, record)

    fields_by_column_name = {field.csv_column_name:
                             field for field in all_possible_fields}

    def get_field_from_csv_column(self, column):
        return self.fields_by_column_name.get(column)

    def get_db_dict(self, record):
        return {
            self.get_field_from_csv_column(csv_col_name).database_name: data
            for csv_col_name, data in record.items()
            }

    def _process_file_stream(self, file_data):
        result = self.file_import_service.process_delimited_file_stream(file_data)
        if result.has_error():
            self.error_message(
                message=result.get_error_message(),
            )
            # TODO: log the actual exception
            headers, records = [], []
            dialect = None
        else:
            headers = result.get_headers()
            records = result.get_rows()
            dialect = result.get_dialect()

        return headers, records, dialect
