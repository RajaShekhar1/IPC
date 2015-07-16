import csv

from taa.services.cases.census_import import (
    preprocess_date,
    preprocess_gender,
    preprocess_numbers,
    preprocess_string,
    preprocess_y_n,
    preprocess_zip,
    RequiredIfAnyInGroupValidator
    )

import re
import dateutil.parser
from datetime import datetime

from taa.services.products.payment_modes import is_payment_mode


class EnrollmentImportService(object):
    def convert_csv_to_json(self, csv_bytes):
        pass

    def submit_file_records(self, records):
        response = EnrollmentImportResponse()
        parser = EnrollmentRecordParser()
        # Process all records
        parser.process_records(records)
        errors = parser.errors

        # If there are errors in the parser, return
        for error in errors:
            response.add_error(error["type"], error["field_name"])

        return response



class EnrollmentImportResponse(object):
    def __init__(self):
        self.errors = []

    def add_error(self, type, fields):
        error = EnrollmentImportError(type, fields)
        self.errors.append(error)
        return error

    def is_success(self):
        return not self.errors

    def is_error(self):
        return not self.is_success()

    def get_errors(self):
        return self.errors


class EnrollmentImportError(object):
    def __init__(self, type, fields):
        self.type = type
        self.fields = [fields]

    def get_type(self):
        return self.type

    def get_fields(self):
        """
        returns a list of column names that this error refers to.
        """
        return self.fields

    def get_message(self):
        error_messages = dict()
        return error_messages.get(self.type, "Error with column: {}".format(self.type))


class EnrollmentRecordField():
    def __init__(self, dict_key_name, database_name, preprocessor, validators,
                 post_processors=None):
        self.dict_key_name = dict_key_name
        self.database_name = database_name
        self.preprocessor = preprocessor or (lambda x: x)
        self.validators = validators or []
        self.post_processors = post_processors or []

    def validate(self, parser, record):
        all_valid = True
        for validator in self.validators:
            is_valid, error_type, error_message = validator(self, record)
            if not is_valid:
                parser.error_record_field(error_type,
                                          error_message,
                                          self.dict_key_name,
                                          record)
                all_valid = False
        return all_valid

    def get_column_from_record(self, record):
        return record.get(self.dict_key_name, u'')

    def preprocess(self, data, record):
        return self.preprocessor(data, record)

    def postprocess(self, field_data, all_data):
        new_data = field_data
        for postprocessor in self.post_processors:
            new_data = postprocessor(self, new_data, all_data)
        return new_data

    def add_validator(self, validator):
        self.validators.append(validator)

# VALIDATORS

def required_validator(field, record, message=None):
    data = field.get_column_from_record(record)
    if not data:
        message = message if message else "Required Data Missing"
        return False, "missing_data", message
    return True, None, None

def ssn_validator(field, record):
    ssn_pattern = re.compile('^\d{9}$')
    ssn = field.get_column_from_record(record)
    if not ssn:
        # Allow blank unless combined with required validator
        return True, None, None
    elif not ssn_pattern.match(ssn):
        return False, "invalid_ssn", "Invalid SSN"
    return True, None, None

def payment_mode_validator(field, record):
    payment_mode = field.get_column_from_record(record)
    if not payment_mode:
        # If product is not set and it is not required, we can return true
        return True, None, None
    if not is_payment_mode(name=payment_mode):
        return False, "invalid_mode", "Invalid payment mode"
    return True, None, None

def product_validator(field, record):
    # Needs a database call to check if product exists
    return True, None, None

def gender_validator(field, record):
    gender = field.get_column_from_record(record)
    if not gender:
        # Allow blank unless combined with required validator
        return True, None, None
    if gender not in ['male', 'female', 'm', 'f']:
        return False, "invalid_gender", "Gender must be 'Male' or 'Female'"
    return True, None, None

def birthdate_validator(field, record):
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


def email_validator(field, record):
    email = field.get_column_from_record(record)
    if not email:
        # Allow blank unless combined with required validator
        return True, None, None
    if '@' not in email and len(email) < 3:
        return False, "invalid_email", 'Invalid email'
    return True, None, None

def coverage_validator(field, record):
    coverage_pattern = re.compile('^[0-9]+$')
    coverage = field.get_column_from_record(record)
    if not coverage:
        return True, None, None
    if not coverage_pattern.match(coverage):
        return False, "invalid_coverage", "Invalid coverage format"
    return True, None, None

zip_pattern = re.compile('^\d{3,5}$')


def zip_validator(field, record):
    zip = field.get_column_from_record(record)
    if not zip:
        # Allow blank unless combined with required validator
        return True, None, None
    if not zip_pattern.match(zip):
        return False, "invalid_zip", "Invalid ZIP code"
    return True, None, None


def state_validator(field, record):
    state = field.get_column_from_record(record)
    if not state:
        return True, None, None

    from taa.services.products import ProductService
    ps = ProductService()
    if not state or not len(state) == 2 or not state in ps.get_all_statecodes():
        return False, "invalid_state", "Invalid US State. Must be two-letter abbreviation."

    return True, None, None



class EnrollmentRecordParser(object):
    user_token = EnrollmentRecordField("user_token", "user_token", preprocess_string, [required_validator])
    case_token = EnrollmentRecordField("case_token", "case_token", preprocess_string, [required_validator])
    product_code = EnrollmentRecordField("product_code", "product_code", preprocess_string, [required_validator, product_validator])
    payment_mode = EnrollmentRecordField("payment_mode", "payment_mode", preprocess_string, [required_validator, payment_mode_validator])
    emp_first = EnrollmentRecordField("emp_first", "employee_first", preprocess_string, [required_validator])
    emp_last = EnrollmentRecordField("emp_last", "employee_last", preprocess_string, [required_validator])
    emp_gender = EnrollmentRecordField("emp_gender", "employee_gender", preprocess_gender, [required_validator, gender_validator])
    emp_ssn = EnrollmentRecordField("emp_ssn", "employee_ssn", preprocess_numbers, [required_validator, ssn_validator])
    emp_birthdate = EnrollmentRecordField("emp_birthdate", "employee_birthdate", preprocess_date, [required_validator, birthdate_validator])
    emp_coverage = EnrollmentRecordField("emp_coverage", "employee_coverage", preprocess_string, [required_validator, coverage_validator])
    emp_premium = EnrollmentRecordField("emp_premium", "employee_premium", preprocess_string, [required_validator])
    emp_street = EnrollmentRecordField("emp_street", "employee_street", preprocess_string, [required_validator])
    emp_street2 = EnrollmentRecordField("emp_street2", "employee_street2", preprocess_string, [])
    emp_city = EnrollmentRecordField("emp_city", "employee_city", preprocess_string, [required_validator])
    emp_state = EnrollmentRecordField("emp_state", "employee_state", preprocess_string, [required_validator, state_validator])
    emp_zipcode = EnrollmentRecordField("emp_zipcode", "employee_zipcode", preprocess_zip, [required_validator, zip_validator])
    emp_phone = EnrollmentRecordField("emp_phone", "employee_phone", preprocess_string, [])
    emp_pin = EnrollmentRecordField("emp_pin", "employee_pin", preprocess_numbers, [required_validator])
    emp_sig_txt = EnrollmentRecordField("emp_sig_txt", "employee_sig_txt", preprocess_string, [required_validator])
    application_date = EnrollmentRecordField("application_date", "application_date", preprocess_date, [required_validator])
    time_stamp = EnrollmentRecordField("time_stamp", "time_stamp", preprocess_date, [required_validator])
    signed_at_city = EnrollmentRecordField("signed_at_city", "signed_at_city", preprocess_string, [required_validator])
    signed_at_state = EnrollmentRecordField("signed_at_state", "signed_at_state", preprocess_string, [required_validator, state_validator])
    agent_name = EnrollmentRecordField("agent_name", "agent_name", preprocess_string, [required_validator])
    agent_code = EnrollmentRecordField("agent_code", "agent_code", preprocess_string, [required_validator])
    agent_sig_txt = EnrollmentRecordField("agent_sig_txt", "agent_sig_txt", preprocess_string, [required_validator])
    all_fields = [
        user_token,
        case_token,
        product_code ,
        payment_mode ,
        emp_first,
        emp_last,
        emp_gender,
        emp_ssn,
        emp_birthdate,
        emp_coverage,
        emp_premium,
        emp_street,
        emp_street2,
        emp_city,
        emp_state,
        emp_zipcode,
        emp_phone,
        emp_pin,
        emp_sig_txt,
        application_date,
        time_stamp,
        signed_at_city,
        signed_at_state,
        agent_name,
        agent_code,
        agent_sig_txt
    ]

    def __init__(self):
        self.errors = []
        self.valid_data = []
        self.used_ssns = set()

    fields_by_dict_key = {field.dict_key_name: field for field in all_fields}

    def get_field_by_dict_key(self, dict_key):
        return self.fields_by_dict_key.get(dict_key)

    def process_records(self, records):
        self.validate_data_keys(records)
        # Don't do any more processing if missing important data_keys
        if self.errors:
            return
        preprocessed_records = (self.preprocess_record(record)
                                for record in records)
        valid_data = []
        for record in preprocessed_records:
            if self.validate_record(record):
                self.postprocess_record(record)
                self.valid_data.append(record)


    def preprocess_record(self, record):
        data = {}
        for key in record:
            field = self.get_field_by_dict_key(key)
            if not field:
                continue
            data[key] = field.preprocess(record[key], record)
        return data

    def postprocess_record(self, record):
        for field in self.all_fields:
            if field.dict_key_name not in record:
                val = None
            else:
                val = record[field.dict_key_name]
            record[field.dict_key_name] = field.postprocess(val, record)

    def validate_record(self, record):
        is_valid = True
        for field in self.all_fields:
            is_valid &= field.validate(self, record)
        # Do not allow duplicate employee SSNs in a single upload
        ssn = record.get(self.emp_ssn.dict_key_name)
        if ssn and ssn in self.used_ssns:
            is_valid = False
            self.error_record_field(
                "Duplicate SSN in data",
                self.emp_ssn.dict_key_name,
                record
            )
        elif ssn:
            self.used_ssns.add(ssn)
        # Some modes require us to throw an error if an existing record exists
        # in the database (matched on SSN). Check that here.
        # if self.error_if_matching and ssn in self.error_if_matching:
        #     self.error_record_field(
        #         "A census record exists that matches this SSN. This is not "
        #         "allowed when uploading in 'Add New Records' mode.",
        #         self.emp.csv_column_name,
        #         record
        #     )
        return is_valid

    def error_record_field(self, type, message, field_name, data):
        self.errors.append(dict(
            type=type,
            message=message,
            records=[data],
            field_name=field_name,
        ))

    def get_valid_data(self):
        if self.errors:
            # Right now, it is all or nothing, so return empty list if any
            # errors occurred
            return []
        return self.valid_data

    def _get_missing_data_keys(self, record):
        required_data_keys = [
            "user_token",
            "case_token",
            "product_code",
            "payment_mode",
            "emp_first",
            "emp_last",
            "emp_gender",
            "emp_ssn",
            "emp_birthdate",
            "emp_coverage",
            "emp_premium",
            "emp_street",
            "emp_street2",
            "emp_city",
            "emp_state",
            "emp_zipcode",
            "emp_pin",
            "emp_sig_txt",
            "application_date",
            "time_stamp",
            "signed_at_city",
            "signed_at_state",
            "agent_name",
            "agent_code",
            "agent_sig_txt"
        ]
        return {d for d in required_data_keys if d not in record}

    def validate_data_keys(self, records):
        if len(records) == 0:
            self.error_message(
                "The uploaded CSV file did not appear to have a valid header "
                "row. Please see the sample data file for formatting examples."
            )
        for record in records:
            missing_keys = self._get_missing_data_keys(record)
            for key in missing_keys:
                self.error_record_field(type="missing_header",
                                        message="Missing required table column",
                                        field_name=self.get_field_by_dict_key(key).dict_key_name,
                                        data=record)
