from taa.services.cases.census_import import (
    preprocess_date,
    preprocess_gender,
    preprocess_numbers,
    preprocess_string,
    preprocess_y_n,
    preprocess_zip,
    RequiredIfAnyInGroupValidator,
    birthdate_validator,
    email_validator,
    gender_validator,
    required_validator,
    ssn_validator,
    state_validator,
    zip_validator
    )


class EnrollmentImportService(object):
    def submit_file_records(self, records):
        return EnrollmentImportResponse()

class EnrollmentImportResponse(object):
    def __init__(self, result):
        self.errors = []

    def add_error(self, type, fields):
        error = EnrollmentImportError(type, fields)
        self.errors.append(error);
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
        self.fields = fields

    def get_type(self):
        return self.type

    def get_field(self):
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
            is_valid, error_message = validator(self, record)
            if not is_valid:
                parser.error_record_field(error_message,
                                          self.dict_key_name,
                                          parser.get_line_number(),
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

class EnrollmentRecordParser(object):
    # Modeled from
    # employee_first = CensusRecordField('EMP_FIRST', 'employee_first', preprocess_string, [required_validator])
    user_token = EnrollmentRecordField("user_token", "user_token", preprocess_string, [required_validator])
    case_token = EnrollmentRecordField("case_token", "case_token", preprocess_string, [required_validator])
    product_code = EnrollmentRecordField("product_code", "product_code", preprocess_string, [required_validator])
    payment_mode = EnrollmentRecordField("payment_mode", "payment_mode", preprocess_string, [required_validator])
    emp_first = EnrollmentRecordField("emp_first", "employee_first", preprocess_string, [required_validator])
    emp_last = EnrollmentRecordField("emp_last", "employee_last", preprocess_string, [required_validator])
    emp_ssn = EnrollmentRecordField("emp_ssn", "employee_ssn", preprocess_numbers, [required_validator, ssn_validator])
    emp_birthdate = EnrollmentRecordField("emp_birthdate", "employee_birthdate", preprocess_date, [required_validator, birthdate_validator])
    emp_coverage = EnrollmentRecordField("emp_coverage", "employee_coverage", preprocess_numbers, [required_validator])
    emp_premium = EnrollmentRecordField("emp_premium", "employee_premium", preprocess_numbers, [required_validator])
    emp_street = EnrollmentRecordField("emp_street", "employee_street", preprocess_string, [required_validator])
    emp_street2 = EnrollmentRecordField("emp_street2", "employee_street2", preprocess_string, [required_validator])
    emp_city = EnrollmentRecordField("emp_city", "employee_city", preprocess_string, [required_validator])
    emp_state = EnrollmentRecordField("emp_state", "employee_state", preprocess_string, [required_validator, state_validator])
    emp_zipcode = EnrollmentRecordField("emp_zip", "employee_zip", preprocess_zip, [required_validator, zip_validator])
    emp_phone = EnrollmentRecordField("emp_phone", "employee_phone", preprocess_string, [])
    emp_pin = EnrollmentRecordField("emp_pin", "employee_pin", preprocess_numbers, [required_validator])
    emp_sig_txt = EnrollmentRecordField("emp_sig_txt", "employee_sig_txt", preprocess_string, [required_validator])
    emp_application_date = EnrollmentRecordField("emp_application_date", "employee_application_date", preprocess_date, [required_validator])
    time_stamp = EnrollmentRecordField("time_stamp", "time_stamp", preprocess_date, [required_validator])
    signed_at_city = EnrollmentRecordField("signed_at_city", "signed_at_city", preprocess_string, [required_validator])
    signed_at_state = EnrollmentRecordField("signed_at_state", "signed_at_state", preprocess_string, [required_validator, state_validator])
    agent_name = EnrollmentRecordField("agent_name", "agent_name", preprocess_string, [required_validator])
    agent_code = EnrollmentRecordField("agent_code", "agent_code", preprocess_string, [required_validator])
    agent_sig_txt = EnrollmentRecordField("agent_sig_txt", "agent_sig_txt", preprocess_string, [required_validator])

    # Need to implement a way to check each record for required fields and return errors if they don't have them

    def process_records(self, records):
        preprocessed_records = (self.preprocess_record(record)
                                for record in records)
