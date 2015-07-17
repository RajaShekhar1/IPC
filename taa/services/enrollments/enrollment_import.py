import csv

from taa.services.cases.census_import import (
    preprocess_numbers,
    preprocess_string,
    preprocess_zip,
    )

from taa.services.preprocessors import *

from taa.services.validators import *

from taa.services import RequiredFeature

class EnrollmentImportService(object):
    def convert_csv_to_json(self, csv_bytes):
        pass

    def submit_file_records(self, records):
        response = EnrollmentImportResponse()
        parser = EnrollmentRecordParser()

        # Process all records
        parser.process_records(records)
        errors = parser.errors
        # If there are errors in the parser, add them to the response
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
            field = self
            is_valid, error_type, error_message = validator(field, record)
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


class EnrollmentRecordParser(object):
    product_service = RequiredFeature("ProductService")
    #Case/Record information
    user_token = EnrollmentRecordField("user_token", "user_token", preprocess_string, [required_validator, api_token_validator])
    case_token = EnrollmentRecordField("case_token", "case_token", preprocess_string, [required_validator, case_token_validator])
    product_code = EnrollmentRecordField("product_code", "product_code", preprocess_string, [required_validator, product_validator])
    payment_mode = EnrollmentRecordField("payment_mode", "payment_mode", preprocess_string, [required_validator, payment_mode_validator])

    #Employee Information
    emp_first = EnrollmentRecordField("emp_first", "employee_first", preprocess_string, [required_validator])
    emp_last = EnrollmentRecordField("emp_last", "employee_last", preprocess_string, [required_validator])
    emp_gender = EnrollmentRecordField("emp_gender", "employee_gender", preprocess_string, [required_validator, gender_validator])
    emp_ssn = EnrollmentRecordField("emp_ssn", "employee_ssn", preprocess_numbers, [required_validator, ssn_validator])
    emp_birthdate = EnrollmentRecordField("emp_birthdate", "employee_birthdate", preprocess_date, [required_validator, birthdate_validator])
    emp_coverage = EnrollmentRecordField("emp_coverage", "employee_coverage", preprocess_string, [required_validator, coverage_validator])
    emp_premium = EnrollmentRecordField("emp_premium", "employee_premium", preprocess_string, [required_validator, premium_validator])
    emp_street = EnrollmentRecordField("emp_street", "employee_street", preprocess_string, [required_validator])
    emp_street2 = EnrollmentRecordField("emp_street2", "employee_street2", preprocess_string, [])
    emp_city = EnrollmentRecordField("emp_city", "employee_city", preprocess_string, [required_validator])
    emp_state = EnrollmentRecordField("emp_state", "employee_state", preprocess_string, [required_validator, state_validator])
    emp_zipcode = EnrollmentRecordField("emp_zipcode", "employee_zipcode", preprocess_zip, [required_validator, zip_validator])
    emp_phone = EnrollmentRecordField("emp_phone", "employee_phone", preprocess_string, [])
    emp_pin = EnrollmentRecordField("emp_pin", "employee_pin", preprocess_numbers, [required_validator])

    #Spouse Information
    sp_first = EnrollmentRecordField("sp_first", "spouse_first", preprocess_string, [])
    sp_last = EnrollmentRecordField("sp_last", "spouse_last", preprocess_string, [])
    sp_birthdate = EnrollmentRecordField("sp_birthdate", "spouse_birthdate", preprocess_date, [birthdate_validator])
    sp_ssn = EnrollmentRecordField("sp_ssn", "spouse_ssn", preprocess_numbers, [ssn_validator])
    sp_coverage = EnrollmentRecordField("sp_coverage", "spouse_coverage", preprocess_string, [coverage_validator])
    sp_premium = EnrollmentRecordField("sp_premium", "spouse_premium", preprocess_string, [premium_validator])

    #Signing Information
    emp_sig_txt = EnrollmentRecordField("emp_sig_txt", "employee_sig_txt", preprocess_string, [required_validator])
    application_date = EnrollmentRecordField("application_date", "application_date", preprocess_date, [required_validator])
    time_stamp = EnrollmentRecordField("time_stamp", "time_stamp", preprocess_date, [required_validator])
    signed_at_city = EnrollmentRecordField("signed_at_city", "signed_at_city", preprocess_string, [required_validator])
    signed_at_state = EnrollmentRecordField("signed_at_state", "signed_at_state", preprocess_string, [required_validator, state_validator])
    agent_name = EnrollmentRecordField("agent_name", "agent_name", preprocess_string, [required_validator])
    agent_code = EnrollmentRecordField("agent_code", "agent_code", preprocess_string, [required_validator])
    agent_sig_txt = EnrollmentRecordField("agent_sig_txt", "agent_sig_txt", preprocess_string, [required_validator])

    #All spouse data is required if any spouse data is given
    spouse_fields = [sp_first, sp_last, sp_birthdate, sp_ssn]
    for field in spouse_fields:
        validator = RequiredIfAnyInGroupValidator(
            spouse_fields,
            message="{} is required if any of the following are"
                    "provided: {}".format(field.dict_key_name,
                                          ', '.join([f.dict_key_name
                                                     for f in spouse_fields
                                                     if f is not field])
                                          ))
        # If any in group provided, all must be valid
        field.add_validator(validator)

    premium_coverage_required = [
        (emp_premium, emp_coverage),
        (emp_coverage, emp_premium),
        (sp_premium, sp_coverage),
        (sp_coverage, sp_premium)
    ]

    for field, group in premium_coverage_required:
        field.add_validator(RequiredIfAnyInGroupValidator([group],
                            "{} is required if {} is provided".format(field.dict_key_name, group.dict_key_name))
                            )

    sp_premium.add_validator(
            RequiredIfAnyInGroupValidator(
                [sp_coverage],
                "Spouse premium is required if spouse coverage is provided"
            )
        )

    all_fields = [
        #Case data
        user_token,
        case_token,
        product_code,
        payment_mode,

        #Employee data
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

        #Spouse data
        sp_first,
        sp_last,
        sp_birthdate,
        sp_ssn,
        sp_coverage,
        sp_premium,

        #Signing data
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

    #Child data
    MAX_CHILDREN = 6
    for num in range(1, MAX_CHILDREN + 1):
        child_first = EnrollmentRecordField('ch{}_first'.format(num),
                                        'child{}_first'.format(num),
                                        preprocess_string, [])
        child_last = EnrollmentRecordField('ch{}_last'.format(num),
                                       'child{}_last'.format(num),
                                       preprocess_string, [])
        child_birthdate = EnrollmentRecordField('ch{}_birthdate'.format(num),
                                            'child{}_birthdate'.format(num),
                                            preprocess_date,
                                            [birthdate_validator])
        child_ssn = EnrollmentRecordField('ch{}_ssn'.format(num),
                                            'child{}_ssn'.format(num),
                                            preprocess_numbers,
                                            [ssn_validator])
        child_coverage = EnrollmentRecordField('ch{}_coverage'.format(num),
                                            'child{}_coverage'.format(num),
                                            preprocess_string,
                                            [coverage_validator])
        child_premium = EnrollmentRecordField('ch{}_premium'.format(num),
                                            'child{}_premium'.format(num),
                                            preprocess_string,
                                            [premium_validator])

        child_premium.add_validator(RequiredIfAnyInGroupValidator([child_coverage],
                                        "child_premium is required if child_coverage is provided")
                                    )
        child_coverage.add_validator(RequiredIfAnyInGroupValidator([child_premium],
                                        "child_coverage is required if child_premium is provided")
                                    )
        all_fields += [child_first, child_last, child_birthdate, child_ssn, child_coverage, child_premium]


    def __init__(self):
        self.errors = []
        self.valid_data = []

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
        for record in preprocessed_records:
            if self.validate_record(record):
                self.postprocess_record(record)
                self.valid_data.append(record)
        self.validate_questions(records)

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
