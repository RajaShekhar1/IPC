from taa.services.preprocessors import preprocess_date
from taa.services.validators import required_validator, api_token_validator, case_token_validator, product_validator, \
    payment_mode_validator, gender_validator, ssn_validator, birthdate_validator, coverage_validator, premium_validator, \
    state_validator, zip_validator, question_answered_validator, RequiredIfAnyInGroupValidator, \
    enrollment_type_validator, email_validator, height_validator, weight_validator, replaced_or_financing_validator, \
    timestamp_validator
from taa.services import RequiredFeature
from taa.services.cases.census_import import preprocess_string, preprocess_numbers, preprocess_zip


class EnrollmentRecordField():
    def __init__(self, dict_key_name, database_name, preprocessor, validators,
                 post_processors=None, description="", title="", flat_file_size=0):
        self.dict_key_name = dict_key_name
        self.database_name = database_name
        self.description = description
        self.title = title
        self.flat_file_size = flat_file_size
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
    MAX_QUESTIONS = 10

    product_service = RequiredFeature("ProductService")
    case_service = RequiredFeature("CaseService")

    # Case/Record information
    user_token = EnrollmentRecordField("user_token", "user_token", preprocess_string, [required_validator, api_token_validator], flat_file_size=0, description="A token representing the API user")
    case_token = EnrollmentRecordField("case_token", "case_token", preprocess_string, [required_validator, case_token_validator], flat_file_size=0, description="A token identifying the TAA enrollment case")
    product_code = EnrollmentRecordField("product_code", "product_code", preprocess_string, [required_validator, product_validator], flat_file_size=8, description="A string specifying the product name")
    payment_mode = EnrollmentRecordField("payment_mode", "payment_mode", preprocess_numbers, [required_validator, payment_mode_validator], flat_file_size=2, description="A two digit number resenting the payment mode")
    enrollment_type = EnrollmentRecordField("enrollment_type", "enrollment_type", preprocess_string, [required_validator, enrollment_type_validator], flat_file_size=1, description="How the application was taken")

    # Employee Information
    emp_first = EnrollmentRecordField("emp_first", "employee_first", preprocess_string, [required_validator], flat_file_size=14, description="Employee first name")
    emp_last = EnrollmentRecordField("emp_last", "employee_last", preprocess_string, [required_validator], flat_file_size=20, description="Employee last name")
    emp_gender = EnrollmentRecordField("emp_gender", "employee_gender", preprocess_string, [required_validator, gender_validator], flat_file_size=1, description="Employee gender")
    emp_ssn = EnrollmentRecordField("emp_ssn", "employee_ssn", preprocess_numbers, [required_validator, ssn_validator], flat_file_size=9, description="Employee SSN")
    emp_birthdate = EnrollmentRecordField("emp_birthdate", "employee_birthdate", preprocess_date, [required_validator, birthdate_validator], flat_file_size=10, description="Employee Birthday")
    emp_coverage = EnrollmentRecordField("emp_coverage", "employee_coverage", preprocess_string, [coverage_validator], flat_file_size=6, description="Employee Coverage")
    emp_premium = EnrollmentRecordField("emp_premium", "employee_premium", preprocess_string, [premium_validator], flat_file_size=6, description="Employee Premium")
    emp_street = EnrollmentRecordField("emp_street", "employee_street", preprocess_string, [required_validator], flat_file_size=29, description="Employee street address")
    emp_street2 = EnrollmentRecordField("emp_street2", "employee_street2", preprocess_string, [], flat_file_size=29, description="Employee street address 2")
    emp_city = EnrollmentRecordField("emp_city", "employee_city", preprocess_string, [required_validator], flat_file_size=14, description="Employee city")
    emp_state = EnrollmentRecordField("emp_state", "employee_state", preprocess_string, [required_validator, state_validator], flat_file_size=2, description="2 character employee statecode")
    emp_zipcode = EnrollmentRecordField("emp_zipcode", "employee_zipcode", preprocess_zip, [required_validator, zip_validator], flat_file_size=9, description="Employee zipcode, up to 9 characters")
    emp_phone = EnrollmentRecordField("emp_phone", "employee_phone", preprocess_string, [], flat_file_size=10, description="Employee phone number")
    emp_pin = EnrollmentRecordField("emp_pin", "employee_pin", preprocess_numbers, [], flat_file_size=15, description="Employee pin, if provided.")
    emp_date_of_hire = EnrollmentRecordField("emp_date_of_hire", "employee_date_of_hire", preprocess_date, [required_validator], flat_file_size=10, description="Date of Hire")

    # Spouse Information
    sp_first = EnrollmentRecordField("sp_first", "spouse_first", preprocess_string, [], flat_file_size=14, description="Spouse first name")
    sp_last = EnrollmentRecordField("sp_last", "spouse_last", preprocess_string, [], flat_file_size=20, description="Spouse last name")
    sp_birthdate = EnrollmentRecordField("sp_birthdate", "spouse_birthdate", preprocess_date, [birthdate_validator], flat_file_size=10, description="Spouse birthdate")
    sp_gender = EnrollmentRecordField("sp_gender", "spouse_gender", preprocess_string, [gender_validator], flat_file_size=1, description="Spouse Gender")
    sp_ssn = EnrollmentRecordField("sp_ssn", "spouse_ssn", preprocess_numbers, [ssn_validator], flat_file_size=9, description="Spouse SSN")
    sp_street = EnrollmentRecordField("sp_street", "spouse_street", preprocess_string, [], flat_file_size=29, description="Spouse street address")
    sp_street2 = EnrollmentRecordField("sp_street2", "spouse_street2", preprocess_string, [], flat_file_size=29, description="Spouse street address 2")
    sp_city = EnrollmentRecordField("sp_city", "spouse_city", preprocess_string, [], flat_file_size=14, description="Spouse city")
    sp_state = EnrollmentRecordField("sp_state", "spouse_state", preprocess_string, [state_validator], flat_file_size=2, description="2 character Spouse statecode")
    sp_zipcode = EnrollmentRecordField("sp_zipcode", "spouse_zipcode", preprocess_zip, [zip_validator], flat_file_size=9, description="Spouse zipcode")
    sp_phone = EnrollmentRecordField("sp_phone", "spouse_phone", preprocess_string, [], flat_file_size=10, description="Spouse phone number")
    sp_coverage = EnrollmentRecordField("sp_coverage", "spouse_coverage", preprocess_string, [coverage_validator], flat_file_size=6, description="Spouse coverage")
    sp_premium = EnrollmentRecordField("sp_premium", "spouse_premium", preprocess_string, [premium_validator], flat_file_size=6, description="Spouse premium")

    # Optional Fields
    actively_at_work = EnrollmentRecordField("actively_at_work", "actively_at_work", preprocess_string, [question_answered_validator], flat_file_size=1, description="Is the Employee actively at work?")
    emp_email = EnrollmentRecordField("emp_email", "employee_email", preprocess_string, [email_validator], flat_file_size=40, description="Employee email address")
    emp_height_inches = EnrollmentRecordField("emp_height_inches", "employee_height_inches", preprocess_numbers, [height_validator], flat_file_size=2, description="Employee height in inches")
    emp_weight_pounds = EnrollmentRecordField("emp_weight_pounds", "employee_weight_pounds", preprocess_numbers, [weight_validator], flat_file_size=3, description="Employee weight in pounds")
    emp_smoker = EnrollmentRecordField("emp_smoker", "employee_smoker", preprocess_string, [question_answered_validator], flat_file_size=1, description="Is employee a tobacco user")

    sp_email = EnrollmentRecordField("sp_email", "spouse_email", preprocess_string, [email_validator], flat_file_size=40, description="Spouse email address")
    sp_height_inches = EnrollmentRecordField("sp_height_inches", "spouse_height_inches", preprocess_numbers, [height_validator], flat_file_size=2, description="Spouse height in inches")
    sp_weight_pounds = EnrollmentRecordField("sp_weight_pounds", "spouse_weight_pounds", preprocess_numbers, [weight_validator], flat_file_size=3, description="Spouse weight in pounds")
    sp_smoker = EnrollmentRecordField("sp_smoker", "spouse_smoker", preprocess_string, [question_answered_validator], flat_file_size=1, description="Is spouse a tobacco user")

    existing_insurance = EnrollmentRecordField("existing_insurance", "existing_insurance", preprocess_string, [question_answered_validator], flat_file_size=1, description="Does anyone on this application have any existing life insurance or annuity contracts?")
    replacing_insurance = EnrollmentRecordField("replacing_insurance", "replacing_insurance", preprocess_string, [question_answered_validator], flat_file_size=1, description="Will the coverage applied for replace any existing life insurance or annuities?")
    sp_treated_6_months = EnrollmentRecordField("sp_treated_6_months", "sp_treated_6_months", preprocess_string, [question_answered_validator], flat_file_size=1, description="During the prior 6 months, other than for routine medical care, has spouse been diagnosed or treated by a member of the medical profession in a hospital or any other medical facility?")
    sp_disabled_6_months = EnrollmentRecordField("sp_disabled_6_months", "sp_disabled_6_months", preprocess_string, [question_answered_validator], flat_file_size=1, description="Has spouse been disabled in the prior 6 months or received disability payments?")
    replacement_read_aloud = EnrollmentRecordField("replacement_read_aloud", "replacement_read_aloud", preprocess_string, [question_answered_validator], flat_file_size=1, description="Should replacement notice be read aloud")
    replacement_is_terminating = EnrollmentRecordField("replacement_is_terminating", "replacement_is_terminating", preprocess_string, [question_answered_validator], flat_file_size=1, description="Are you considering discontinuing making premium payments, surrendering, forfeiting, assigning to the insurer, or otherwise terminating your existing policy or contract?")
    replacement_using_funds = EnrollmentRecordField("replacement_using_funds", "replacement_using_funds", preprocess_string, [question_answered_validator], flat_file_size=1, description="Are you considering using funds from your existing policies or contracts to pay premiums due on the new policy or contract?")

    emp_bene_name = EnrollmentRecordField("emp_bene_name", "employee_bene_name", preprocess_string, [], flat_file_size=40, description="Employee primary beneficiary name")
    emp_bene_birthdate = EnrollmentRecordField("emp_bene_birthdate", "employee_bene_birthdate", preprocess_date, [birthdate_validator], flat_file_size=8, description="Employee primary beneficiary birthdate")
    emp_bene_relationship = EnrollmentRecordField("emp_bene_relationship", "employee_bene_relationship", preprocess_string, [], flat_file_size=15, description="Employee primary beneficiary relationship")
    emp_bene_ssn = EnrollmentRecordField("emp_bene_ssn", "employee_bene_ssn", preprocess_numbers, [ssn_validator], flat_file_size=9, description="Employee primary beneficiary SSN")
    sp_bene_name = EnrollmentRecordField("sp_bene_name", "spouse_bene_name", preprocess_string, [], flat_file_size=40, description="Spouse primary beneficiary name")
    sp_bene_birthdate = EnrollmentRecordField("sp_bene_birthdate", "spouse_bene_birthdate", preprocess_date, [], flat_file_size=8, description="Spouse primary beneficiary birthdate")
    sp_bene_relationship = EnrollmentRecordField("sp_bene_relationship", "spouse_bene_relationship", preprocess_string, [], flat_file_size=15, description="Spouse primary beneficiary relationship")
    sp_bene_ssn = EnrollmentRecordField("sp_bene_ssn", "spouse_bene_ssn", preprocess_numbers, [ssn_validator], flat_file_size=9, description="Spouse primary beneficiary SSN")
    emp_cont_bene_name = EnrollmentRecordField("emp_cont_bene_name", "employee_cont_bene_name", preprocess_string, [], flat_file_size=40, description="Employee contingent beneficiary name")
    emp_cont_bene_birthdate = EnrollmentRecordField("emp_cont_bene_birthdate", "employee_cont_bene_birthdate", preprocess_date, [birthdate_validator], flat_file_size=8, description="Employee contingent beneficiary birthdate")
    emp_cont_bene_relationship = EnrollmentRecordField("emp_cont_bene_relationship", "employee_cont_bene_relationship", preprocess_string, [], flat_file_size=15, description="Employee contingent beneficiary relationship")
    emp_cont_bene_ssn = EnrollmentRecordField("emp_cont_bene_ssn", "employee_cont_bene_ssn", preprocess_numbers, [ssn_validator], flat_file_size=9, description="Employee contingent beneficiary SSN")
    sp_cont_bene_name = EnrollmentRecordField("sp_cont_bene_name", "spouse_cont_bene_name", preprocess_string, [], flat_file_size=40, description="Spouse contingent beneficiary name")
    sp_cont_bene_birthdate = EnrollmentRecordField("sp_cont_bene_birthdate", "spouse_cont_bene_birthdate", preprocess_date, [birthdate_validator], flat_file_size=8, description="Spouse contingent beneficiary birthdate")
    sp_cont_bene_relationship = EnrollmentRecordField("sp_cont_bene_relationship", "spouse_cont_bene_relationship", preprocess_string, [], flat_file_size=15, description="Spouse contingent beneficiary relationship")
    sp_cont_bene_ssn = EnrollmentRecordField("sp_cont_bene_ssn", "spouse_cont_bene_ssn", preprocess_numbers, [ssn_validator], flat_file_size=9, description="Spouse contingent beneficiary SSN")

    # Signing Information
    emp_sig_txt = EnrollmentRecordField("emp_sig_txt", "employee_sig_txt", preprocess_string, [required_validator], flat_file_size=70, description="Signature line for employee")
    application_date = EnrollmentRecordField("application_date", "application_date", preprocess_date, [required_validator], flat_file_size=8, description="Date of the application")
    time_stamp = EnrollmentRecordField("time_stamp", "time_stamp", preprocess_date, [required_validator, timestamp_validator], flat_file_size=19, description="Time the application was received.")
    signed_at_city = EnrollmentRecordField("signed_at_city", "signed_at_city", preprocess_string, [required_validator], flat_file_size=15, description="City where the application was signed")
    signed_at_state = EnrollmentRecordField("signed_at_state", "signed_at_state", preprocess_string, [required_validator, state_validator], flat_file_size=2, description="State in which the enrollment was signed")
    agent_name = EnrollmentRecordField("agent_name", "agent_name", preprocess_string, [required_validator], flat_file_size=15, description="Agent signing name")
    agent_code = EnrollmentRecordField("agent_code", "agent_code", preprocess_string, [required_validator], flat_file_size=8, description="Agent code as provided")
    agent_sig_txt = EnrollmentRecordField("agent_sig_txt", "agent_sig_txt", preprocess_string, [required_validator], flat_file_size=70, description="Signature line for agent")

    # All spouse data is required if any spouse data is given
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

    all_fields = [
        # Case data
        user_token,
        case_token,
        product_code,
        payment_mode,
        enrollment_type,

        # Employee data
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
        emp_date_of_hire,
        actively_at_work,

        # Spouse data
        sp_first,
        sp_last,
        sp_birthdate,
        sp_gender,
        sp_street,
        sp_street2,
        sp_city,
        sp_state,
        sp_zipcode,
        sp_phone,
        sp_ssn,
        sp_coverage,
        sp_premium,

        # Optional fields
        emp_email,
        emp_height_inches,
        emp_weight_pounds,
        emp_smoker,
        sp_email,
        sp_height_inches,
        sp_weight_pounds,
        sp_smoker,

        emp_bene_name,
        emp_bene_birthdate,
        emp_bene_relationship,
        emp_bene_ssn,
        sp_bene_name,
        sp_bene_birthdate,
        sp_bene_relationship,
        sp_bene_ssn,
        emp_cont_bene_name,
        emp_cont_bene_birthdate,
        emp_cont_bene_relationship,
        emp_cont_bene_ssn,
        sp_cont_bene_name,
        sp_cont_bene_birthdate,
        sp_cont_bene_relationship,
        sp_cont_bene_ssn,

        # Signing data
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

    # Replacement info
    all_fields += [
        existing_insurance,
        replacing_insurance,
        sp_treated_6_months,
        sp_disabled_6_months,
        replacement_read_aloud,
        replacement_is_terminating,
        replacement_using_funds,
    ]

    MAX_POLICIES = 4
    for num in range(1, MAX_POLICIES + 1):
        all_fields += [
            EnrollmentRecordField("replacement_policy{}_name".format(num), "replacement_policy{}_name".format(num), preprocess_string, [], flat_file_size=20, description="Name of policy company"),
            EnrollmentRecordField("replacement_policy{}_number".format(num), "replacement_policy{}_number".format(num), preprocess_string, [], flat_file_size=16, description="Policy Number or ID"),
            EnrollmentRecordField("replacement_policy{}_insured".format(num), "replacement_policy{}_insured".format(num), preprocess_string, [], flat_file_size=20, description="Insured name on policy"),
            EnrollmentRecordField("replacement_policy{}_replaced_or_financing".format(num), "replacement_policy{}_replaced_or_financing".format(num), preprocess_string, [replaced_or_financing_validator], flat_file_size=1, description="Funds coming from replaced or financing"),
            EnrollmentRecordField("replacement_policy{}_reason".format(num), "replacement_policy{}_reason".format(num), preprocess_string, [], flat_file_size=70, description="Reason for replacing policy"),
        ]

    # Child data
    MAX_CHILDREN = 6
    for num in range(1, MAX_CHILDREN + 1):
        child_first = EnrollmentRecordField('ch{}_first'.format(num),
                                        'child{}_first'.format(num),
                                        preprocess_string, [], flat_file_size=14, description="Child first name")
        child_last = EnrollmentRecordField('ch{}_last'.format(num),
                                       'child{}_last'.format(num),
                                       preprocess_string, [], flat_file_size=20, description="Child last name")
        child_birthdate = EnrollmentRecordField('ch{}_birthdate'.format(num),
                                            'child{}_birthdate'.format(num),
                                            preprocess_date,
                                            [birthdate_validator], flat_file_size=8, description="Child birthdate")
        child_gender = EnrollmentRecordField('ch{}_gender'.format(num),
                                            'child{}_gender'.format(num),
                                            preprocess_string,
                                            [gender_validator], flat_file_size=1, description="Child gender")
        child_ssn = EnrollmentRecordField('ch{}_ssn'.format(num),
                                            'child{}_ssn'.format(num),
                                            preprocess_numbers,
                                            [ssn_validator], flat_file_size=9, description="Child SSN")
        child_coverage = EnrollmentRecordField('ch{}_coverage'.format(num),
                                            'child{}_coverage'.format(num),
                                            preprocess_string,
                                            [coverage_validator], flat_file_size=6, description="Child coverage")
        child_premium = EnrollmentRecordField('ch{}_premium'.format(num),
                                            'child{}_premium'.format(num),
                                            preprocess_string,
                                            [premium_validator], flat_file_size=6, description="Child premium")

        child_premium.add_validator(RequiredIfAnyInGroupValidator([child_coverage],
                                        "child_premium is required if child_coverage is provided")
                                    )
        child_coverage.add_validator(RequiredIfAnyInGroupValidator([child_premium],
                                        "child_coverage is required if child_premium is provided")
                                    )
        all_fields += [child_first, child_last, child_birthdate, child_gender, child_ssn, child_coverage, child_premium]

    for prefix, db_prefix in [("emp", "employee"), ("sp", "spouse")]:
        for q_num in range(1, MAX_QUESTIONS + 1):
            question = EnrollmentRecordField("{}_question_{}_answer".format(prefix, q_num),
                                            "{}_question_{}_answer".format(db_prefix, q_num),
                                            preprocess_string,
                                            [question_answered_validator],
                                            flat_file_size=1, description="{} Answer to Statement of Health Question".format(db_prefix.capitalize())
                                            )
            all_fields += [question]

    # Add child questions
    for num in range(1, MAX_CHILDREN + 1):
        for q_num in range(1, MAX_QUESTIONS + 1):
            ch_question = EnrollmentRecordField("ch{}_question_{}_answer".format(num, q_num),
                                                "child{}_question_{}_answer".format(num, q_num),
                                                preprocess_string,
                                                [question_answered_validator],
                                                flat_file_size=1, description="Child Answer to Statement of Health Question"
                                                )
            all_fields += [ch_question]
    def __init__(self):
        self.errors = []
        self.valid_data = []
        self.current_record_number = 0

    def process_records(self, records, case):

        self.current_record_number = 0

        self.validate_data_keys(records)
        # Don't do any more processing if missing important data_keys
        if self.errors:
            return

        preprocessed_records = (self.preprocess_record(record) for record in records)
        for record in preprocessed_records:
            self.current_record_number += 1

            validation_tests = [
                lambda: self.validate_record(record),
                lambda: self.validate_statecode(record),
                lambda: self.validate_coverage_selected(record),
                lambda: self.validate_case(record, case),
            ]
            is_valid = True
            for test in validation_tests:
                if not test():
                    is_valid = False
                    break
            if is_valid:
                self.postprocess_record(record)
                self.valid_data.append(record)

    fields_by_dict_key = {field.dict_key_name: field for field in all_fields}

    def get_field_by_dict_key(self, dict_key):
        return self.fields_by_dict_key.get(dict_key)

    def validate_case(self, record, default_case):
        if 'case_token' in record:
            case = self.case_service.get_case_for_token(record['case_token'])
        else:
            case = default_case

        if not case:
            self.error_record_field("invalid_case",
                                    "A valid case could not be found for the current information",
                                    "case_token",
                                    record)
            return False

        # Validate case is enrolling
        if not self.case_service.is_enrolling(case):
            self.error_record_field("invalid_case", "Case is not enrolling", "case_token", record)
            return False

        # Store the case ID in the record
        record['case_id'] = case.id

        return True

    def validate_statecode(self, record):
        if not self.product_service.is_valid_statecode_for_product(record.get("product_code"), record.get("signed_at_state")):
            self.error_record_field("invalid_state_for_product",
                                    "Provided 'signed at state' is invalid for this product.",
                                    "signed_at_state",
                                    record)
            return False
        return True

    def validate_coverage_selected(self, record):
        if record['emp_coverage'] or record.get('sp_coverage'):
            return True

        for num in range(1, self.MAX_CHILDREN + 1):
            if record.get('ch{}_coverage'.format(num)):
                return True

        self.error_record_field("missing_data", "At least one applicant must select coverage", "emp_coverage", record)
        return False

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
            record=data,
            record_num=self.current_record_number,
            field_name=field_name,
        ))

    def get_valid_data(self):
        if self.errors:
            # Right now, it is all or nothing, so return empty list if any
            # errors occurred
            return []
        return self.valid_data

    def _get_missing_data_keys(self, record):

        # Do a case-insensitive match on the columns
        for key in record.keys():
            record[key.lower()] = record[key]
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
            "emp_date_of_hire",
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
            self.error_record_field(type="missing_header",
                                    message="The uploaded CSV file did not appear to have a valid header "
                                            "row. Please see the sample data file for formatting examples.",
                                    field_name="",
                                    data=None)
        for record in records:
            missing_keys = self._get_missing_data_keys(record)
            for key in missing_keys:
                self.error_record_field(type="missing_header",
                                        message="Missing required table column",
                                        field_name=self.get_field_by_dict_key(key).dict_key_name,
                                        data=record)


    def _does_record_have_spouse(self, record):
        return record.get("sp_first")

    def _does_record_have_child(self, record, num):
        return record.get("ch{}_first".format(num))
