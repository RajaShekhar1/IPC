import csv

from taa.core import TAAFormError
from taa.services.cases.census_import import (
    preprocess_numbers,
    preprocess_string,
    preprocess_zip,
    )

from taa.services.preprocessors import *
from taa.services.validators import *

from taa.services.products.payment_modes import is_payment_mode, MODES_BY_NAME
from taa.services.products import StatementOfHealthQuestionService
from taa.services import RequiredFeature, LookupService
from taa.services.enrollments.enrollment_import_processor import EnrollmentProcessor, EnrollmentImportError

class EnrollmentImportService(object):
    case_service = RequiredFeature("CaseService")
    file_import_service = RequiredFeature("FileImportService")

    def process_enrollment_data(self, data, data_format, case_token=None, auth_token=None):
        processor = EnrollmentProcessor()
        processor.process_enrollment_import_request(
            data,
            data_format,
            case_token=case_token,
            auth_token=auth_token
        )

        return processor

    def submit_file_records(self, records):
        response = EnrollmentImportResponse()
        parser = EnrollmentRecordParser()
        # Process all records
        parser.process_records(records, case=None)
        errors = parser.errors
        # If there are errors in the parser, add them to the response
        for error in errors:
            response.add_error(error["type"], error["field_name"], message=error['message'])

        response.records = parser.get_valid_data()

        return response

    def standardize_imported_data(self, data):
        product_service = LookupService("ProductService")
        def build_person(prefix):
            base_dict = dict(
                first=data.get("{}_first".format(prefix)),
                last=data.get("{}_last".format(prefix)),
                birthdate=data.get("{}_birthdate".format(prefix)),
                ssn=data.get("{}_ssn".format(prefix)),
                gender=data.get("{}_gender".format(prefix)),
                phone=data.get("{}_phone".format(prefix)),
                address1=data.get("{}_street".format(prefix)),
                address2=data.get("{}_street2".format(prefix)),
                city=data.get("{}_city".format(prefix)),
                state=data.get("{}_state".format(prefix)),
                zip=data.get("{}_zipcode".format(prefix)),
                height=data.get("{}_height_inches".format(prefix)),
                weight=data.get("{}_weight_pounds".format(prefix)),
                is_smoker=data.get("{}_smoker".format(prefix))=="Y",
                soh_questions=[]
            )
            answers = dict(Y="Yes", N="No", y="Yes", n="No")
            code = data.get("product_code")
            products = product_service.get_products_by_codes([code])
            product = products[0]
            state = data.get("signed_at_state")
            questions = StatementOfHealthQuestionService().get_health_questions(product, state)

            for q_num in range(1, len(questions)+1):
                answer = data.get("{}_question_{}_answer".format(prefix, q_num))

                if answer:
                    base_dict["soh_questions"].append(dict(
                        question=questions[q_num-1].question,
                        answer=answers.get(answer)
                    ))
            return base_dict

        out_data = {
            "employee": {},
            "spouse": {},
            "children": [],
            "child_coverages": [],
            "replacement_policies": [],
        }

        out_data["is_third_party"] = data.get("is_third_party")
        out_data["did_decline"] = data.get("did_decline") or False

        out_data["enrollCity"] = data.get("signed_at_city")
        out_data["enrollState"] = data.get("signed_at_state")
        out_data["product_type"] = data.get("product_code")

        out_data["payment_mode"] = MODES_BY_NAME.get(data["payment_mode"])
        out_data["payment_mode_text"] = data["payment_mode"]

        out_data["existing_insurance"] = data.get("existing_insurance") in ["Y","y"]

        out_data["replacing_insurance"] = data.get("replacing_insurance") in ["Y","y"]

        out_data["replacement_read_aloud"] = data.get("replacement_read_aloud") in ["Y","y"]

        out_data["replacement_is_terminating"] = data.get("replacement_is_terminating") in ["Y","y"]

        out_data["replacement_using_funds"] = data.get("replacement_using_funds") in ["Y","y"]

        out_data["is_employee_actively_at_work"] = data.get("actively_at_work") in ["Y","y"]

        out_data["has_spouse_been_treated_6_months"] = data.get("sp_treated_6_months") in ["Y","y"]

        out_data["has_spouse_been_disabled_6_months"] = data.get("sp_disabled_6_months") in ["Y","y"]

        out_data["employee_beneficiary"] = data.get("emp_bene_relationship")
        out_data["employee_beneficiary_name"] = data.get("emp_bene_name")
        out_data["employee_beneficiary_ssn"] = data.get("emp_bene_ssn")
        out_data["employee_beneficiary_relationship"] = data.get("emp_bene_relationship")

        out_data["spouse_beneficiary"] = data.get("sp_bene_relationship")
        out_data["spouse_beneficiary_name"] = data.get("sp_bene_name")
        out_data["spouse_beneficiary_ssn"] = data.get("sp_bene_ssn")
        out_data["spouse_beneficiary_relationship"] = data.get("sp_bene_relationship")

        out_data["replacement_policies"].append(dict(
            name=data.get("replacement_policy1_name"),
            policy_number=data.get("replacement_policy1_number"),
            insured=data.get("replacement_policy1_insured"),
            replaced_or_financing=data.get("replacement_policy1_replaced_or_financing"),
            replacement_reason=data.get("replacement_policy1_reason")
        ))

        emp_address = "{}{}{}{}{}".format(data.get("emp_street"), data.get("emp_street2"), data.get("emp_city"), data.get("emp_state"), data.get("emp_zipcode"))

        sp_address = "{}{}{}{}{}".format(data.get("sp_street"), data.get("sp_street2"), data.get("sp_city"), data.get("sp_state"), data.get("sp_zipcode"))

        out_data["is_spouse_address_same_as_employee"] = emp_address == sp_address

        out_data["employee"].update(build_person("emp"))
        out_data["employee_coverage"] = dict(
            face_value=data.get("emp_coverage"),
            premium=data.get("emp_premium")
        )
        out_data["spouse"].update(build_person("sp"))
        out_data["spouse_coverage"] = dict(
            face_value=data.get("sp_coverage"),
            premium=data.get("sp_premium")
        )
        for num in range(1, EnrollmentRecordParser.MAX_CHILDREN+1):
            if data.get("ch{}_first".format(num)):
                out_data["children"].append(build_person("ch{}".format(num)))
                out_data["child_coverages"].append(dict(
                    face_value=data.get("ch{}_coverage".format(num)),
                    premium=data.get("ch{}_premium".format(num))
                ))
        return out_data

class EnrollmentImportResponse(object):
    def __init__(self):
        self.errors = []
        self.records = []

    def add_error(self, type, fields, message):
        error = EnrollmentImportError(type, fields, message)
        self.errors.append(error)
        return error

    def is_success(self):
        return not self.errors

    def is_error(self):
        return not self.is_success()

    def get_errors(self):
        return self.errors

    def get_parsed_records(self):
        return self.records



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
    user_token = EnrollmentRecordField("user_token", "user_token", preprocess_string, [required_validator, api_token_validator], flat_file_size=64, description="A token representing the api user")
    case_token = EnrollmentRecordField("case_token", "case_token", preprocess_string, [required_validator, case_token_validator], flat_file_size=64, description="A token representing a case")
    product_code = EnrollmentRecordField("product_code", "product_code", preprocess_string, [required_validator, product_validator], flat_file_size=5, description="A 5 character string representing the product")
    payment_mode = EnrollmentRecordField("payment_mode", "payment_mode", preprocess_string, [required_validator, payment_mode_validator], flat_file_size=2, description="A two digit number resenting the payment mode")

    # Employee Information
    emp_first = EnrollmentRecordField("emp_first", "employee_first", preprocess_string, [required_validator], flat_file_size=14, description="Employee first name")
    emp_last = EnrollmentRecordField("emp_last", "employee_last", preprocess_string, [required_validator], flat_file_size=20, description="Employee last name")
    emp_gender = EnrollmentRecordField("emp_gender", "employee_gender", preprocess_string, [required_validator, gender_validator], flat_file_size=1, description="Employee gender, either 'M' or 'F'")
    emp_ssn = EnrollmentRecordField("emp_ssn", "employee_ssn", preprocess_numbers, [required_validator, ssn_validator], flat_file_size=9, description="Employee SSN, format NNNNNNNNN")
    emp_birthdate = EnrollmentRecordField("emp_birthdate", "employee_birthdate", preprocess_date, [required_validator, birthdate_validator], flat_file_size=8, description="Employee Birthday, format MMDDCCYY")
    emp_coverage = EnrollmentRecordField("emp_coverage", "employee_coverage", preprocess_string, [required_validator, coverage_validator], flat_file_size=6, description="Employee Coverage, format NNNNNN")
    emp_premium = EnrollmentRecordField("emp_premium", "employee_premium", preprocess_string, [required_validator, premium_validator], flat_file_size=6, description="Employee Premium, format NN.NNN")
    emp_street = EnrollmentRecordField("emp_street", "employee_street", preprocess_string, [required_validator], flat_file_size=29, description="Employee street address")
    emp_street2 = EnrollmentRecordField("emp_street2", "employee_street2", preprocess_string, [], flat_file_size=29, description="Employee street address 2")
    emp_city = EnrollmentRecordField("emp_city", "employee_city", preprocess_string, [required_validator], flat_file_size=14, description="Employee city")
    emp_state = EnrollmentRecordField("emp_state", "employee_state", preprocess_string, [required_validator, state_validator], flat_file_size=2, description="2 character employee statecode")
    emp_zipcode = EnrollmentRecordField("emp_zipcode", "employee_zipcode", preprocess_zip, [required_validator, zip_validator], flat_file_size=9, description="Employee zipcode, up to 9 characters")
    emp_phone = EnrollmentRecordField("emp_phone", "employee_phone", preprocess_string, [], flat_file_size=10, description="Employee phone number, format NNNNNNNNNN")
    emp_pin = EnrollmentRecordField("emp_pin", "employee_pin", preprocess_numbers, [required_validator], flat_file_size=15, description="Employee pin, format NNNNNNNNNNNNNNN")

    # Spouse Information
    sp_first = EnrollmentRecordField("sp_first", "spouse_first", preprocess_string, [], flat_file_size=14, description="Spouse first name")
    sp_last = EnrollmentRecordField("sp_last", "spouse_last", preprocess_string, [], flat_file_size=20, description="Spouse last name")
    sp_birthdate = EnrollmentRecordField("sp_birthdate", "spouse_birthdate", preprocess_date, [birthdate_validator], flat_file_size=8, description="Spouse birthdate, format MMDDCCYY")
    sp_gender = EnrollmentRecordField("sp_gender", "spouse_gender", preprocess_string, [gender_validator], flat_file_size=1)
    sp_ssn = EnrollmentRecordField("sp_ssn", "spouse_ssn", preprocess_numbers, [ssn_validator], flat_file_size=9, description="")
    sp_street = EnrollmentRecordField("sp_street", "spouse_street", preprocess_string, [], flat_file_size=29, description="Spouse street address")
    sp_street2 = EnrollmentRecordField("sp_street2", "spouse_street2", preprocess_string, [], flat_file_size=29, description="Spouse street address 2")
    sp_city = EnrollmentRecordField("sp_city", "spouse_city", preprocess_string, [], flat_file_size=14, description="Spouse city")
    sp_state = EnrollmentRecordField("sp_state", "spouse_state", preprocess_string, [state_validator], flat_file_size=2, description="2 character Spouse statecode")
    sp_zipcode = EnrollmentRecordField("sp_zipcode", "spouse_zipcode", preprocess_zip, [zip_validator], flat_file_size=9, description="Spouse zipcode, up to 9 characters")
    sp_phone = EnrollmentRecordField("sp_phone", "spouse_phone", preprocess_string, [], flat_file_size=10, description="Spouse phone number, format NNNNNNNNNN")
    sp_coverage = EnrollmentRecordField("sp_coverage", "spouse_coverage", preprocess_string, [coverage_validator], flat_file_size=6, description="")
    sp_premium = EnrollmentRecordField("sp_premium", "spouse_premium", preprocess_string, [premium_validator], flat_file_size=6, description="")

    #Optional Fields
    actively_at_work = EnrollmentRecordField("actively_at_work", "actively_at_work", preprocess_string, [question_answered_validator], flat_file_size=1, description="")
    emp_email = EnrollmentRecordField("emp_email", "employee_email", preprocess_string, [], flat_file_size=40, description="")
    emp_date_of_hire = EnrollmentRecordField("emp_date_of_hire", "employee_date_of_hire", preprocess_date, [], flat_file_size=8, description="")
    emp_height_inches = EnrollmentRecordField("emp_height_inches", "employee_height_inches", preprocess_numbers, [], flat_file_size=2, description="")
    emp_weight_pounds = EnrollmentRecordField("emp_weight_pounds", "employee_weight_pounds", preprocess_numbers, [], flat_file_size=3, description="")
    emp_smoker = EnrollmentRecordField("emp_smoker", "employee_smoker", preprocess_string, [question_answered_validator], flat_file_size=1, description="")
    sp_street = EnrollmentRecordField("sp_street", "spouse_street", preprocess_string, [], flat_file_size=29, description="")
    sp_street2 = EnrollmentRecordField("sp_street2", "spouse_street2", preprocess_string, [], flat_file_size=29, description="")
    sp_city = EnrollmentRecordField("sp_city", "spouse_city", preprocess_string, [], flat_file_size=14, description="")
    sp_state = EnrollmentRecordField("sp_state", "spouse_state", preprocess_string, [state_validator], flat_file_size=2, description="")
    sp_zipcode = EnrollmentRecordField("sp_zipcode", "spouse_zipcode", preprocess_zip, [zip_validator], flat_file_size=9, description="")
    sp_phone = EnrollmentRecordField("sp_phone", "spouse_phone", preprocess_numbers, [], flat_file_size=10, description="")
    existing_insurance = EnrollmentRecordField("existing_insurance", "existing_insurance", preprocess_string, [question_answered_validator], flat_file_size=1, description="")
    replacing_insurance = EnrollmentRecordField("replacing_insurance", "replacing_insurance", preprocess_string, [question_answered_validator], flat_file_size=1, description="")
    sp_treated_6_months = EnrollmentRecordField("sp_treated_6_months", "sp_treated_6_months", preprocess_string, [question_answered_validator], flat_file_size=1, description="")
    sp_disabled_6_months = EnrollmentRecordField("sp_disabled_6_months", "sp_disabled_6_months", preprocess_string, [question_answered_validator], flat_file_size=1, description="")
    replacement_read_aloud = EnrollmentRecordField("replacement_read_aloud", "replacement_read_aloud", preprocess_string, [question_answered_validator], flat_file_size=1, description="")
    replacement_is_terminating = EnrollmentRecordField("replacement_is_terminating", "replacement_is_terminating", preprocess_string, [question_answered_validator], flat_file_size=1, description="")
    replacement_using_funds = EnrollmentRecordField("replacement_using_funds", "replacement_using_funds", preprocess_string, [question_answered_validator], flat_file_size=1, description="")
    replacement_policy1_name = EnrollmentRecordField("replacement_policy1_name", "replacement_policy1_name", preprocess_string, [], flat_file_size=20, description="")
    replacement_policy1_number = EnrollmentRecordField("replacement_policy1_number", "replacement_policy1_number", preprocess_string, [], flat_file_size=10, description="")
    replacement_policy1_insured = EnrollmentRecordField("replacement_policy1_insured", "replacement_policy1_insured", preprocess_string, [], flat_file_size=20, description="")
    replacement_policy1_replaced_or_financing = EnrollmentRecordField("replacement_policy1_replaced_or_financing", "replacement_policy1_replaced_or_financing", preprocess_string, [], flat_file_size=1, description="")
    replacement_policy1_reason = EnrollmentRecordField("replacement_policy1_reason", "replacement_policy1_reason", preprocess_string, [], flat_file_size=70, description="")
    emp_bene_name = EnrollmentRecordField("emp_bene_name", "employee_bene_name", preprocess_string, [], flat_file_size=40, description="")
    emp_bene_birthdate = EnrollmentRecordField("emp_bene_birthdate", "employee_bene_birthdate", preprocess_date, [birthdate_validator], flat_file_size=8, description="")
    emp_bene_relationship = EnrollmentRecordField("emp_bene_relationship", "employee_bene_relationship", preprocess_string, [], flat_file_size=15, description="")
    emp_bene_ssn = EnrollmentRecordField("emp_bene_ssn", "employee_bene_ssn", preprocess_numbers, [ssn_validator], flat_file_size=9, description="")
    sp_bene_name = EnrollmentRecordField("sp_bene_name", "spouse_bene_name", preprocess_string, [], flat_file_size=40, description="")
    sp_bene_birthdate = EnrollmentRecordField("sp_bene_birthdate", "spouse_bene_birthdate", preprocess_date, [], flat_file_size=8, description="")
    sp_bene_relationship = EnrollmentRecordField("sp_bene_relationship", "spouse_bene_relationship", preprocess_string, [], flat_file_size=15, description="")
    sp_bene_ssn = EnrollmentRecordField("sp_bene_ssn", "spouse_bene_ssn", preprocess_numbers, [ssn_validator], flat_file_size=9, description="")
    emp_cont_bene_name = EnrollmentRecordField("emp_cont_bene_name", "employee_cont_bene_name", preprocess_string, [], flat_file_size=40, description="")
    emp_cont_bene_birthdate = EnrollmentRecordField("emp_cont_bene_birthdate", "employee_cont_bene_birthdate", preprocess_date, [birthdate_validator], flat_file_size=8, description="")
    emp_cont_bene_relationship = EnrollmentRecordField("emp_cont_bene_relationship", "employee_cont_bene_relationship", preprocess_string, [], flat_file_size=15, description="")
    emp_cont_bene_ssn = EnrollmentRecordField("emp_cont_bene_ssn", "employee_cont_bene_ssn", preprocess_numbers, [ssn_validator], flat_file_size=9, description="")
    sp_cont_bene_name = EnrollmentRecordField("sp_cont_bene_name", "spouse_cont_bene_name", preprocess_string, [], flat_file_size=40, description="")
    sp_cont_bene_birthdate = EnrollmentRecordField("sp_cont_bene_birthdate", "spouse_cont_bene_birthdate", preprocess_date, [birthdate_validator], flat_file_size=8, description="")
    sp_cont_bene_relationship = EnrollmentRecordField("sp_cont_bene_relationship", "spouse_cont_bene_relationship", preprocess_string, [], flat_file_size=15, description="")
    sp_cont_bene_ssn = EnrollmentRecordField("sp_cont_bene_ssn", "spouse_cont_bene_ssn", preprocess_numbers, [ssn_validator], flat_file_size=9, description="")

    # Signing Information
    emp_sig_txt = EnrollmentRecordField("emp_sig_txt", "employee_sig_txt", preprocess_string, [required_validator], flat_file_size=70, description="")
    application_date = EnrollmentRecordField("application_date", "application_date", preprocess_date, [required_validator], flat_file_size=8, description="")
    time_stamp = EnrollmentRecordField("time_stamp", "time_stamp", preprocess_date, [required_validator], flat_file_size=6, description="")
    signed_at_city = EnrollmentRecordField("signed_at_city", "signed_at_city", preprocess_string, [required_validator], flat_file_size=15, description="")
    signed_at_state = EnrollmentRecordField("signed_at_state", "signed_at_state", preprocess_string, [required_validator, state_validator], flat_file_size=2, description="")
    agent_name = EnrollmentRecordField("agent_name", "agent_name", preprocess_string, [required_validator], flat_file_size=15, description="")
    agent_code = EnrollmentRecordField("agent_code", "agent_code", preprocess_string, [required_validator], flat_file_size=8, description="")
    agent_sig_txt = EnrollmentRecordField("agent_sig_txt", "agent_sig_txt", preprocess_string, [required_validator], flat_file_size=70, description="")

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

    sp_premium.add_validator(
            RequiredIfAnyInGroupValidator(
                [sp_coverage],
                "Spouse premium is required if spouse coverage is provided"
            )
        )

    all_fields = [
        # Case data
        user_token,
        case_token,
        product_code,
        payment_mode,

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

        #Optional fields
        actively_at_work,
        emp_email,
        emp_date_of_hire,
        emp_height_inches,
        emp_weight_pounds,
        emp_smoker,
        sp_street,
        sp_street2,
        sp_city,
        sp_state,
        sp_zipcode,
        sp_phone,
        existing_insurance,
        replacing_insurance,
        sp_treated_6_months,
        sp_disabled_6_months,
        replacement_read_aloud,
        replacement_is_terminating,
        replacement_using_funds,
        replacement_policy1_name,
        replacement_policy1_number,
        replacement_policy1_insured,
        replacement_policy1_replaced_or_financing,
        replacement_policy1_reason,
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

    # Child data
    MAX_CHILDREN = 6
    for num in range(1, MAX_CHILDREN + 1):
        child_first = EnrollmentRecordField('ch{}_first'.format(num),
                                        'child{}_first'.format(num),
                                        preprocess_string, [], flat_file_size=14, description="")
        child_last = EnrollmentRecordField('ch{}_last'.format(num),
                                       'child{}_last'.format(num),
                                       preprocess_string, [], flat_file_size=20, description="")
        child_birthdate = EnrollmentRecordField('ch{}_birthdate'.format(num),
                                            'child{}_birthdate'.format(num),
                                            preprocess_date,
                                            [birthdate_validator], flat_file_size=8, description="")
        child_gender = EnrollmentRecordField('ch{}_gender'.format(num),
                                            'child{}_gender'.format(num),
                                            preprocess_string,
                                            [gender_validator], flat_file_size=1, description="")
        child_ssn = EnrollmentRecordField('ch{}_ssn'.format(num),
                                            'child{}_ssn'.format(num),
                                            preprocess_numbers,
                                            [ssn_validator], flat_file_size=9, description="")
        child_coverage = EnrollmentRecordField('ch{}_coverage'.format(num),
                                            'child{}_coverage'.format(num),
                                            preprocess_string,
                                            [coverage_validator], flat_file_size=6, description="")
        child_premium = EnrollmentRecordField('ch{}_premium'.format(num),
                                            'child{}_premium'.format(num),
                                            preprocess_string,
                                            [premium_validator], flat_file_size=6, description="")

        child_premium.add_validator(RequiredIfAnyInGroupValidator([child_coverage],
                                        "child_premium is required if child_coverage is provided")
                                    )
        child_coverage.add_validator(RequiredIfAnyInGroupValidator([child_premium],
                                        "child_coverage is required if child_premium is provided")
                                    )
        all_fields += [child_first, child_last, child_birthdate, child_gender, child_ssn, child_coverage, child_premium]

    for q_num in range(1, MAX_QUESTIONS + 1):
        questions = []
        emp_question = EnrollmentRecordField("emp_question_{}_answer".format(q_num),
                                             "employee_question_{}_answer".format(q_num),
                                             preprocess_string,
                                             [question_answered_validator],
                                             flat_file_size=1, description=""
                                             )
        sp_question = EnrollmentRecordField("sp_question_{}_answer".format(q_num),
                                            "spouse_question_{}_answer".format(q_num),
                                            preprocess_string,
                                            [question_answered_validator],
                                            flat_file_size=1, description=""
                                            )
        for num in range(1, MAX_CHILDREN + 1):
            ch_question = EnrollmentRecordField("ch{}_question_{}_answer".format(num, q_num),
                                                "child{}_question_{}_answer".format(num, q_num),
                                                preprocess_string,
                                                [question_answered_validator],
                                                flat_file_size=1, description=""
                                                )
            questions += [ch_question]
        questions += [emp_question, sp_question]
        all_fields += questions


    def __init__(self):
        self.errors = []
        self.valid_data = []

    def process_records(self, records, case):

        self.validate_data_keys(records)
        # Don't do any more processing if missing important data_keys
        if self.errors:
            return

        preprocessed_records = (self.preprocess_record(record) for record in records)
        for record in preprocessed_records:
            validation_tests = [
                lambda: self.validate_record(record),
                lambda: self.validate_statecode(record),
                lambda: self.validate_questions(record),
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
            case = self.case_service.get_case_by_token(record['case_token'])
        else:
            case = default_case

        if not case:
            self.error_record_field("invalid_case",
                                    "A valid case could not be found for the current information",
                                    "case_token",
                                    record)
            return False

        # Store the case ID in the record
        record['case_id'] = case.id

        # TODO: Validate case is enrolling

        return True


    def validate_statecode(self, record):
        if not self.product_service.is_valid_statecode_for_product(record.get("product_code"), record.get("signed_at_state")):
            self.error_record_field("invalid_state_for_product",
                                    "Provided 'signed at state' is invalid for this product.",
                                    "signed_at_state",
                                    record)
            return False
        return True

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

    def validate_questions(self, record):
        is_valid = True

        for applicant_abbr, applicant_type in [("emp", "employee"), ("sp", "spouse"), ("ch", "child")]:
            required_count = self.product_service.get_num_health_questions(
                record.get("product_code"),
                record['signed_at_state'],
                applicant_type
            )

            if applicant_type == "employee":
                is_valid = self._add_error_if_wrong_question_count(record, required_count, applicant_abbr)
            elif applicant_type == "spouse":
                if not self._does_record_have_spouse(record):
                    continue
                is_valid = self._add_error_if_wrong_question_count(record, required_count, applicant_abbr)
            else:
                valid_child_nums = [n for n in range(1, self.MAX_CHILDREN + 1)
                                    if self._does_record_have_child(record, n)
                ]
                for num in valid_child_nums:
                    child_prefix = "{}{}".format(applicant_abbr, num)
                    is_valid = self._add_error_if_wrong_question_count(record, required_count, child_prefix)

        return is_valid

    def _does_record_have_spouse(self, record):
        return record.get("sp_first")

    def _does_record_have_child(self, record, num):
        return record.get("ch{}_first".format(num))

    def _add_error_if_wrong_question_count(self, record, required_count, applicant_prefix):
        if self._count_valid_questions(record, applicant_prefix) is not required_count:
            self.error_record_field(type="invalid_questions",
                                    message="Incorrect number of questions supplied; {} should have {} questions".format(applicant_prefix, required_count),
                                    field_name="{}_questions".format(applicant_prefix),
                                    data=record)
            return False

        return True

    def _count_valid_questions(self, record, applicant_prefix):
        actual_count = 0
        for q_num in range(1, self.MAX_QUESTIONS + 1):
            question = record.get("{}_question_{}_answer".format(applicant_prefix, q_num))
            if not question:
                # If there isn't a question here, we have reached the number of questions for this person
                break

            # Otherwise, add this question to the count
            actual_count += 1

        return actual_count
