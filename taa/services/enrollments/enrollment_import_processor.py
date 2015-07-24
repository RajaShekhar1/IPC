from flask import abort

from taa.core import TAAFormError
from taa.services import RequiredFeature

class EnrollmentProcessor(object):
    api_token_service = RequiredFeature("ApiTokenService")
    case_service = RequiredFeature("CaseService")
    enrollment_service = RequiredFeature("EnrollmentApplicationService")
    enrollment_record_parser = RequiredFeature("EnrollmentRecordParser")
    user_service = RequiredFeature("UserService")
    file_import_service = RequiredFeature("FileImportService")
    enrollment_import_service = RequiredFeature("EnrollmentImportService")

    def __init__(self):
        self.errors = []
        self.num_processed = 1

    def process_enrollment_import_request(self, data, data_format, auth_token=None, case_token=None):

        self.authenticate_user(auth_token)

        case = self.get_case(case_token)

        processed_data = self.extract_dictionaries(data, data_format)

        # Process all records
        self.enrollment_record_parser.process_records(processed_data, case)

        for error in self.enrollment_record_parser.errors:
            self._add_error(error["type"], error["field_name"], error['message'])

        if self.errors:
            raise TAAFormError(errors=[e.to_json() for e in self.errors])

        for cur_data in self.get_valid_data():
            wizard_data = self.enrollment_import_service.standardize_imported_data(cur_data)
            case = self.get_case(cur_data.get("case_token"))
            agent = case.owner_agent

            self.enrollment_service.save_enrollment_data(wizard_data, case, None, agent)

    def get_valid_data(self):
        return self.enrollment_record_parser.get_valid_data()

    def authenticate_user(self, auth_token):
        is_valid_user = (not auth_token
                         and self.user_service.can_current_user_submit_enrollments()
                         )
        is_valid_auth_token = (auth_token
                               and self.api_token_service.is_valid_token(auth_token)
                               )
        if not is_valid_user and not is_valid_auth_token:
            abort(401, "Missing or invalid authentication token")

    def get_num_processed(self):
        return self.num_processed

    def _add_error(self, type, fields, message):
        error = EnrollmentImportError(type, fields, message)
        self.errors.append(error)
        return error

    def is_success(self):
        return not self.errors

    def is_error(self):
        return not self.is_success()

    def get_errors(self):
        return self.errors

    def process_wizard_enrollment_request(self, case_id, auth_token=None):
        pass

    def get_case(self, case_token):
        if case_token:
            case = self.case_service.get_case_for_token(case_token)
        else:
            case = None
        if not self.case_service.is_case_enrolling(case):
            self._add_error("invalid_case", [], "Case is not enrolling")
            raise TAAFormError(errors=[e.to_json() for e in self.errors])
        return case


    def extract_dictionaries(self, data, data_format):
        if data_format == "csv":
            result = self.file_import_service.process_delimited_file_stream(data)
            if result.has_error():
                raise TAAFormError(result.get_error_message())
            return result.get_rows()

class EnrollmentImportError(object):
    def __init__(self, type, fields, message):
        self.type = type
        self.fields = [fields]
        self.message = message

    def get_type(self):
        return self.type

    def get_fields(self):
        """
        returns a list of column names that this error refers to.
        """
        return self.fields

    def get_message(self):
        if self.message:
            return self.message
        return "Error with column: {}".format(self.type)

    def to_json(self):
        return {'type':self.type, 'message': self.get_message(), 'fields':self.fields}
