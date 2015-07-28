from flask import abort

from taa.core import TAAFormError, db
from taa.services import RequiredFeature
from taa.services.docusign.docusign_envelope import \
    EnrollmentDataWrap,\
    create_envelope_recipients,\
    create_fpp_envelope_components


class EnrollmentProcessor(object):
    api_token_service = RequiredFeature("ApiTokenService")
    case_service = RequiredFeature("CaseService")
    enrollment_import_service = RequiredFeature("EnrollmentImportService")
    enrollment_service = RequiredFeature("EnrollmentApplicationService")
    enrollment_record_parser_service = RequiredFeature("EnrollmentRecordParser")
    file_import_service = RequiredFeature("FileImportService")
    pdf_generator_service = RequiredFeature('ImagedFormGeneratorService')
    user_service = RequiredFeature("UserService")

    def __init__(self):
        self.errors = []
        self.num_processed = 1
        self.enrollment_record_parser = None

    def process_enrollment_import_request(self, data, data_format, auth_token=None, case_token=None):
        # Instantiate new record parser
        self.enrollment_record_parser = self.enrollment_record_parser_service()

        self.authenticate_user(auth_token)
        processed_data = self.extract_dictionaries(data, data_format)
        case_from_token = self.get_case(case_token)

        # Process all records
        self.enrollment_record_parser.process_records(processed_data, case_from_token)

        for error in self.enrollment_record_parser.errors:
            self._add_error(error["type"], error["field_name"], error['message'])

        if self.errors:
            raise TAAFormError(errors=[e.to_json() for e in self.errors])

        for record in self.get_valid_data():
            # Standardize
            standardized_data = self.enrollment_import_service.standardize_imported_data(record, method='api_import')
            case = self.case_service.get(standardized_data['case_id'])

            # Save
            enrollment_record = self.save_validated_data(standardized_data, case)

            # Create DocuSign tabs
            data_wrap = EnrollmentDataWrap(standardized_data,
                                           census_record=enrollment_record.census_record,
                                           case=enrollment_record.case)
            employee_recip, recipients = create_envelope_recipients(case, data_wrap)
            components = create_fpp_envelope_components(data_wrap, recipients)
            main_form = components[0]
            tabs = []
            for recipient in recipients:
                tabs += main_form.generate_tabs(recipient)

            # Create PDF
            pdf_bytes = self.pdf_generator_service.generate_form_pdf(main_form.template_id, tabs)
            with open('test_output.pdf', 'wb+') as f:
                f.write(pdf_bytes)

        db.session.commit()

    def save_validated_data(self, standardized_data, case):
        agent = case.owner_agent
        return self.enrollment_service.save_enrollment_data(standardized_data, case, None, agent)

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
        return case

    def extract_dictionaries(self, data, data_format):
        if data_format == "csv":
            result = self.file_import_service.process_delimited_file_stream(data)
            if result.has_error():
                raise TAAFormError(result.get_error_message())
            return result.get_rows()
        elif data_format == "flat":
            result = self.file_import_service.process_flat_file_stream(data)
            if result.has_error():
                raise TAAFormError(result.get_error_message())
            return result.get_data()


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
