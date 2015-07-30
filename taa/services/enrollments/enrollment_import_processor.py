from flask import abort, render_template

import requests
import mandrill

from taa import mandrill_flask
from taa.core import TAAFormError, db
from taa.services import RequiredFeature

class EnrollmentProcessor(object):

    api_token_service = RequiredFeature("ApiTokenService")
    case_service = RequiredFeature("CaseService")

    enrollment_import_service = RequiredFeature("EnrollmentImportService")
    enrollment_service = RequiredFeature("EnrollmentApplicationService")
    enrollment_record_parser_service = RequiredFeature("EnrollmentRecordParser")
    enrollment_submission = RequiredFeature("EnrollmentSubmissionService")

    file_import_service = RequiredFeature("FileImportService")
    user_service = RequiredFeature("UserService")

    def __init__(self):
        self.errors = []
        self.enrollment_record_parser = None
        self.enrolling_agent = None
        self.processed_data = []

    def process_enrollment_import_request(self, data, data_format, auth_token=None, case_token=None):

        self.authenticate_user(auth_token)

        self.validate_records(case_token, data, data_format)

        if self.errors:
            self.raise_error_exception()

        self.submit_validated_data()

    def validate_records(self, case_token, data, data_format):
        self.processed_data = self.extract_dictionaries(data, data_format)
        case_from_token = self.get_case(case_token)
        # Process all records
        self.enrollment_record_parser = self.enrollment_record_parser_service()
        self.enrollment_record_parser.process_records(self.processed_data, case_from_token)
        for error in self.enrollment_record_parser.errors:
            self._add_error(error["type"], error["field_name"], error['message'])

    def submit_validated_data(self):
        for record in self.get_valid_data():
            # Standardize
            standardized_data = self.enrollment_import_service.standardize_imported_data(record, method='api_import')

            # Save
            enrollment_record = self.save_validated_data(standardized_data, record)

            # Submit the enrollment
            self.enrollment_submission.submit_imported_enrollment(enrollment_record)
        db.session.commit()

    def save_validated_data(self, standardized_data, raw_data):
        case = self.case_service.get(standardized_data['case_id'])
        return self.enrollment_service.save_enrollment_data(
            standardized_data,
            case,
            None,
            case.owner_agent,
            received_data=raw_data,
        )

    def raise_error_exception(self):
        raise TAAFormError(errors=[e.to_json() for e in self.errors])

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
        return len(self.processed_data)

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

    def _error_email_body(self):
        if self.is_success():
            return render_template('emails/enrollment_upload_email.html',
                                   errors=[]
                                   )
        else:
            errors = [{"type": e.get_type(), "fields": e.get_fields(), "message": e.get_message()} for e in self.get_errors()]
            return render_template('emails/enrollment_upload_email.html',
                                   errors=errors
                                   )

    def send_errors_email(self):

        error_email = self.get_error_email()
        if not error_email or not self.get_errors():
            return

        self._send_email(
            from_email="support@5Starenroll.com",
            from_name="5Star Enrollment",
            to_email=error_email,
            to_name=self.get_error_email_name(),
            subject="Errors importing records to 5Star Enrollment",
            body=self._error_email_body()
            )

    def get_error_email(self):
        user = self.get_error_user()
        if not user:
            return None

        return user.email

    def get_error_email_name(self):
        user = self.get_error_user()
        if not user:
            return None

        return user.name

    def get_error_user(self):
        """Who to send errors to. Pull it from the auth_token"""
        if not self.processed_data:
            return None

        return self.get_user(self.processed_data[0].get('auth_token'))

    def _send_email(self, from_email, from_name, to_email, to_name, subject,
                    body):
        try:
            mandrill_flask.send_email(
                to=[{'email': to_email, 'name': to_name}],
                from_email=from_email,
                from_name=from_name,
                subject=subject,
                html=body,
                auto_text=True,
            )
        except mandrill.Error as e:
            print("Exception sending email: %s - %s; to %s"%(e.__class__, e, to_email))
            return False
        except requests.exceptions.HTTPError as e:
            print("Exception sending email: %s - %s; to %s"%(e.__class__, e, to_email))
            return False
        except Exception as e:
            print "Exception sending email: %s - %s"%(e.__class__, e)
            return False

        return True
    
    def get_case(self, case_token):
        if case_token:
            case = self.case_service.get_case_for_token(case_token)
        else:
            case = None
        return case

    def get_user(self, user_token):
        if user_token:
            user = self.api_token_service.get_sp_user_by_token(user_token)
        else:
            user = None
        return user

    def extract_dictionaries(self, data, data_format):
        if data_format == "csv":
            result = self.file_import_service.process_delimited_file_stream(data)
            if result.has_error():
                raise TAAFormError(result.get_error_message())
            rows = result.get_rows()
            return rows
        elif data_format == "flat":
            result = self.file_import_service.process_flat_file_stream(data)
            if result.has_error():
                self.errors += result.get_errors()
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
