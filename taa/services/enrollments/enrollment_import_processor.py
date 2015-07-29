from flask import abort, render_template

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
        self.num_processed = 1
        self.enrollment_record_parser = None
        self.enrolling_agent = None

    def process_enrollment_import_request(self, data, data_format, auth_token=None, case_token=None):
        # Instantiate new record parser

        self.enrollment_record_parser = self.enrollment_record_parser_service()

        self.authenticate_user(auth_token)
        processed_data = self.extract_dictionaries(data, data_format)
        case_from_token = self.get_case(case_token)
        self.enrolling_agent = self.get_user(processed_data[0].get("user_token"))

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
            enrollment_record = self.save_validated_data(standardized_data, case, record)

            # Submit the enrollment
            self.submit_imported_enrollment.submit_enrollment(enrollment_record)

        db.session.commit()

    def save_validated_data(self, standardized_data, case, raw_data):
        agent = case.owner_agent
        return self.enrollment_service.save_enrollment_data(standardized_data, case, None, agent, received_data=raw_data)

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
        errors = self.get_errors()
        if errors:
            agent_email = self.enrolling_agent.email
            agent_name = "{} {}".format(self.enrolling_agent.first, self.enrolling_agent.last).capitalize()
            email_body = self._error_email_body()
            self._send_email(
                from_email="errors@5Star.com",
                from_name="5Star Enrollment",
                to_email=agent_email,
                to_name=agent_name,
                subject="Your recent upload to 5Star Enrollment",
                body=email_body
                )

    def process_wizard_enrollment_request(self, case_id, auth_token=None):
        pass

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
