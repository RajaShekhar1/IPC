from flask import abort, render_template

import requests
import mandrill

from taa import mandrill_flask, app
from taa.core import TAAFormError, db, DBService
from taa.services import RequiredFeature
from taa.services.enrollments.models import EnrollmentLog

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

    def process_enrollment_import_request(self, data, data_format, data_source=None, auth_token=None, case_token=None):
        self.authenticate_user(auth_token)

        self.validate_records(case_token, data, data_format)

        self.log_request(data, data_source, auth_token, case_token)

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
            self._add_error(error["type"], error["field_name"], error['message'], error['record'], error['record_num'])

    def log_request(self, data, data_source, auth_token, case_token):
        if data_source == "dropbox":
            source = EnrollmentLog.SUBMIT_SOURCE_DROPBOX
        else:
            source = EnrollmentLog.SUBMIT_SOURCE_API
        enrollment_log_service = EnrollmentLogService()
        matching_data_hash = enrollment_log_service.lookup_hash(data.getvalue())
        if matching_data_hash and not self.errors and not app.config.get('ALLOW_DUPLICATE_SUBMISSION'):
            self._add_error("duplicate_upload", "", "This upload was previously uploaded on {}".format(matching_data_hash.timestamp), [], 0)
            return
        enrollment_log_service.create_new_log(
            source = source,
            num_processed = self.get_num_processed(),
            num_errors = len(self.errors),
            hash_data = data.getvalue(),
            auth_token = auth_token,
            case_token = case_token
        )

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

    def _add_error(self, type, fields, message, record, record_num):
        error = EnrollmentImportError(type, fields, message, record, record_num)
        self.errors.append(error)
        return error

    def is_success(self):
        return not self.errors

    def is_error(self):
        return not self.is_success()

    def get_errors(self):
        return self.errors

    def _status_email_body(self):
        from datetime import datetime
        if self.is_success():
            return render_template('emails/enrollment_upload_email.html',
                                   errors=[],
                                   num_processed=self.get_num_processed(),
                                   user=self.get_status_email_name(),
                                   timestamp=datetime.now()
                                   )
        else:
            errors = [{"type": e.get_type(), "fields": e.get_fields(), "message": e.get_message()} for e in self.get_errors()]
            return render_template('emails/enrollment_upload_email.html',
                                   errors=errors,
                                   num_processed=self.get_num_processed(),
                                   user=self.get_status_email_name(),
                                   timestamp=datetime.now()
                                   )

    def send_status_email(self):
        status_email = self.get_status_email()
        if not status_email:
            return

        if self.errors:
            email_subject = "Your recent submission to 5Star Enrollment failed."
        else:
            email_subject = "Your recent submission to 5Star Enrollment succeeded."

        self._send_email(
            from_email="support@5Starenroll.com",
            from_name="5Star Enrollment",
            to_email=status_email,
            to_name=self.get_status_email_name(),
            subject=email_subject,
            body=self._status_email_body()
            )

    def get_status_email(self):
        user = self.get_status_user()
        if not user:
            return None

        return user.email

    def get_status_email_name(self):
        user = self.get_status_user()
        if not user:
            return None

        return user.full_name

    def get_status_user(self):
        """Who to send errors to. Pull it from the auth_token"""
        if not self.processed_data:
            return None

        return self.get_user(self.processed_data[0].get('user_token'))

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
        elif data_format == "json":
            return


class EnrollmentImportError(object):
    def __init__(self, type, fields, message, record, record_num):
        self.type = type
        self.fields = [fields]
        self.message = message
        self.record = record
        self.record_num = record_num

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
        return {'type':self.type, 'message': self.get_message(), 'fields':self.fields, 'record_num':self.record_num}

class EnrollmentLogService(DBService):
    __model__ = EnrollmentLog

    def __init__(self):
        pass

    def create_new_log(self, source, num_processed, num_errors, hash_data, auth_token=None, case_token=None):
        self.create(**dict(
            source=source,
            auth_token=auth_token,
            case_token=case_token,
            num_processed=num_processed,
            num_errors=num_errors,
            log_hash=self.generate_hash(hash_data)
        ))
        db.session.commit()

    def lookup_hash(self, hash_data):
        search_hash = self.generate_hash(hash_data)
        return db.session.query(EnrollmentLog
        ).filter_by(log_hash=search_hash
        ).first()

    def generate_hash(self, data):
        import hashlib
        new_hash = hashlib.md5(data).hexdigest()
        return new_hash
