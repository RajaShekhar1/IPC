from datetime import datetime
import hashlib

from flask import abort, render_template
import requests

from taa import app
from taa.core import TAAFormError, db, DBService
from taa.services import RequiredFeature, LookupService
from taa.services.enrollments.models import EnrollmentImportBatch, EnrollmentImportBatchItem


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

    def process_enrollment_import_request(self, data, data_format, data_source=None, auth_token=None, case_token=None, user_href=None, filename=None):

        self.authenticate_user(auth_token)
        self.validate_records(case_token, data, data_format)
        enrollment_batch = self.create_enrollment_batch(data, data_source, auth_token, case_token, user_href, filename)

        if self.errors:
            self.raise_error_exception()

        # Add records to batch and queue for submission
        self.add_enrollments_to_batch(enrollment_batch)
        self.enrollment_submission.submit_import_enrollments(enrollment_batch)

    def validate_records(self, case_token, data, data_format):
        self.processed_data = self.extract_dictionaries(data, data_format)
        case_from_token = self.get_case(case_token)
        # Process all records
        self.enrollment_record_parser = self.enrollment_record_parser_service()
        self.enrollment_record_parser.process_records(self.processed_data, case_from_token)
        for error in self.enrollment_record_parser.errors:
            self._add_error(error["type"], error["field_name"], error['message'], error['record'], error['record_num'])

    def create_enrollment_batch(self, file_obj, data_source, auth_token, case_token, user_href, filename):
        if data_source == "dropbox":
            source = EnrollmentImportBatch.SUBMIT_SOURCE_DROPBOX
        else:
            source = EnrollmentImportBatch.SUBMIT_SOURCE_API

        file_obj.seek(0)
        data = file_obj.read()
        enrollment_batch_service = EnrollmentImportBatchService()
        matching_data_hash = enrollment_batch_service.lookup_hash(data)
        if matching_data_hash and not self.errors and not app.config.get('ALLOW_DUPLICATE_SUBMISSION'):
            self._add_error("duplicate_upload", "", "This upload was previously uploaded on {}".format(matching_data_hash.timestamp), [], 0)
            return None

        return enrollment_batch_service.create_new_batch(
            source = source,
            num_processed = self.get_num_processed(),
            num_errors = len(self.errors),
            hash_data = data,
            auth_token = auth_token,
            case_token = case_token,
            user_href=user_href,
            filename=filename,
        )

    def add_enrollments_to_batch(self, enrollment_batch):
        enrollment_records = self.generate_enrollment_records()
        EnrollmentImportBatchService().add_enrollments_to_batch(enrollment_batch, enrollment_records)

    def generate_enrollment_records(self):
        enrollment_records = []
        for record in self.get_valid_data():
            # Standardize the data
            standardized_data = self.enrollment_import_service.standardize_imported_data(record, method='api_import')

            # Save
            enrollment_record = self.save_validated_data(standardized_data, record)
            enrollment_records.append(enrollment_record)

        return enrollment_records

    def save_validated_data(self, standardized_data, raw_data):
        case = self.case_service.get(standardized_data['case_id'])

        # We want to merge multiple enrollments to a single "person" in the census data via SSN match.
        census_record = self.find_matching_census_record(case, standardized_data)

        return self.enrollment_service.save_enrollment_data(
            standardized_data,
            case,
            census_record,
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

    def _status_email_body(self, user_name, filename=None):
        from datetime import datetime
        if self.is_success():
            return render_template('emails/enrollment_upload_email.html',
                                   errors=[],
                                   num_processed=self.get_num_processed(),
                                   user=user_name,
                                   timestamp=datetime.now().strftime("%m/%d/%Y %H:%M:%S"),
                                   filename=filename,
                                   )
        else:
            errors = [{"type": e.get_type(),
                       "fields": e.get_fields(),
                       "message": e.get_message(),
                       "record_num": e.record_num,
                       } for e in self.get_errors()]
            return render_template('emails/enrollment_upload_email.html',
                                   errors=errors,
                                   num_processed=self.get_num_processed(),
                                   user=user_name,
                                   timestamp=datetime.now().strftime("%m/%d/%Y %H:%M:%S"),
                                   filename=filename,
                                   )

    def send_status_email(self, user_href=None, filename=None):
        if not user_href:
            # We can't send the email if we don't know the user.
            return

        if self.errors:
            email_subject = "Your recent submission to 5Star Enrollment failed."
        else:
            email_subject = "Your recent submission to 5Star Enrollment succeeded."

        user_name = self.get_status_email_name(user_href)

        self._send_email(
            from_email="5Star Enrollment <support@5StarEnroll.com>",
            to_email=self.get_status_email(user_href),
            to_name=user_name,
            subject=email_subject,
            body=self._status_email_body(user_name, filename)
            )

    def get_status_email(self, user_href):
        user = self.get_status_user(user_href)
        if not user:
            return None

        return user.email

    def get_status_email_name(self, user_href):
        user = self.get_status_user(user_href)
        if not user:
            return None

        return user.full_name

    def get_status_user(self, user_href):
        """Who to send errors to. Pull it from the auth_token"""
        return self.user_service.get_stormpath_user_by_href(user_href)

    def _send_email(self, from_email, to_email, to_name, subject, body):
        mailer = LookupService('MailerService')
        try:
            mailer.send_email(
                to=["{} <{}>".format(to_name, to_email)],
                from_email=from_email,
                subject=subject,
                html=body,
                track_clicks=False,
            )
        except mailer.Error as e:
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

    def find_matching_census_record(self, case, data):
        emp_ssn = data['employee']['ssn']
        matching = self.case_service.get_census_records(case, filter_ssn=emp_ssn)
        if matching:
            return matching[0]
        else:
            return None

    def send_generic_error_email(self, user_href):
        if not user_href:
            return

        body_message = """
            An unforeseen error has prevented processing of your file submission on {},
            likely due to unexpected characters in the file header or body.
            Please remove any hidden/non-printable characters from the file and resubmit.
            Contact 5Star Operations team if you need further assistance. Thank you.
        """.format(datetime.now().strftime("%F %T"))

        errors = [{"type": "Generic",
                   "fields": "",
                   "message": "Error reading file",
                   "record_num": "",
                   }
      ]

        user_name = self.get_status_email_name(user_href)

        body = render_template('emails/enrollment_upload_email.html',
                           errors=errors,
                           num_processed=0,
                           user=user_name,
                           timestamp=datetime.now()
                           )

    
        self._send_email(
            from_email="5Star Enrollment <support@5StarEnroll.com>",
            to_email=self.get_status_email(user_href),
            to_name=user_name,
            subject="Your recent submission to 5Star Enrollment failed",
            body=body,
        )


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


class EnrollmentImportBatchService(DBService):
    __model__ = EnrollmentImportBatch

    def get_batch_items(self, batch):
        return db.session.query(EnrollmentImportBatchItem).with_parent(batch
                        ).order_by(db.desc(EnrollmentImportBatchItem.processed_time)
                        )

    def get_records_needing_submission(self, batch):
        return db.session.query(EnrollmentImportBatchItem
                 ).with_parent(batch
                 ).filter(EnrollmentImportBatchItem.status != EnrollmentImportBatchItem.STATUS_SUCCESS
                 )

    def create_new_batch(self, source, num_processed, num_errors, hash_data, auth_token=None, case_token=None, user_href=None, filename=None):
        return self.create(**dict(
            source=source,
            auth_token=auth_token,
            case_token=case_token,
            num_processed=num_processed,
            num_errors=num_errors,
            log_hash=self.generate_hash(hash_data),
            timestamp=datetime.now(),
            filename=filename,
            user_href=user_href,
        ))

    def add_enrollments_to_batch(self, batch, enrollment_records):
        for record in enrollment_records:
            EnrollmentImportBatchItemService().create_batch_item(batch, record)

        db.session.commit()

    def lookup_hash(self, hash_data):

        search_hash = self.generate_hash(hash_data)
        return db.session.query(EnrollmentImportBatch
        ).filter_by(log_hash=search_hash
        ).first()

    def generate_hash(self, data):
        new_hash = hashlib.md5(data).hexdigest()
        return new_hash

    def delete_batch(self, batch):
        from taa.services.enrollments import EnrollmentApplicationService
        enrollment_service = EnrollmentApplicationService()
        # Remove all the enrollments for each batch item, then remove the items, and last the batch itself.
        for item in batch.batch_items:
            enrollment_service.delete_enrollment_record(item.enrollment_record)
            EnrollmentImportBatchItemService().delete(item)

        EnrollmentImportBatchService().delete(batch)

        db.session.commit()

class EnrollmentImportBatchItemService(DBService):
    __model__ = EnrollmentImportBatchItem

    def create_batch_item(self, batch, enrollment_record):
        return self.create(**dict(
            enrollment_batch=batch,
            enrollment_record=enrollment_record,
            status=self.__model__.STATUS_QUEUED,

        ))

    def delete_for_enrollment(self, enrollment_application):
        batch_item = self.first(enrollment_record_id=enrollment_application.id)
        if not batch_item:
            return

        return self.delete(batch_item)
