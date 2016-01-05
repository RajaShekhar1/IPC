from datetime import datetime
import csv

from taa.helpers import UnicodeWriter
from taa.core import DBService
from taa.core import db
from taa.services import RequiredFeature
from models import CaseCensus
from census_import import CensusRecordParser

class CensusRecordService(DBService):
    __model__ = CaseCensus

    self_enrollment_link_service = RequiredFeature("SelfEnrollmentLinkService")

    def _preprocess_params(self, kwargs):
        """
        Convert the date columns to plain dates, not datetimes, so we don't
        get spurious UPDATES when merging records
        """
        from sqlalchemy.inspection import inspect
        inspector = inspect(CaseCensus)
        for c in inspector.columns:
            if (type(c.type) == db.Date and
                    c.name in kwargs and
                    isinstance(kwargs[c.name], datetime)):
                kwargs[c.name] = kwargs[c.name].date()
        return kwargs

    def export_csv(self, file, census_records):
        writer = UnicodeWriter(file)
        # Write the header row
        writer.writerow(self.get_csv_headers())
        # Write all the data
        for record in census_records:
            writer.writerow(self.get_csv_row_from_db_row(record))
        return writer

    def get_csv_headers(self):
        return [field.csv_column_name
                for field in CensusRecordParser.all_possible_fields]

    def get_csv_row_from_db_row(self, census_record):
        return [getattr(census_record, field.database_name)
         for field in CensusRecordParser.all_possible_fields
        ]

    def get_csv_row_from_dict(self, census_record):
        return [census_record[field.database_name]
                for field in CensusRecordParser.all_possible_fields]

    def format_ssn(self, ssn):
        if not len(ssn) == 9:
            return ssn
        return '{}-{}-{}'.format(ssn[:3], ssn[3:5], ssn[5:])

    def merge_census_data(self, case, file_data, replace_matching):
        """
        Updates existing records and adds new. Matches based on SSN, and depending on
         :replace_matching, will do replace matches or skip over them.
        """
        # Get existing census data indexed by SSN for matching
        existing = self.find(case_id=case.id).all()
        existing_by_ssn = {r.employee_ssn: r for r in existing}
        # Parse the uploaded file and validate it. If we are in add-only mode,
        # pass in the existing SSN dict.
        parser = CensusRecordParser()
        parser.process_file(file_data,
                            error_if_matching=(existing_by_ssn if
                                               not replace_matching else None))
        # Do the merge
        added = []
        updated = []
        for record in parser.get_valid_data():
            if record['EMP_SSN'] in existing_by_ssn:
                if replace_matching:
                    # Update Existing
                    updated_record = self.update_without_save(
                        existing_by_ssn[record['EMP_SSN']],
                        **parser.get_db_dict(record))
                    updated.append(updated_record)
                else:
                    # We are in "Add-only" mode, an error will have been added
                    # already for this record
                    continue
            else:
                # Add new census record
                added.append(
                    self.add_record_from_upload(case,
                                                **parser.get_db_dict(record)))
        # Only commit the changes if we had no errors
        if not parser.errors:
            db.session.flush()
        valid_records = added + updated
        return parser.errors, valid_records

    def replace_census_data(self, case, file_stream):
        # Process the upload before deleting the current data
        parser = CensusRecordParser()
        parser.process_file(file_stream)
        # Bail out if any errors
        if parser.errors:
            valid_records = []
            return parser.errors, valid_records
        # Delete existing records for this case
        self.remove_all_for_case(case)
        # Add all uploaded records
        valid_records = [
            self.add_record_from_upload(case, **parser.get_db_dict(record))
            for record in parser.get_valid_data()]
        db.session.flush()
        return parser.errors, valid_records

    def add_record_from_upload(self, case, **data):
        data['is_uploaded_census'] = True
        return self.add_record(case, **data)

    def add_record(self, case, **data):
        """
        Create and add to the DB session, but don't commit or flush the session
        for speed
        """
        if case:
            data['case_id'] = case.id
        else:
            # Ad-hoc record
            data['case_id'] = None
        if 'is_uploaded_census' not in data:
            data['is_uploaded_census'] = False
        # TODO: See if there are any other records that need a final "cleaning"
        # before being saved
        if 'spouse_birthdate' in data and not data['spouse_birthdate']:
            data['spouse_birthdate'] = None
        record = self.new(**data)
        db.session.add(record)
        return record

    def remove_all_for_case(self, case):
        self.find(case_id=case.id).delete()

    def update_from_enrollment(self, record, data):
        """
        Update the enrollment census with data that was potentially corrected
        while enrolling
        """

        def convert_smoker_to_y_n(val):
            if val is None:
                return ''
            return 'Y' if val else 'N'

        employee = data['employee']
        spouse = data['spouse']
        children = data['children']
        # TODO: See if there are any other records that need a final "cleaning"
        # before being saved
        if 'birthdate' in spouse and not spouse['birthdate']:
            # Ensure date is NULL in DB, not ""
            data['spouse']['birthdate'] = None
        record.employee_ssn = self.strip_ssn(employee['ssn'])
        record.employee_first = employee['first']
        record.employee_last = employee['last']
        record.employee_gender = employee['gender']
        record.employee_birthdate = employee['birthdate']
        record.employee_email = employee['email']
        record.employee_phone = employee['phone']
        record.employee_street_address = employee['address1']
        record.employee_street_address2 = employee['address2']
        record.employee_city = employee['city']
        record.employee_state = employee['state']
        record.employee_zip = employee['zip']
        record.employee_height_inches = employee['height']
        record.employee_weight_lbs = employee['weight']
        record.employee_smoker = convert_smoker_to_y_n(employee['is_smoker'])
        record.spouse_ssn = self.strip_ssn(spouse['ssn'])
        record.spouse_first = spouse['first']
        record.spouse_last = spouse['last']
        record.spouse_gender = spouse['gender']
        record.spouse_birthdate = spouse['birthdate']
        record.spouse_email = spouse['email']
        record.spouse_phone = spouse['phone']
        record.spouse_street_address = spouse['address1']
        record.spouse_street_address2 = spouse['address2']
        record.spouse_city = spouse['city']
        record.spouse_state = spouse['state']
        record.spouse_zip = spouse['zip']
        record.spouse_height_inches = spouse['height']
        record.spouse_weight_lbs = spouse['weight']
        record.spouse_smoker = convert_smoker_to_y_n(spouse['is_smoker'])
        for i, child in enumerate(children):
            child_num = i + 1
            setattr(record, 'child{}_first'.format(child_num), child['first'])
            setattr(record, 'child{}_last'.format(child_num), child['last'])
            setattr(record, 'child{}_birthdate'.format(child_num),
                    child['birthdate'])
        db.session.flush()

    def strip_ssn(self, ssn):
        return ssn.strip().replace('-','') if ssn else ''

    def serialize_with_tokens(self, case, census_records, url_root):
        # Return record_id, ssn, and self-enroll token. If self-enroll token does not exist, generate it.
        out = []
        for record in census_records:

            # Get previously generated link if available
            link = self.self_enrollment_link_service.get_for_census_record(record)
            if link is None:
                # Otherwise generate one
                link = self.self_enrollment_link_service.generate_link(url_root, case, record)

            out.append(dict(
                id=record.id,
                ssn=record.employee_ssn,
                self_enroll_url=link.url if link else None,
            ))

        return out