import dateutil.parser
from datetime import datetime
import re
import csv
import StringIO

from flask import abort
from flask_stormpath import current_user
from sqlalchemy.orm import joinedload
import sqlalchemy as sa

from taa.core import DBService
from taa.core import db

from models import Case, CaseCensus, CaseEnrollmentPeriod, \
    CaseOpenEnrollmentPeriod, CaseAnnualEnrollmentPeriod

class CaseService(DBService):
    
    __model__ = Case
    
    def __init__(self, *args, **kwargs):
        super(CaseService, self).__init__(*args, **kwargs)
        
        self.census_records = CensusRecordService()
        self.enrollment_periods = CaseEnrollmentPeriodsService()

    def _preprocess_params(self, kwargs):
        kwargs = super(CaseService, self)._preprocess_params(kwargs)
        
        products = kwargs.get('products', [])
        if products and all(isinstance(p, unicode) for p in products):
            from taa.services.products import ProductService
            products_service = ProductService()
            kwargs['products'] = products_service.get_products_by_codes(products)
        
        return kwargs
    
    def get_if_allowed(self, case_id):
        
        case = self.get_or_404(case_id)

        if self.can_current_user_view_case(case):
            return case
        
        abort(401)
        
    def search_cases(self, by_agent=None, by_name=None, only_enrolling=False):
        # TODO: account for sub-agents
        query = self.query()
        if by_name:
            query = query.filter(Case.company_name.ilike(by_name))
        
        if by_agent:
            query = query.filter(Case.agent_id == by_agent)
        
        results = query.all()
        
        if only_enrolling:
            results = [case for case in results if self.is_enrolling(case)]
            
        return results
        
    def update_products(self, case, products):
        
        case.products = products
        db.session.flush()
        
    def get_agent_cases(self, agent, **kwargs):
        # TODO: account for sub-agents
        return self.search_cases(by_agent=agent.id, **kwargs)
        
    def is_enrolling(self, case):
        
        return case.active and any(
            period.currently_active()
            for period in case.enrollment_periods
        )
    
    def agent_can_view_case(self, agent, case):
        # TODO: account for sub-agents
        return case.agent_id == agent.id
    
    # Enrollment Periods
    
    def get_enrollment_periods(self, case):
        return self.enrollment_periods.get_all_for_case(case)
        
    def get_case_enrollment_period_data(self, case):
        # Return a dict for populating the form
        data = {'enrollment_period_type': case.enrollment_period_type}
        for period in self.get_enrollment_periods(case):
            period.populate_data_dict(data)
            
        return data
        
    def update_enrollment_periods(self, case, **data):
        
        if case.enrollment_period_type != data['enrollment_period_type']:
            case.enrollment_period_type = data['enrollment_period_type']

        # Remove existing periods
        self.enrollment_periods.remove_all_for_case(case)
            
        # Add the new enrollment period
        self.enrollment_periods.add_for_case(case, **data)
        
        
    # Census records

    def get_census_records(self, case, offset=None, num_records=None,
                           search_text=None, text_columns=None, 
                           sorting=None, sort_desc=False):

        query = self.census_records.find(case_id=case.id)

        if sorting:
            sort_col = getattr(CaseCensus, sorting)
            if sort_desc:
                sort_col = sa.desc(sort_col)
            query = query.order_by(sort_col)
            
        if search_text and text_columns:
            query = self._filter_record_text(query, search_text, text_columns)

        if offset > 0:
            query = query.offset(offset)
        if num_records > 0:
            query = query.limit(num_records)

        return query.all()

    def export_census_records(self, records):
        stream = StringIO.StringIO()
        self.census_records.export_csv(stream, records)
        return stream.getvalue()

    def count_census_records(self, case, search_text=None, text_columns=None):
        query = self.census_records.find(case_id=case.id)
        if search_text and text_columns:
            query = self._filter_record_text(query, search_text, text_columns)

        return query.count()

    def _filter_record_text(self, query, search_text, text_columns):
        filters = []
        for col in text_columns:
            filters.append(getattr(CaseCensus, col).ilike(search_text + "%"))
        return query.filter(sa.or_(*filters))

    def get_census_record(self, case, census_record_id):
        
        q = self.census_records.query(
            ).options(joinedload('case')
            ).filter_by(id=census_record_id
            )
        if case:
            q = q.filter_by(case_id=case.id)
        record = q.first()
        
        if not record:
            abort(404)
        
        return record
    
    def get_record_if_allowed(self, census_record_id):
        record = self.get_census_record(None, census_record_id)
        if not record:
            abort(404)
            
        # Verify authorization
        if self.can_current_user_view_case(record.case):
            return record
        
        abort(401)

    def can_current_user_view_case(self, case):
        from taa.services.agents import AgentService

        agent_service = AgentService()
        if agent_service.is_user_admin(current_user):
            return True
        elif agent_service.is_user_agent(current_user):
            agent = agent_service.get_agent_from_user(current_user)

            return self.agent_can_view_case(agent, case)
        
        return False
    
    def merge_census_data(self, case, records, replace_matching):
        return self.census_records.merge_census_data(case, records, replace_matching)
        
    def replace_census_data(self, case, records):
        return self.census_records.replace_census_data(case, records)
    
    def update_census_record(self, record, data):
        return self.census_records.update(record, **data)
    
    def delete_census_record(self, record):
        return self.census_records.delete(record)
    
class CaseEnrollmentPeriodsService(DBService):
    __model__ = CaseEnrollmentPeriod
    
    def add_for_case(self, case, **data):
        if data['enrollment_period_type'] == 'open':
            periods = [CaseOpenEnrollmentPeriod(start_date=data['open_period_start_date'], case_id=case.id)]
        else:
            valid_periods = [self.parse_annual_period_dates(d) for d in data['annual_period_dates'] 
                             if d['period_start_date'] and d['period_end_date']
                            ]
            
            periods = [CaseAnnualEnrollmentPeriod(start_date=d[0], end_date=d[1], case_id=case.id) 
                       for d in valid_periods]
        
        for p in periods:
            self.save(p)
        
        
    def parse_annual_period_dates(self, period):
        
        start = self.valid_annual_date(period['period_start_date'])
        end = self.valid_annual_date(period['period_end_date'])
        
        return start, end 
    
    def valid_annual_date(self, d):
        if not d:
            return None
        
        return parse(d + '/%s'%datetime.now().year)
                
    def get_all_for_case(self, case):
        return case.enrollment_periods
    
    def remove_all_for_case(self, case):
        self.query().filter(CaseEnrollmentPeriod.case == case).delete()
        
    
class CensusRecordService(DBService):
    __model__ = CaseCensus

    def get_record_dict(self, census_record):
        """
        Returns a dictionary suitable for displaying by the UI
        """
        return dict(
            id=census_record.id,
            ssn=self.format_ssn(census_record.employee_ssn),
            first=census_record.employee_first,
            last=census_record.employee_last,
            email=census_record.employee_email,
            sp_first=census_record.spouse_first,
            sp_last=census_record.spouse_last,
            completed_enrollment="---",
            elected_coverage="---",
        )
    
    def export_csv(self, file, census_records):
        writer = csv.writer(file)
        
        # Write the header row
        writer.writerow( [field.csv_column_name
               for field in CensusRecordParser.all_possible_fields
        ])
        
        # Write all the data
        for record in census_records:
            row = [getattr(record, field.database_name)
                   for field in CensusRecordParser.all_possible_fields
            ]
            writer.writerow(row)
            
        # Todo: include data about enrollment options?    
        
        return writer
    
    def format_ssn(self, ssn):
        if not len(ssn) == 9:
            return ssn

        return "{}-{}-{}".format(ssn[:3], ssn[3:5], ssn[5:])
        
    def merge_census_data(self, case, file_stream, replace_matching):
        """
        Updates existing records and adds new. Matches based on SSN, and depending on
         :replace_matching, will do replace matches or skip over them.
        """
        
        
        parser = CensusRecordParser()
        parser.process_file(file_stream)
        
        # Do the merge
        existing = self.find(case_id=case.id).all()
        existing_by_ssn = {r.employee_ssn: r for r in existing}
        added = []
        updated = []
        for record in parser.get_valid_data():
            if record['EMP_SSN'] in existing_by_ssn:
                if replace_matching:
                    # Update Existing
                    self.update(existing_by_ssn[record['EMP_SSN']], **parser.get_db_dict(record))
                    updated.append(existing_by_ssn[record['EMP_SSN']])
                else:
                    # Skip matching
                    continue
            else:
                # Add new
                added.append(self.add_record(case, **parser.get_db_dict(record)))
        
        db.session.commit()
        
        return parser.errors, added + updated
    
    def replace_census_data(self, case, file_stream):
        # Delete records for this case
        self.find(case_id=case.id).delete()
        
        parser = CensusRecordParser()
        parser.process_file(file_stream)
        
        # Add new records    
        valid_records = [self.add_record(case, **parser.get_db_dict(record)) for record in parser.get_valid_data()]
        db.session.commit()
        
        return parser.errors, valid_records
        
    def add_record(self, case, **data):
        """ 
        Create and add to the session, but don't commit or flush the session for speed
        """
        data['case_id'] = case.id
        record = self.new(**data)
        db.session.add(record)
        return record



class CensusRecordField(object):
    """
    Defines a column for the uploaded CSV census data
    """
    def __init__(self, csv_column_name, database_name, preprocessor, validators):
        self.csv_column_name = csv_column_name
        self.database_name = database_name
        self.preprocessor = preprocessor or (lambda x: x)
        self.validators = validators or []
    
    def validate(self, parser, record):
        all_valid = True
        for validator in self.validators:
            is_valid, error_message = validator(self, record)
            if not is_valid:
                parser.error_record_field(error_message,  
                                          self.csv_column_name,
                                          parser.get_line_number(),
                                          record)
                all_valid = False
            
        return all_valid
    
    def get_column_from_record(self, record):
        return record.get(self.csv_column_name, u'')
        
    def preprocess(self, data):
        return self.preprocessor(data)
        
    def add_validator(self, validator):
        self.validators.append(validator)

##
# Validators
##

def required_validator(field, record):
    data = field.get_column_from_record(record)
    if not data:
        return False, "Required Data Missing"
    
    return True, None

ssn_pattern = re.compile('^\d{9}$')
def ssn_validator(field, record):
    ssn = field.get_column_from_record(record)
    if not ssn:
        # Allow blank unless combined with required validator
        return True, None
    elif not ssn_pattern.match(ssn):
        return False, "Invalid SSN"
    
    return True, None

def gender_validator(field, record):
    gender = field.get_column_from_record(record)
    if not gender:
        # Allow blank unless combined with required validator
        return True, None,
    
    if gender not in ['male', 'female', 'm', 'f']:
        return False, "Gender must be 'Male' or 'Female'"
    
    return True, None

def birthdate_validator(field, record):
    date = field.get_column_from_record(record)
    if not date:
        # Allow blank unless combined with required validator
        return True, None
    
    if date > datetime.now():
        # The preprocessor currently keeps this from happening, but I will leave 
        #  it in here in case that changes
        return False, "Future date is not allowed for a birthday"
    
    return True, None

def email_validator(field, record):
    email = field.get_column_from_record(record)
    if not email:
        # Allow blank unless combined with required validator
        return True, None
    
    if '@' not in email and len(email) < 3:
        return False, 'Invalid email'

    return True, None

zip_pattern = re.compile('^\d{3,5}$')
def zip_validator(field, record):
    zip = field.get_column_from_record(record)
    if not zip:
        # Allow blank unless combined with required validator
        return True, None
    
    if not zip_pattern.match(zip):
        return False, "Invalid ZIP code"

    return True, None


def state_validator(field, record):
    state = field.get_column_from_record(record)
    from taa.services.products import ProductService
    
    ps = ProductService()
    if not state or not len(state) == 2 or not state in ps.get_all_statecodes():
        return False, "Invalid US State. Must be two-letter abbreviation."

    return True, None

class RequiredIfAnyInGroupValidator(object):
    def __init__(self, group_fields):
        self.group_fields = group_fields
        
    def __call__(self, field, record):
        # If any of the given fields have a value, require this field
        if any(group_field.get_column_from_record(record) for group_field in self.group_fields):
            return required_validator(field, record)
        
        return True, None

##
# Data preprocessors
##
def preprocess_string(data):
    if data is None:
        return u''
    return unicode(data).strip()

def preprocess_date(data):
    if data is None:
        return u''

    d = dateutil.parser.parse(data)
    if d >= datetime.today():
        # This can happen when you try to parse 2-digit years (excel issue?)
        # Solution should be OK, but if someone puts a future date in (like for an expected child?)
        # it doesn't work, and also won't work for 100+ year-old people. Which can't apply for life insurance, I think.
        d = datetime(d.year - 100, d.month, d.day)
    
    return d
    
def preprocess_zip(data):
    if data is None:
        return u''
    
    # Just want first five characters
    return unicode(data).strip().replace('-', '')[:5]

def preprocess_gender(data):
    data = data.lower()
    if data == 'f':
        return 'female'
    elif data == 'm':
        return 'male'
    
    return data

def preprocess_numbers(data):
    if data is None:
        return ''
    return "".join(c for c in unicode(data) if c.isdigit()) 
    

class CensusRecordParser(object):
    
    # Construct the fields and wire up the correct validation
    employee_first = CensusRecordField("EMP_FIRST", "employee_first", preprocess_string, [required_validator])
    employee_last = CensusRecordField("EMP_LAST", "employee_last", preprocess_string, [required_validator])
    employee_ssn = CensusRecordField("EMP_SSN", "employee_ssn", preprocess_numbers,
                                     [required_validator, ssn_validator])
    employee_gender = CensusRecordField("EMP_GENDER", "employee_gender", preprocess_gender,
                                        [required_validator, gender_validator])
    employee_birthdate = CensusRecordField("EMP_BIRTHDATE", "employee_birthdate", preprocess_date,
                                           [required_validator, birthdate_validator])
    employee_email = CensusRecordField("EMP_EMAIL", "employee_email", preprocess_string,
                                       [required_validator, email_validator])
    employee_phone = CensusRecordField("EMP_PHONE", "employee_phone", preprocess_string, [])
    employee_address1 = CensusRecordField("EMP_ADDRESS1", "employee_street_address", preprocess_string, [])
    employee_address2 = CensusRecordField("EMP_ADDRESS2", "employee_street_address2", preprocess_string, [])
    employee_city = CensusRecordField("EMP_CITY", "employee_city", preprocess_string, [])
    employee_state = CensusRecordField("EMP_STATE", "employee_state", preprocess_string, [state_validator])
    employee_zip = CensusRecordField("EMP_ZIP", "employee_zip", preprocess_zip, [zip_validator])

    spouse_first = CensusRecordField("SP_FIRST", "spouse_first", preprocess_string, [])
    spouse_last = CensusRecordField("SP_LAST", "spouse_last", preprocess_string, [])
    spouse_ssn = CensusRecordField("SP_SSN", "spouse_ssn", preprocess_numbers, [ssn_validator])
    spouse_gender = CensusRecordField("SP_GENDER", "spouse_gender", preprocess_gender, [gender_validator])
    spouse_birthdate = CensusRecordField("SP_BIRTHDATE", "spouse_birthdate", preprocess_date, [birthdate_validator])
    spouse_email = CensusRecordField("SP_EMAIL", "spouse_email", preprocess_string, [email_validator])
    spouse_phone = CensusRecordField("SP_PHONE", "spouse_phone", preprocess_string, [])
    spouse_address1 = CensusRecordField("SP_ADDRESS1", "spouse_street_address", preprocess_string, [])
    spouse_address2 = CensusRecordField("SP_ADDRESS2", "spouse_street_address2", preprocess_string, [])
    spouse_city = CensusRecordField("SP_CITY", "spouse_city", preprocess_string, [])
    spouse_state = CensusRecordField("SP_STATE", "spouse_state", preprocess_string, [state_validator])
    spouse_zip = CensusRecordField("SP_ZIP", "spouse_zip", preprocess_zip, [zip_validator])

    # Add group validation requirement. If any field in the group is given, all must be present
    spouse_fields = [spouse_first, spouse_last, spouse_ssn]
    validator = RequiredIfAnyInGroupValidator(spouse_fields)
    for field in spouse_fields:
        field.add_validator(validator)

    all_possible_fields = [
        employee_first,
        employee_last,
        employee_ssn,
        employee_gender,
        employee_birthdate,
        employee_email,
        employee_phone,
        employee_address1,
        employee_address2,
        employee_city,
        employee_state,
        employee_zip,

        spouse_first,
        spouse_last,
        spouse_ssn,
        spouse_birthdate,
        spouse_gender,
        spouse_email,
        spouse_phone,
        spouse_address1,
        spouse_address2,
        spouse_city,
        spouse_state,
        spouse_zip,
    ]

    MAX_CHILDREN = 6
    for num in range(1, MAX_CHILDREN + 1):
        child_first = CensusRecordField("CH{}_FIRST".format(num), "child{}_first".format(num), preprocess_string, [])
        child_last = CensusRecordField("CH{}_LAST".format(num), "child{}_last".format(num), preprocess_string, [])
        child_birthdate = CensusRecordField("CH{}_BIRTHDATE".format(num), "child{}_birthdate".format(num), preprocess_date,
                                            [birthdate_validator])

        child_group = [child_first, child_last, child_birthdate]
        validator = RequiredIfAnyInGroupValidator(child_group)
        for field in child_group:
            field.add_validator(validator)
        all_possible_fields += child_group


    def __init__(self):
        self.errors = []
        self.valid_data = []
        self.line_number = 0
        
    def _process_file_stream(self, file_stream):
        # To get universal newlines (ie, cross-platform) we use splitlines()
        bytes = file_stream.getvalue()
        reader = csv.DictReader(bytes.splitlines(), restkey="extra")
        
        try:
            headers = reader.fieldnames
            records = [r for r in reader]
        except csv.Error as e:
            self.error_message(message="Invalid CSV file format. First problem found on line {}. Detailed error: {}".format(
                reader.line_num, e))
            headers = records = []
        
        return headers, records
    
    def process_file(self, file_stream):
        headers, records = self._process_file_stream(file_stream)
        self.validate_header_row(headers, records)
        
        if self.errors:
            return
        
        preprocessed_records = (self.preprocess_record(record) for record in records)
        
        self.line_number = 0
        self.valid_data = []
        for record in preprocessed_records:
            self.line_number += 1
            if self.validate_record(headers, record):
                self.valid_data.append(record)
        
    def validate_record(self, headers, record):
        is_valid = True
        
        for field in self.all_possible_fields:
            is_valid &= field.validate(self, record)
        
        #is_valid &= self.validate_employee_data(headers, record)
        #is_valid &= self.validate_optional_data(headers, record)
        return is_valid
    
    def get_line_number(self):
        return self.line_number
    
    def get_valid_data(self):
        if self.errors:
            # Right now, it is all or nothing, so return empty list if any errors occurred
            return []
        
        return self.valid_data
    
    def validate_header_row(self, headers, records):
        if len(records) <= 1:
            self.error_message(
                "The uploaded CSV file did not appear to have a valid header row. Please see the sample data file for formatting examples."
            )
        
        missing_headers = self._get_missing_headers(headers)
        if missing_headers:
            missing_msg = ', '.join(missing_headers)
            self.error_message("The following required columns are missing from the uploaded file: {}".format(missing_msg))
    
    
    def get_error_headers(self, field_name):
        headers = ['EMP_FIRST', 'EMP_LAST']
        if field_name not in headers:
            headers.append(field_name)
        return headers
    
    def _get_missing_headers(self, headers):
        required_headers = [
            'EMP_SSN',
            'EMP_FIRST',
            'EMP_LAST',
            'EMP_GENDER',
            'EMP_BIRTHDATE',
            'EMP_EMAIL',
        ]
        return {h for h in required_headers if h not in headers}
    
    def error_message(self, message):
        self.errors.append(dict(
            message=message,
            records=[],
        ))
    
    def error_record_field(self, message, field_name, line_number, data):
        self.errors.append(dict(
            message=message,
            records=[data],
            line_number=line_number,
            headers=self.get_error_headers(field_name),
            field_name=field_name,
        ))
    
    def preprocess_record(self, record):
        data = {}
        
        for column in record:
            field = self.get_field_from_csv_column(column)
            if not field:
                continue
            
            data[column] = field.preprocess(record[column])
        
        return data
    
    fields_by_column_name = {field.csv_column_name: field for field in all_possible_fields}
    def get_field_from_csv_column(self, column):
        return self.fields_by_column_name.get(column)
    
    def get_db_dict(self, record):
        return {
            self.get_field_from_csv_column(csv_col_name).database_name: data
            for csv_col_name, data in record.items()
        }
        
            