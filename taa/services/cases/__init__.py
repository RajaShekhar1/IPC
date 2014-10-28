from dateutil.parser import parse
from datetime import datetime
import re

from flask import abort
from flask_stormpath import current_user
from sqlalchemy.orm import joinedload

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
        
    def get_agent_cases(self, agent, only_enrolling=False):
        # TODO: account for sub-agents
        all_cases = self.find(agent_id=agent.id).all()
        if only_enrolling:
            return [case for case in all_cases if self.is_enrolling(case)]
        
        return all_cases
    
    def is_enrolling(self, case):
        
        return case.active and any(
            period.currently_active()
            for period in case.enrollment_periods
        )
    
    def agent_can_view_case(self, agent, case):
        # TODO: account for sub-agents
        return case.agent_id == agent.id
    
    def get_census_records(self, case):
        return self.census_records.find(case_id=case.id).all()

    
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

    #CSV_FIELD_FORMAT = ["EMP_SSN", "EMP_FIRST", "EMP_LAST", "EMP_BIRTHDATE", "EMP_EMAIL",
    #                    "SP_SSN", "SP_FIRST", "SP_LAST", "SP_BIRTHDATE", ]
    CSV_FIELD_MAPPING = {
        'EMP_SSN': ('employee_ssn', lambda x: x.replace('-', '')),
        'EMP_FIRST':'employee_first',
        'EMP_LAST': 'employee_last',
        'EMP_GENDER': 'employee_gender',
        'EMP_BIRTHDATE': 'employee_birthdate',
        'EMP_EMAIL': 'employee_email',
        'EMP_PHONE': 'employee_phone',
        'EMP_ADDRESS1': 'employee_street_address',
        'EMP_ADDRESS2': 'employee_street_address2',
        'EMP_CITY': 'employee_city',
        'EMP_STATE': 'employee_state',
        'EMP_ZIP': ('employee_zip', lambda x: x.replace('-', '')[:5]),
        
        'SP_SSN': ('spouse_ssn', lambda x: x.replace('-', '')),
        'SP_FIRST': 'spouse_first',
        'SP_GENDER': 'spouse_gender',
        'SP_LAST': 'spouse_last',
        'SP_BIRTHDATE': 'spouse_birthdate',
        'SP_EMAIL': 'spouse_email',
        'SP_PHONE': 'spouse_phone',
        'SP_ADDRESS1': 'spouse_street_address',
        'SP_ADDRESS2': 'spouse_street_address2',
        'SP_CITY': 'spouse_city',
        'SP_STATE': 'spouse_state',
        'SP_ZIP': ('spouse_zip', lambda x: x.replace('-', '')[:5]),
        
    }
    # Add children defs
    for num in range(1, 6+1):
        CSV_FIELD_MAPPING['CH{}_FIRST'.format(num)] = 'child{}_first'.format(num)
        CSV_FIELD_MAPPING['CH{}_LAST'.format(num)] = 'child{}_last'.format(num)
        CSV_FIELD_MAPPING['CH{}_BIRTHDATE'.format(num)] = 'child{}_birthdate'.format(num)
        
    def merge_census_data(self, case, records, replace_matching):
        existing = self.find(case_id=case.id).all()
        existing_by_ssn = {r.employee_ssn: r for r in existing}

        # Extra copy for now
        records = [r for r in records]
        
        parsed_records = [self.parse_census_record(case, r) for r in records]
        if any(errors for errors, r in parsed_records):
            all_errors = []
            error_records = []
            for i, (errors, record) in enumerate(parsed_records):
                if errors:
                    all_errors.append(errors)
                    error_records.append(records[i])
            
            return all_errors, error_records

        added = []
        for errors, record in parsed_records:
            if record['employee_ssn'] in existing_by_ssn:
                if replace_matching:
                    self.update(existing_by_ssn[record['employee_ssn']], **record)
                else:
                    continue
            else:
                added.append(self.add_record(**record))
        
        db.session.commit()
        # return just added records
        return [], added
    
    def replace_census_data(self, case, records):
        # Delete records for this case
        self.find(case_id=case.id).delete()

        # Extra copy for now
        records = [r for r in records]

        parsed_records = [self.parse_census_record(case, record) for record in records]
        
        if not any(errors for errors, record in parsed_records):
            records = [self.add_record(**record) for errors, record in parsed_records]
            db.session.commit()
            return [], records
        else:
            all_errors = []
            error_records = []
            for i, (errors, record) in enumerate(parsed_records):
                if errors:
                    all_errors.append(errors)
                    error_records.append(records[i])
            return all_errors, records
        
    def add_record(self, **data):
        # Create and add to the session, but don't commit or flush for speed
        record = self.new(**data)
        db.session.add(record)
        return record
        
    def parse_census_record(self, case, record):
        
        data = {'case_id': case.id}
        
        for field in record:
            if field in self.CSV_FIELD_MAPPING:
                conv = self.CSV_FIELD_MAPPING[field]
                if isinstance(conv, str):
                    data[conv] = record[field] if record[field] else None
                else:
                    record_field_name, conv = conv
                    data[record_field_name] = conv(record[field]) if conv(record[field]) else None
        
        # verification of required data present (SSN, email, names, birth date)
        employee_required = {
            'employee_ssn':valid_ssn, 
            'employee_email':valid_email, 
            'employee_first':valid_name, 
            'employee_last':valid_name, 
            'employee_birthdate':valid_birthdate,
        }
        valid_if_present = {
            'employee_gender': valid_gender,
            'spouse_gender': valid_gender,
            'employee_state': valid_state,
            'employee_zip': valid_zip,
            'spouse_state': valid_state,
            'spouse_zip': valid_zip,
            'spouse_ssn': valid_ssn,
            'spouse_email': valid_email,
            'spouse_birthdate': valid_birthdate,
        }
        for x in range(1, 6+1):
            valid_if_present['child{}_birthdate'.format(x)] = valid_birthdate
        
        errors = []
        for field_name, validator in employee_required.items():
            if not field_name in data:
                errors.append(dict(message='Employee data required: {}'.format(field_name), field_name=field_name))
                continue 
            
            is_valid, error_msg = validator(data[field_name])
            if not is_valid:
                errors.append(dict(message='Invalid employee data: {}'.format(error_msg), field_name=field_name))
        
        for field_name, validator in valid_if_present.items():
            if not data.get(field_name):
                continue
            
            is_valid, error_msg = validator(data[field_name])
            if not is_valid:
                errors.append(dict(message='Invalid data: {}'.format(error_msg), field_name=field_name))
        
        return errors, data

ssn_pattern = re.compile('^\d{9}$')
def valid_ssn(ssn):
    ssn = ssn.strip().replace('-', '')
    
    
    if not ssn_pattern.match(ssn):
        return False, "Invalid SSN: '{}'".format(ssn)
    
    return True, None

def valid_email(email):
    
    if '@' not in email and len(email) < 3:
        return False, 'Invalid email: \'{}\''.format(email)
    
    return True, None

def valid_birthdate(date):
    
    d = parse(date)
    if d >= datetime.today():
        return False, 'Invalid birthdate: future date \'{}\''.format(date)
    
    return True, None

def valid_name(name):
    name = name.strip()
    if not name:
        return False, "Invalid Name: '{}'".format(name)
    return True, None


def valid_state(state):
    from taa.services.products import ProductService
    ps = ProductService()
    if not state or not len(state) == 2 or not state in ps.get_all_statecodes():
        return False, "Invalid US State: '{}'. Must be two-letter abbreviation.".format(state)
    
    return True, None

zip_pattern = re.compile('^\d{3,5}$')
def valid_zip(zip):
    if not zip_pattern.match(zip):
        return False, "Invalid ZIP code: '{}'".format(zip)
    
    return True, None

def valid_gender(gender):
    if gender not in ['Male', 'Female', '']:
        return False, "Gender must be 'Male' or 'Female', not '{}'".format(gender)
    
    return True, None
