import dateutil.parser
from datetime import datetime, date as datetime_date
import re
import csv
import StringIO

from flask import abort
from flask_stormpath import current_user
from sqlalchemy.orm import joinedload
import sqlalchemy as sa

from taa.core import DBService
from taa.core import db
from taa.services.agents.models import Agent
from models import (Case, CaseCensus, CaseEnrollmentPeriod,
                    CaseOpenEnrollmentPeriod, CaseAnnualEnrollmentPeriod,
                    SelfEnrollmentSetup)

class CaseService(DBService):
    __model__ = Case

    def __init__(self, *args, **kwargs):
        super(CaseService, self).__init__(*args, **kwargs)
        self.census_records = CensusRecordService()
        self.enrollment_periods = CaseEnrollmentPeriodsService()
        self.self_enrollment = SelfEnrollmentService()

    def _preprocess_params(self, kwargs):
        kwargs = super(CaseService, self)._preprocess_params(kwargs)
        products = kwargs.get('products', [])
        if products and all(isinstance(p, unicode) for p in products):
            from taa.services.products import ProductService
            products_service = ProductService()
            kwargs['products'] = products_service.get_products_by_codes(
                products)
        return kwargs

    def get_if_allowed(self, case_id):
        case = self.get_or_404(case_id)
        if self.can_current_user_view_case(case):
            return case
        abort(401)

    def search_cases(self, by_agent=None, by_name=None, only_enrolling=False):
        query = self.query()
        if by_name:
            query = query.filter(Case.company_name.ilike(by_name))
        if by_agent:
            # Right now, an agent can 'see' a given case if he is either
            # the owner or a 'partner' agent, which is an explicitly maintained
            # list by the HO admin.
            query = query.filter(db.or_(
                Case.agent_id == by_agent,
                Case.partner_agents.any(Agent.id == by_agent)
                )
            )
        results = query.all()
        if only_enrolling:
            results = [case for case in results if self.is_enrolling(case)]
        return results

    def get_products_for_case(self, case):
        # Return the sorted list of products for this case
        return sorted(case.products, cmp=lambda x, y: cmp(x.name, y.name))

    def update_products(self, case, products):
        case.products = products
        db.session.flush()

    def get_agent_cases(self, agent, **kwargs):
        return self.search_cases(by_agent=agent.id, **kwargs)

    def is_enrolling(self, case):
        return case.can_enroll()

    def get_most_recent_enrollment_period(self, case):
        periods = self.get_enrollment_periods(case)
        if not periods:
            return None
        if self.is_enrolling(case):
            # Return the active period
            return filter(lambda p: p.currently_active(), periods)[0]
        # Sort by start date descending and return the first one
        past_periods = filter(lambda p: p.get_start_date() < datetime.now(),
                              periods)
        if not past_periods:
            return None
        past_periods.sort(cmp=lambda x, y: -cmp(x.get_start_date(),
                                                y.get_start_date()))
        return past_periods[0]

    def agent_can_view_case(self, agent, case):
        return case.agent_id == agent.id or agent in case.partner_agents

    def update_partner_agents(self, case, agents):
        from models import case_partner_agents
        sql = case_partner_agents.delete(case_partner_agents.c.case_id ==
                                         case.id)
        db.session.execute(sql)
        db.session.flush()
        case.partner_agents = agents
        db.session.flush()

    def get_case_owner(self, case):
        return case.owner_agent if case.owner_agent else None

    def get_case_partner_agents(self, case):
        return [a for a in case.partner_agents if a != case.owner_agent]

    # Enrollment Periods

    def validate_enrollment_periods(self, case, data):
        return self.enrollment_periods.validate_for_case(case, data)

    def get_enrollment_periods(self, case):
        return case.enrollment_periods

    def get_case_enrollment_period_data(self, case):
        # Return a dict for populating the form
        data = {'enrollment_period_type': case.enrollment_period_type}
        for period in self.get_enrollment_periods(case):
            period.populate_data_dict(data)
        return data

    def update_enrollment_periods(self, case, periods):
        # Make sure the case enrollment type is updated to match the type of
        # the uploaded periods
        for period in periods:
            if period['period_type'] == (CaseOpenEnrollmentPeriod.PERIOD_TYPE
                                         and case.enrollment_period_type !=
                                         Case.OPEN_ENROLLMENT_TYPE):
                case.enrollment_period_type = Case.OPEN_ENROLLMENT_TYPE
            elif period['period_type'] == (CaseAnnualEnrollmentPeriod.PERIOD_TYPE
                                           and case.enrollment_period_type !=
                                           Case.ANNUAL_ENROLLMENT_TYPE):
                case.enrollment_period_type = Case.ANNUAL_ENROLLMENT_TYPE
        # Remove existing periods
        self.enrollment_periods.remove_all_for_case(case)
        # Add the new enrollment period
        added = self.enrollment_periods.add_for_case(case, periods)
        db.session.flush()
        return added

    # Census records

    def get_census_records(self, case, offset=None, num_records=None,
                           search_text=None, text_columns=None,
                           sorting=None, sort_desc=False, include_enrolled=True,
                           filter_ssn=None, filter_birthdate=None):
        from taa.services.enrollments.models import EnrollmentApplication
        query = self.census_records.find(case_id=case.id)
        # Filter enrollment status. Also load in any enrollment data eagerly.
        if include_enrolled == False:
            # Since we need to filter on enrollment status, pull in the
            # enrollment applications if it exists.
            query = query.outerjoin('enrollment_applications').filter(
                EnrollmentApplication.application_status ==
                EnrollmentApplication.APPLICATION_STATUS_DECLINED)
            query = query.options(db.contains_eager(
                'enrollment_applications').subqueryload(
                'coverages').joinedload('product'))
        else:
            # Eager load enrollment applications, coverages, and associated
            # products
            query = query.options(
                db.joinedload('enrollment_applications'
                    ).subqueryload('coverages'
                    ).joinedload('product')
            )
        if filter_ssn:
            query = query.filter(CaseCensus.employee_ssn ==
                                 filter_ssn.replace('-', ''))
        if filter_birthdate:
            bd = dateutil.parser.parse(filter_birthdate)
            query = query.filter(CaseCensus.employee_birthdate == bd)
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
            filters.append(getattr(CaseCensus, col).ilike(search_text + '%'))
        return query.filter(sa.or_(*filters))

    def get_census_record(self, case, census_record_id):
        q = self.census_records.query().options(
            joinedload('case')).filter_by(id=census_record_id)
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
        if agent_service.can_manage_all_cases(current_user):
            return True
        elif agent_service.is_user_agent(current_user):
            agent = agent_service.get_agent_from_user(current_user)
            return self.agent_can_view_case(agent, case)
        return False

    def create_ad_hoc_census_record(self, case, **data):
        # TODO: move this check up to the specific API that requires it if necessary
        # Ad-hoc records that decline are a special case that have no
        # requirements
        # if 'ssn' not in data:
        #    abort(400, "SSN is required to create an ad-hoc census record")
        ssn = data.get('ssn', '').replace('-', '')
        record =  self.census_records.add_record(case, **dict(employee_ssn=ssn,
                                                              is_uploaded_census=False))
        db.session.flush()
        return record

    def merge_census_data(self, case, file_data, replace_matching):
        return self.census_records.merge_census_data(case, file_data,
                                                     replace_matching)

    def replace_census_data(self, case, records):
        return self.census_records.replace_census_data(case, records)

    def update_census_record(self, record, data):
        return self.census_records.update(record, **data)

    def update_census_record_from_enrollment(self, record, data):
        return self.census_records.update_from_enrollment(record, data)

    def delete_census_record(self, record):
        if record.enrollment_applications:
            # Can only delete an enrolled record if we have permission
            from taa.services.agents import AgentService
            agent_service = AgentService()
            if not agent_service.can_manage_all_cases(current_user):
                abort(401, "The current user does not have permission to delete"
                           "an enrolled census record.")
            from taa.services.enrollments import (EnrollmentApplicationService,
                                                  EnrollmentApplicationCoverageService)
            enrollment_service = EnrollmentApplicationService()
            enrollment_service.delete_enrollment_data(record)
        return self.census_records.delete(record)

    def delete_case(self, case):
        from taa.services.agents import AgentService
        from taa.services.enrollments import EnrollmentApplicationService
        enrollments_service = EnrollmentApplicationService()
        agent_service = AgentService()
        # Remove all enrollments if allowed
        if agent_service.can_manage_all_cases(current_user):
            enrollments_service.delete_case_enrollment_data(case)
        # remove all census records and enrollment_periods first
        self.census_records.remove_all_for_case(case)
        self.enrollment_periods.remove_all_for_case(case)
        return self.delete(case)

    def does_case_have_enrollments(self, case):
        from taa.services.enrollments import EnrollmentApplicationService
        return bool(
            EnrollmentApplicationService().find(case_id=case.id).count())

    # Self-enrollment setup

    def get_self_enrollment_setup(self, case):
        return self.self_enrollment.first(case_id=case.id)

    def create_self_enrollment_setup(self, case, data):
        if 'created_by' not in data:
            data['created_by'] = case.agent_id
        return self.self_enrollment.create(case_id=case.id, **data)

    def update_self_enrollment_setup(self, setup, data):
        return self.self_enrollment.update(setup, **data)


class CaseEnrollmentPeriodsService(DBService):
    __model__ = CaseEnrollmentPeriod

    def validate_for_case(self, case, data):
        errors = []
        if case.enrollment_period_type == Case.OPEN_ENROLLMENT_TYPE:
            return self.validate_open_enrollment_period(case, data)
        else:
            return self.validate_annual_enrollment_period(case, data)

    def validate_open_enrollment_period(self, case, data):
        errors = {}
        if len(data) == 0:
            periods = []
        elif len(data) > 1:
            periods = [data[0]]
        else:
            periods = data
        for period in data:
            # Not finished?
            dateutil.parser.parse(period['start_date'])
            dateutil.parser.parse(period['end_date'])
        return errors

    def validate_annual_enrollment_period(self, case, data):
        errors = []
        # for period in data:
        #    if not period.get('start_date'):
        #        errors.append(dict(error='Invalid date'))
        return errors

    def add_for_case(self, case, period_data):
        periods = []
        for period in period_data:
            if period['period_type'] == CaseAnnualEnrollmentPeriod.PERIOD_TYPE:
                start = self.valid_annual_date(period['start_date'])
                end = self.valid_annual_date(period['end_date'])
                periods.append(CaseAnnualEnrollmentPeriod(start_date=start,
                                                          end_date=end,
                                                          case_id=case.id))
            elif period['period_type'] == CaseOpenEnrollmentPeriod.PERIOD_TYPE:
                start = self.valid_date(period['start_date'])
                end = self.valid_date(period['end_date'])
                periods.append(CaseOpenEnrollmentPeriod(start_date=start,
                                                        end_date=end,
                                                        case_id=case.id))
        for p in periods:
            self.save(p)
        return periods

    def valid_annual_date(self, d):
        if not d:
            return None
        date = dateutil.parser.parse('{}/{}'.format(d, datetime.now().year))
        # strip time
        return datetime_date(date.year, date.month, date.day)

    def valid_date(self, d):
        if not d:
            return None
        date = dateutil.parser.parse(d)
        # Strip time
        return datetime_date(date.year, date.month, date.day)

    def remove_all_for_case(self, case):
        self.query().filter(CaseEnrollmentPeriod.case == case).delete()


class CensusRecordService(DBService):
    __model__ = CaseCensus

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

    def get_record_dict(self, census_record):
        """
        Returns a dictionary suitable for displaying by the UI
        """
        from taa.models import EnrollmentApplication
        enrollment_status = ''
        for application in census_record.enrollment_applications:
            if (application.application_status ==
                    EnrollmentApplication.APPLICATION_STATUS_ENROLLED):
                enrollment_status = 'Enrolled'
            else:
                enrollment_status = 'Declined'
        return dict(
            id=census_record.id,
            ssn=self.format_ssn(census_record.employee_ssn),
            first=census_record.employee_first,
            last=census_record.employee_last,
            email=census_record.employee_email,
            sp_first=census_record.spouse_first,
            sp_last=census_record.spouse_last,
            completed_enrollment=enrollment_status != '',
            elected_coverage=enrollment_status == 'Enrolled',
        )

    def get_full_record_dict(self, census_record):
        return {
            field.csv_column_name: getattr(census_record, field.database_name)
            for field in CensusRecordParser.all_possible_fields
            }

    def export_csv(self, file, census_records):
        writer = csv.writer(file)
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


class CensusRecordField(object):
    """
    Defines a column for the uploaded CSV census data
    """
    def __init__(self, csv_column_name, database_name, preprocessor, validators,
                 post_processors=None):
        self.csv_column_name = csv_column_name
        self.database_name = database_name
        self.preprocessor = preprocessor or (lambda x: x)
        self.validators = validators or []
        self.post_processors = post_processors or []

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

    def preprocess(self, data, record):
        return self.preprocessor(data, record)

    def postprocess(self, field_data, all_data):
        new_data = field_data
        for postprocessor in self.post_processors:
            new_data = postprocessor(self, new_data, all_data)
        return new_data

    def add_validator(self, validator):
        self.validators.append(validator)

##
# Validators
##

def required_validator(field, record, message=None):
    data = field.get_column_from_record(record)
    if not data:
        message = message if message else "Required Data Missing"
        return False, message
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
    else:
        pass
    if not isinstance(date, datetime):
        return False, "Invalidate date"
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
    if not state:
        return True, None

    from taa.services.products import ProductService
    ps = ProductService()
    if not state or not len(state) == 2 or not state in ps.get_all_statecodes():
        return False, "Invalid US State. Must be two-letter abbreviation."

    return True, None


class RequiredIfAnyInGroupValidator(object):
    def __init__(self, group_fields, message=None):
        self.group_fields = group_fields
        self.message = message

    def __call__(self, field, record):
        # If any of the given fields have a value, require this field
        if any(group_field.get_column_from_record(record)
               for group_field in self.group_fields):
            return required_validator(field, record, self.message)
        return True, None

##
# Data preprocessors
##

def preprocess_string(data, record=None):
    if data is None:
        return u''
    return unicode(data).strip()

def postprocess_spouse_last(field, data, record):
    "Automatically populate a spouse or child last name if blank"
    employee_last = preprocess_string(
        CensusRecordParser.employee_last.get_column_from_record(record))
    # Use one of the required spouse fields
    spouse_birthdate = record.get(
        CensusRecordParser.spouse_birthdate.csv_column_name)
    if not data and employee_last and spouse_birthdate:
        return employee_last
    return data


def postprocess_children_last(field, data, record):
    """Automatically populate child last names if blank"""
    employee_last = preprocess_string(
        CensusRecordParser.employee_last.get_column_from_record(record))
    p = re.compile('CH(\d)_LAST')
    match = p.match(field.csv_column_name)
    if match:
        child_num = match.groups()[0]
        csv_first_col_name = 'CH{}_FIRST'.format(child_num)
        csv_birthdate_col_name = 'CH{}_BIRTHDATE'.format(child_num)
        first = record.get(csv_first_col_name)
        birthdate = record.get(csv_birthdate_col_name)

        # Use one of the required spouse fields
        spouse_birthdate = record.get(
            CensusRecordParser.spouse_birthdate.csv_column_name)
        if (not data) and employee_last and (first or birthdate):
            return employee_last
    return data


def preprocess_date(data, record):
    if data is None or data == '':
        return None
    try:
        d = dateutil.parser.parse(data)
        if d >= datetime.today():
            # This can happen when you try to parse 2-digit years (excel issue?)
            # Solution should be OK, but if someone puts a future date in
            # (like for an expected child?) it doesn't work, and also won't
            # work for 100+ year-old people. Which can't apply for life
            # insurance, I think.
            d = datetime(d.year - 100, d.month, d.day)
    except ValueError:
        # Can't be parsed as a date; return as-is and let validation
        # handle the error
        return data
    return d


def preprocess_zip(data, record):
    if data is None:
        return u''
    # Just want first five characters
    return unicode(data).strip().replace('-', '')[:5]


def preprocess_gender(data, record):
    data = data.lower()
    if data == 'f':
        return 'female'
    elif data == 'm':
        return 'male'
    return data


def preprocess_numbers(data, record):
    if data is None:
        return ''
    return ''.join(c for c in unicode(data) if c.isdigit())


def preprocess_y_n(data, record):
    if data is None or data == '':
        return ''
    if str(data).lower() in ['y', 'true', 'yes']:
        return 'Y'
    else:
        return 'N'

class CensusRecordParser(object):
    # Construct the fields and wire up the correct validation
    # Employee
    employee_first = CensusRecordField('EMP_FIRST', 'employee_first', preprocess_string, [required_validator])
    employee_last = CensusRecordField('EMP_LAST', 'employee_last', preprocess_string, [required_validator])
    employee_ssn = CensusRecordField('EMP_SSN', 'employee_ssn', preprocess_numbers, [required_validator, ssn_validator])
    employee_gender = CensusRecordField('EMP_GENDER', 'employee_gender', preprocess_gender, [gender_validator])
    employee_birthdate = CensusRecordField('EMP_BIRTHDATE', 'employee_birthdate', preprocess_date, [required_validator, birthdate_validator])
    employee_email = CensusRecordField('EMP_EMAIL', 'employee_email', preprocess_string, [email_validator])
    employee_phone = CensusRecordField('EMP_PHONE', 'employee_phone', preprocess_string, [])
    employee_address1 = CensusRecordField('EMP_ADDRESS1', 'employee_street_address', preprocess_string, [])
    employee_address2 = CensusRecordField('EMP_ADDRESS2', 'employee_street_address2', preprocess_string, [])
    employee_city = CensusRecordField('EMP_CITY', 'employee_city', preprocess_string, [])
    employee_state = CensusRecordField('EMP_STATE', 'employee_state', preprocess_string, [state_validator])
    employee_zip = CensusRecordField('EMP_ZIP', 'employee_zip', preprocess_zip, [zip_validator])
    employee_height_inches = CensusRecordField('EMP_HEIGHT_IN', 'employee_height_inches', preprocess_string, [])
    employee_weight_lbs = CensusRecordField('EMP_WEIGHT_LBS', 'employee_weight_lbs', preprocess_string, [])
    employee_smoker = CensusRecordField('EMP_SMOKER_Y_N', 'employee_smoker', preprocess_y_n, [])
    # Spouse
    spouse_first = CensusRecordField('SP_FIRST', 'spouse_first', preprocess_string, [])
    spouse_last = CensusRecordField('SP_LAST', 'spouse_last', preprocess_string, [], [postprocess_spouse_last])
    spouse_ssn = CensusRecordField('SP_SSN', 'spouse_ssn', preprocess_numbers, [ssn_validator])
    spouse_gender = CensusRecordField('SP_GENDER', 'spouse_gender', preprocess_gender, [gender_validator])
    spouse_birthdate = CensusRecordField('SP_BIRTHDATE', 'spouse_birthdate', preprocess_date, [birthdate_validator])
    spouse_email = CensusRecordField('SP_EMAIL', 'spouse_email', preprocess_string, [email_validator])
    spouse_phone = CensusRecordField('SP_PHONE', 'spouse_phone', preprocess_string, [])
    spouse_address1 = CensusRecordField('SP_ADDRESS1', 'spouse_street_address', preprocess_string, [])
    spouse_address2 = CensusRecordField('SP_ADDRESS2', 'spouse_street_address2', preprocess_string, [])
    spouse_city = CensusRecordField('SP_CITY', 'spouse_city', preprocess_string, [])
    spouse_state = CensusRecordField('SP_STATE', 'spouse_state', preprocess_string, [state_validator])
    spouse_zip = CensusRecordField('SP_ZIP', 'spouse_zip', preprocess_zip, [zip_validator])
    spouse_height_inches = CensusRecordField('SP_HEIGHT_IN', 'spouse_height_inches', preprocess_string, [])
    spouse_weight_lbs = CensusRecordField('SP_WEIGHT_LBS', 'spouse_weight_lbs', preprocess_string, [])
    spouse_smoker = CensusRecordField('SP_SMOKER_Y_N', 'spouse_smoker', preprocess_y_n, [])
    # Add group validation requirement. If any field in the group is given,
    # all must be present
    spouse_fields = [spouse_first, spouse_birthdate]
    for field in spouse_fields:
        validator = RequiredIfAnyInGroupValidator(
            spouse_fields,
            message="{} is required if any of the following are"
                    "provided: {}".format(field.csv_column_name,
                                          ', '.join([f.csv_column_name
                                                     for f in spouse_fields
                                                     if f is not field])
                                          ))
        # If any in group provided, all must be valid
        field.add_validator(validator)
        # Also require this field if the SSN was provided
        field.add_validator(RequiredIfAnyInGroupValidator(
            [spouse_ssn],
            message="{} is required if {} is provided".format(
                field.csv_column_name, spouse_ssn.csv_column_name)
        ))
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
        employee_height_inches,
        employee_weight_lbs,
        employee_smoker,
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
        spouse_height_inches,
        spouse_weight_lbs,
        spouse_smoker,
    ]
    MAX_CHILDREN = 6
    for num in range(1, MAX_CHILDREN + 1):
        child_first = CensusRecordField('CH{}_FIRST'.format(num),
                                        'child{}_first'.format(num),
                                        preprocess_string, [])
        child_last = CensusRecordField('CH{}_LAST'.format(num),
                                       'child{}_last'.format(num),
                                       preprocess_string, [],
                                       [postprocess_children_last])
        child_birthdate = CensusRecordField('CH{}_BIRTHDATE'.format(num),
                                            'child{}_birthdate'.format(num),
                                            preprocess_date,
                                            [birthdate_validator])
        # Require child_first if child birthdate given
        child_first.add_validator(
            RequiredIfAnyInGroupValidator([child_birthdate]))
        all_possible_fields += [child_first, child_last, child_birthdate]

    def __init__(self):
        self.errors = []
        self.valid_data = []
        self.used_ssns = set()
        self.line_number = 0

    def _process_file_stream(self, file_data):
        # To get universal newlines (ie, cross-platform) we use splitlines()
        bytes = file_data.getvalue()
        # Autodetect CSV dialect
        dialect = csv.Sniffer().sniff(bytes)
        lines = bytes.splitlines()
        lines = self._preprocess_header_row(lines, dialect)
        reader = csv.DictReader(lines, restkey='extra', dialect=dialect)
        try:
            headers = reader.fieldnames
            records = [r for r in reader]
        except csv.Error as e:
            self.error_message(
                # message="Invalid CSV file format. First problem found on "
                # "line {}. Detailed error: {}".format(# reader.line_num, e)
                message="There was a problem in the file or file format that "
                        "prevented us from accepting it. Please ensure you are "
                        "sending a valid CSV file, compare your file with the "
                        "provided sample CSV, or otherwise double-check the "
                        "data you are sending. If you continue to have "
                        "problems, please contact your 5Star representative "
                        "for assistance.",
            )
            # TODO: log the actual exception
            line_number = reader.line_num
            headers = records = []
        return headers, records, dialect

    def _preprocess_header_row(self, lines, dialect):
        """
        To get case-insensitive parsing behaviour, we uppercase every column
        in the first line before passing it off to the DictReader
        """
        header = []
        header_reader = csv.reader([lines[0]], dialect)
        for row in header_reader:
            header = [col.upper() for col in row]
        io = StringIO.StringIO()
        header_writer = csv.writer(io, dialect)
        header_writer.writerow(header)
        lines[0] = io.getvalue()
        return lines

    def process_file(self, file_data, error_if_matching=None):
        headers, records, dialect = self._process_file_stream(file_data)
        self.validate_header_row(headers, records)
        # Don't do any more processing if missing important headers
        if self.errors:
            return
        preprocessed_records = (self.preprocess_record(record)
                                for record in records)
        # Reset internal counters
        self.line_number = 0
        self.valid_data = []
        self.used_ssns = set()
        self.error_if_matching = error_if_matching or {}
        for record in preprocessed_records:
            self.line_number += 1
            if self.validate_record(headers, record):
                # Run any post-processing
                self.postprocess_record(record)
                self.valid_data.append(record)

    def validate_record(self, headers, record):
        is_valid = True
        for field in self.all_possible_fields:
            is_valid &= field.validate(self, record)
        # Do not allow duplicate employee SSNs in a single upload
        ssn = record.get(self.employee_ssn.csv_column_name)
        if ssn and ssn in self.used_ssns:
            is_valid = False
            self.error_record_field(
                "Duplicate SSN in census file",
                self.employee_ssn.csv_column_name,
                self.line_number,
                record
            )
        elif ssn:
            self.used_ssns.add(ssn)
        # Some modes require us to throw an error if an existing record exists
        # in the database (matched on SSN). Check that here.
        if self.error_if_matching and ssn in self.error_if_matching:
            self.error_record_field(
                "A census record exists that matches this SSN. This is not "
                "allowed when uploading in 'Add New Records' mode.",
                self.employee_ssn.csv_column_name,
                self.line_number,
                record
            )
        return is_valid

    def get_line_number(self):
        return self.line_number

    def get_valid_data(self):
        if self.errors:
            # Right now, it is all or nothing, so return empty list if any
            # errors occurred
            return []
        return self.valid_data

    def validate_header_row(self, headers, records):
        if len(records) == 0:
            self.error_message(
                "The uploaded CSV file did not appear to have a valid header "
                "row. Please see the sample data file for formatting examples."
            )
        missing_headers = self._get_missing_headers(headers)
        if missing_headers:
            missing_msg = ', '.join(missing_headers)
            self.error_message("The following required columns are missing "
                               "from the uploaded file: {}".format(missing_msg))

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
            data[column] = field.preprocess(record[column], record)
        return data

    def postprocess_record(self, record):
        for field in self.all_possible_fields:
            if field.csv_column_name not in record:
                val = None
            else:
                val = record[field.csv_column_name]
            record[field.csv_column_name] = field.postprocess(val, record)

    fields_by_column_name = {field.csv_column_name:
                             field for field in all_possible_fields}

    def get_field_from_csv_column(self, column):
        return self.fields_by_column_name.get(column)

    def get_db_dict(self, record):
        return {
            self.get_field_from_csv_column(csv_col_name).database_name: data
            for csv_col_name, data in record.items()
            }


class SelfEnrollmentService(DBService):
    __model__ = SelfEnrollmentSetup
