
import csv
import datetime
import dateutil.parser
from decimal import Decimal
import json
import StringIO
from taa import JSONEncoder

from taa.core import DBService, db
from models import EnrollmentApplication, EnrollmentApplicationCoverage

from taa.services import RequiredFeature
from enrollment_application_coverages import (
    filter_applicant_coverages,
    group_coverages_by_product,
    select_most_recent_coverage,
)


class EnrollmentApplicationService(DBService):
    __model__ = EnrollmentApplication

    report_service = RequiredFeature('EnrollmentReportService')
    coverages_service = RequiredFeature('EnrollmentApplicationCoverageService')
    case_service = RequiredFeature('CaseService')
    product_service = RequiredFeature('ProductService')
    batch_item_service = RequiredFeature('EnrollmentImportBatchItemService')

    def save_enrollment_data(self, data, case, census_record, agent, received_data=None):
        """
        Save all the enrollment and coverage information, including creating a census record if required.
            - received_data is to record the raw imported data that we have not standardized
        """

        if not census_record:
            census_record = self.case_service.create_ad_hoc_census_record(case=case, data=data)

        # Update the census record data with the new data
        self.case_service.update_census_record_from_enrollment(census_record, data)

        # Store extra enrollment data on the enrollment
        enrollment = self._create_enrollment(census_record, data, agent, received_data=received_data)

        # Save coverages
        self._save_coverages(enrollment, data)
        return enrollment

    def delete_case_enrollment_data(self, case):
        for census_record in case.census_records:
            self.delete_enrollment_data(census_record)

    def delete_enrollment_data(self, census_record):
        for enrollment_application in census_record.enrollment_applications:
            # Remove coverage data
            for coverage in enrollment_application.coverages:
                self.coverages_service.delete(coverage)

            # Remove any import batch data
            self.batch_item_service.delete_for_enrollment(enrollment_application)

            # Remove the application data
            self.delete(enrollment_application)

    def _create_enrollment(self, census_record, data, agent, received_data=None):
        # Link to census record and case, if it exists
        if census_record:
            case_id = census_record.case_id
            census_record_id = census_record.id
        else:
            case_id = None
            census_record_id = None

        # Link to agent if given
        if agent:
            agent_code = agent.agent_code
            agent_name = agent.name()
            agent_id = agent.id
        else:
            agent_code = None
            agent_name = None
            agent_id = None

        # Handle decline coverage case
        if data['did_decline']:
            return self.create(**dict(
                case_id=case_id,
                census_record_id=census_record_id,
                application_status=EnrollmentApplication.APPLICATION_STATUS_DECLINED,
                method=data['method'],
            ))
        if data['employee_beneficiary'] == 'spouse':
            emp_beneficiary_name = '{} {}'.format(data['spouse']['first'],
                                                  data['spouse']['last'])
            emp_beneficiary_ssn =  self._strip_ssn(data['spouse']['ssn'])
            emp_beneficiary_relation = 'spouse'
            emp_beneficiary_dob = data['spouse']['birthdate']
        else:

            emp_beneficiary_name = data.get('employee_beneficiary_name')
            emp_beneficiary_ssn = self._strip_ssn(data.get('employee_beneficiary_ssn', None))
            emp_beneficiary_relation = data.get('employee_beneficiary_relationship')
            emp_beneficiary_dob = data.get('employee_beneficiary_dob', None)

        if data['spouse_beneficiary'] == 'spouse':
            sp_beneficiary_name = '{} {}'.format(data['employee']['first'],
                                                 data['employee']['last'])
            sp_beneficiary_ssn = self._strip_ssn(data['employee']['ssn'])
            sp_beneficiary_relation = 'spouse'
            sp_beneficiary_dob = data['employee']['birthdate']
        else:
            sp_beneficiary_name = data.get('spouse_beneficiary_name')
            sp_beneficiary_ssn = self._strip_ssn(data.get('spouse_beneficiary_ssn'))
            sp_beneficiary_relation = data.get('spouse_beneficiary_relationship')
            sp_beneficiary_dob = data.get('spouse_beneficiary_dob')

        given_sig_time = data.get('time_stamp')
        signature_time = given_sig_time if given_sig_time else datetime.datetime.now()
        enrollment_data = dict(
            received_data=json.dumps(received_data, cls=JSONEncoder),
            standardized_data=json.dumps(data, cls=JSONEncoder),
            case_id=case_id,
            census_record_id=census_record_id,
            application_status=EnrollmentApplication.APPLICATION_STATUS_ENROLLED,
            agent_code=agent_code,
            agent_name=agent_name,
            agent_id=agent_id,
            method=data['method'],
            payment_mode=data['payment_mode'],
            # Signing info
            signature_time=signature_time,
            signature_city=data['enrollCity'],
            signature_state=data['enrollState'],
            identity_token=data['identityToken'],
            identity_token_type=data['identityType'],
            # Owner
            is_employee_owner=data['employee_owner'] != 'other',
            employee_other_owner_name=data['employee_other_owner_name'],
            employee_other_owner_ssn=self._strip_ssn(
                data['employee_other_owner_ssn']),
            is_spouse_owner=data['spouse_owner'] != 'other',
            spouse_other_owner_name=data['spouse_other_owner_name'],
            spouse_other_owner_ssn=self._strip_ssn(
                data['spouse_other_owner_ssn']),
            # emp beneficiary
            is_employee_beneficiary_spouse=(data['employee_beneficiary'] ==
                                              'spouse'),
            employee_beneficiary_name=emp_beneficiary_name,
            employee_beneficiary_ssn=emp_beneficiary_ssn,
            employee_beneficiary_relationship=emp_beneficiary_relation,
            employee_beneficiary_birthdate=emp_beneficiary_dob,
            # spouse beneficiary
            is_spouse_beneficiary_employee=(data['spouse_beneficiary'] ==
                                              'spouse'),
            spouse_beneficiary_name=sp_beneficiary_name,
            spouse_beneficiary_ssn=sp_beneficiary_ssn,
            spouse_beneficiary_relationship=sp_beneficiary_relation,
            spouse_beneficiary_birthdate=sp_beneficiary_dob,
        )
        return self.create(**enrollment_data)

    def _strip_ssn(self, ssn):
        return ssn.replace('-', '').strip() if ssn else ''

    def _save_coverages(self, enrollment, data):
        if data['did_decline']:
            return
        product_data = data['product_data']
        product = self.product_service.get(product_data['id'])

        if data['employee_coverage']:
            self.coverages_service.create_coverage(
                enrollment, product, data, data['employee'],
                data['employee_coverage'],
                EnrollmentApplicationCoverage.APPLICANT_TYPE_EMPLOYEE)
        if data['spouse_coverage']:
            self.coverages_service.create_coverage(
                enrollment, product, data, data['spouse'],
                data['spouse_coverage'],
                EnrollmentApplicationCoverage.APPLICANT_TYPE_SPOUSE)
        if data['child_coverages'] and data['child_coverages'][0]:
            self.coverages_service.create_coverage(
                enrollment, product, data, data['children'][0],
                data['child_coverages'][0],
                EnrollmentApplicationCoverage.APPLICANT_TYPE_CHILD)
        db.session.flush()

    # Reports
    def get_enrollment_report(self, case):
        return self.report_service.get_enrollment_report(case)

    def get_enrollment_records_for_census(self, case, census_record_id):
        """
        Does not do any combining data.
        Includes census data for each enrollment, so the same employee in the
        census will show up multiple times, once for each enrollment.
        """
        census_record = self.case_service.get_census_record(case, census_record_id)
        data = []
        if not census_record.enrollment_applications:
            return data
        for enrollment in census_record.enrollment_applications:
            export_record = dict()
            export_record.update(self.get_census_data(census_record))
            export_record.update(self.get_unmerged_enrollment_data(
                census_record, enrollment))
            data.append(export_record)
        return data

    def get_all_enrollment_records(self, case):
        """
        """
        census_records = self.case_service.get_census_records(case)
        return self.get_enrollment_records_for_census_records(census_records)

    def get_enrollment_records_for_census_records(self, census_records):
        """
        Does not do any combining data.
        Includes census data for each enrollment, so the same employee in the
        census will show up multiple times, once for each enrollment.
        """
        data = []
        for census_record in census_records:
            # Export only records with enrollments
            if not census_record.enrollment_applications:
                continue
            for enrollment in census_record.enrollment_applications:
                export_record = dict()
                export_record.update(self.get_census_data(census_record))
                export_record.update(self.get_unmerged_enrollment_data(
                    census_record, enrollment))
                data.append(export_record)
        return data

    def get_enrollment_status(self, census_record):
        # Get the flattened enrollment record
        enrollment_data = self.get_enrollment_data(census_record)
        return (enrollment_data['application_status']
                if enrollment_data else None)

    def get_census_data(self, census_record):
        return census_record.to_json()

    def get_enrollment_data(self, census_record):
        # TODO: Only get_enrollment_status is using this right now,
        #        should merge these functions and remove extraneous code
        #        since no other code needs a merged record
        enrollment_data = {}
        if not census_record.enrollment_applications:
            return None
        # Get the most recent enrollment for the generic data
        enrollment = max(census_record.enrollment_applications,
                         key=lambda e: e.signature_time)
        # Export data from enrollment
        for col in enrollment_columns:
            enrollment_data[col.get_field_name()] = col.get_value(enrollment)
        # Add Coverage data
        coverages = []
        for e in census_record.enrollment_applications:
            coverages += e.coverages
        employee_coverage = (
            self.find_most_recent_coverage_by_product_for_applicant_type(
                coverages,
                EnrollmentApplicationCoverage.APPLICANT_TYPE_EMPLOYEE
            ))
        spouse_coverage = (
            self.find_most_recent_coverage_by_product_for_applicant_type(
                coverages,
                EnrollmentApplicationCoverage.APPLICANT_TYPE_SPOUSE
            ))
        children_coverage = (
            self.find_most_recent_coverage_by_product_for_applicant_type(
                coverages,
                EnrollmentApplicationCoverage.APPLICANT_TYPE_CHILD
            ))
        # Include total annualized premium also
        total_annual_premium = Decimal('0.00')
        # Export coverages for at most six products
        product_list = self.case_service.get_products_for_case(enrollment.case)
        for x in range(6):
            if x < len(product_list):
                product = product_list[x]
            else:
                product = None
            prefix = 'product_{0}'.format(x + 1)
            product_data = {
                '{}_name'.format(prefix): product.name if product else ''
            }
            for applicant_abbr, applicant_coverages in (('emp',
                                                         employee_coverage),
                                                        ('sp',
                                                         spouse_coverage),
                                                        ('ch',
                                                         children_coverage)):
                coverage = (applicant_coverages[product].coverage_face_value
                            if applicant_coverages.get(product) else '')
                premium = (applicant_coverages[product].get_annualized_premium()
                           if applicant_coverages.get(product) else '')
                product_data.update({
                    '{}_{}_coverage'.format(prefix, applicant_abbr):
                        coverage,
                    '{}_{}_annual_premium'.format(prefix, applicant_abbr):
                        premium,
                })
                if premium and premium > Decimal('0.00'):
                    total_annual_premium += premium
            enrollment_data.update(product_data)
        enrollment_data['total_annual_premium'] = total_annual_premium
        return enrollment_data

    def get_unmerged_enrollment_data(self, census_record, enrollment):
        """
        If we are not merging, we know we are dealing with coverages from
        a single enrollment.
        """
        enrollment_data = {}
        if not census_record.enrollment_applications or not enrollment:
            return None

        # Export data from enrollment
        for col in enrollment_columns:
            enrollment_data[col.get_field_name()] = col.get_value(enrollment)

        # Add Coverage data
        coverages = enrollment.coverages
        employee_coverage = self.find_first_coverage_by_product_for_applicant_type(
            coverages, EnrollmentApplicationCoverage.APPLICANT_TYPE_EMPLOYEE)
        spouse_coverage = self.find_first_coverage_by_product_for_applicant_type(
            coverages, EnrollmentApplicationCoverage.APPLICANT_TYPE_SPOUSE)
        children_coverage = self.find_first_coverage_by_product_for_applicant_type(
            coverages, EnrollmentApplicationCoverage.APPLICANT_TYPE_CHILD)

        # Include the calculated total annualized premium also
        total_annual_premium = Decimal('0.00')

        # Export coverages for at most six products
        product_list = self.case_service.get_products_for_case(enrollment.case)
        for x in range(6):
            if x < len(product_list):
                product = product_list[x]
            else:
                product = None
            prefix = 'product_{0}'.format(x + 1)
            product_data = {'{}_name'.format(prefix): product.name if product else ''}

            total_product_premium = Decimal('0.00')
            for applicant_abbr, applicant_coverages in (('emp',
                                                         employee_coverage),
                                                        ('sp',
                                                         spouse_coverage),
                                                        ('ch',
                                                         children_coverage)):
                if applicant_coverages.get(product):
                    applicant_coverage = applicant_coverages[product]
                    coverage = applicant_coverage.coverage_face_value
                    premium = applicant_coverage.get_premium()
                    annualized_premium = applicant_coverage.get_annualized_premium()
                else:
                    coverage = ''
                    premium = ''
                    annualized_premium = ''

                product_data.update({
                    '{}_{}_coverage'.format(prefix, applicant_abbr): coverage,
                    '{}_{}_annual_premium'.format(prefix, applicant_abbr): annualized_premium,
                    '{}_{}_premium'.format(prefix, applicant_abbr): premium,
                })

                # Update totals
                if premium and premium > Decimal('0.00'):
                    total_product_premium += premium

                if annualized_premium and annualized_premium > Decimal('0.00'):
                    total_annual_premium += annualized_premium

            enrollment_data.update(product_data)
            enrollment_data['{}_total_premium'.format(prefix)] = total_product_premium

        enrollment_data['total_annual_premium'] = total_annual_premium

        return enrollment_data

    def find_most_recent_coverage_by_product_for_applicant_type(self,
                                                                all_coverages,
                                                                applicant_type):
        applicant_coverages = filter_applicant_coverages(all_coverages,
                                                         applicant_type)
        coverages_by_product = group_coverages_by_product(applicant_coverages)
        # Pull out the most recent for each product
        return {
            p: select_most_recent_coverage(coverages)
            for p, coverages in coverages_by_product.iteritems()
        }

    def find_first_coverage_by_product_for_applicant_type(self, all_coverages,
                                                          applicant_type):
        applicant_coverages = filter_applicant_coverages(all_coverages,
                                                         applicant_type)
        coverages_by_product = group_coverages_by_product(applicant_coverages)
        # There should be at most one coverage since we should be dealing with
        #  coverages from a single enrollment application.
        return {
            p: coverages[0]
            for p, coverages in coverages_by_product.iteritems()
            if coverages
        }

    def export_enrollment_data(self, data):
        stream = StringIO.StringIO()
        writer = csv.writer(stream)
        # Write the header row
        writer.writerow(self.get_csv_headers())
        # Write all the data
        for record in data:
            writer.writerow(self.get_csv_row(record))
        return stream.getvalue()

    def get_csv_headers(self):
        # enrollment columns, then Census columns
        headers = []
        headers += [c.column_title for c in enrollment_columns]
        headers += [c.column_title for c in coverage_columns]
        headers += self.case_service.census_records.get_csv_headers()
        return headers

    def get_csv_row(self, record):
        row = []
        # Add enrollment record export
        row += [c.get_value(record) for c in enrollment_columns]
        # Add coverage records
        row += [c.get_value(record) for c in coverage_columns]
        # Add census record export
        row += self.case_service.census_records.get_csv_row_from_dict(record)
        return row



def export_string(val):
    return val.strip()


def export_date(val):
    if not val:
        return ''
    return dateutil.parser.parse(val).strftime('%F')


def export_ssn(self, val):
    if not val:
        return ''
    elif len(val) == 9:
        return val[:4]+'-'+val[4:6]+'-'+val[6:]

    return val


class EnrollmentColumn(object):
    def __init__(self, field_name, column_title, export_func, accessor=None):
        self.field_name = field_name
        self.column_title = column_title
        self.export_func = export_func
        self.accessor = accessor

    def get_value(self, record):
        """
        Pull the value for this column out of the given record
        """
        if self.accessor:
            val = self.accessor(record)
        else:
            try:
                val = getattr(record, self.field_name)
            except AttributeError:
                val = record.get(self.field_name)
        return val

    def get_field_name(self):
        return self.field_name


enrollment_columns = [
    EnrollmentColumn('signature_time', 'Timestamp', export_date),
    EnrollmentColumn('application_status', 'Status', export_string),
    EnrollmentColumn('agent_code', 'Agent Code', export_string),
    EnrollmentColumn('agent_name', 'Agent Name', export_string),
    EnrollmentColumn('signature_city', 'Signature City', export_string),
    EnrollmentColumn('signature_state', 'Signature State', export_string),
    EnrollmentColumn('identity_token', 'Date of Hire', export_string),
    EnrollmentColumn('payment_mode', 'Payment Mode', lambda x: x),
    EnrollmentColumn('method', 'Enrollment Method', export_string),
    EnrollmentColumn('is_employee_owner', 'Is Employee Owner', export_string),
    EnrollmentColumn('employee_other_owner_name', 'Other Owner Name', export_string),
    EnrollmentColumn('employee_other_owner_ssn', 'Other Owner SSN', export_ssn),
    EnrollmentColumn('spouse_other_owner_name', 'Spouse Other Owner', export_string),
    EnrollmentColumn('spouse_other_owner_ssn', 'Spouse Other Owner SSN', export_ssn),
    EnrollmentColumn('is_employee_beneficiary_spouse', 'Is Employee Beneficiary Spouse', export_string),
    EnrollmentColumn('employee_beneficiary_name', 'Employee Beneficiary Name', export_string),
    EnrollmentColumn('employee_beneficiary_relationship', 'Employee Beneficiary Relationship', export_string),
    EnrollmentColumn('employee_beneficiary_birthdate', 'Employee Beneficiary Birthdate', export_date),
    EnrollmentColumn('employee_beneficiary_ssn', 'Employee Beneficiary SSN', export_ssn),
    EnrollmentColumn('is_spouse_beneficiary_employee', 'Is Spouse Beneficiary Employee', export_string),
    EnrollmentColumn('spouse_beneficiary_name', 'Spouse Beneficiary Name', export_string),
    EnrollmentColumn('spouse_beneficiary_relationship', 'Spouse Beneficiary Relationship', export_string),
    EnrollmentColumn('spouse_beneficiary_birthdate', 'Spouse Beneficiary Birthdate', export_date),
    EnrollmentColumn('spouse_beneficiary_ssn', 'Spouse Beneficiary SSN', export_ssn),
]


# Include columns for the coverage/premium information for up to six products
coverage_columns = [EnrollmentColumn('total_annual_premium', 'Total Annual Premium', export_string)]
for product_num in range(1, 6+1):
    product_coverage_cols = [
        EnrollmentColumn('product_{}_name'.format(product_num),
                         'Product {} Name'.format(product_num),
                         export_string),
        EnrollmentColumn('product_{}_total_premium'.format(product_num),
                         'Product {} Total Modal Premium'.format(product_num), export_string),
    ]
    for dependent_abbr, dependent_title in (('emp', 'Employee'),
                                            ('sp', 'Spouse'),
                                            ('ch', 'Child')):
        product_coverage_cols += [
            EnrollmentColumn('product_{}_{}_coverage'.format(product_num,
                                                             dependent_abbr),
                             'Product {} {} Coverage'.format(product_num,
                                                             dependent_abbr.upper()),
                             export_string),
            EnrollmentColumn('product_{}_{}_premium'.format(product_num,
                                                             dependent_abbr),
                             'Product {} {} Premium'.format(product_num,
                                                             dependent_abbr.upper()),
                             export_string),
        ]
    coverage_columns += product_coverage_cols
