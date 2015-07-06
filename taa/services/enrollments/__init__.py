from collections import defaultdict
from decimal import Decimal
import csv
import datetime
import dateutil.parser
import json
import re
import StringIO
import time
import unicodedata
import uuid

import requests
import mandrill

from flask import render_template

from taa import mandrill_flask
from taa.core import DBService
from taa.core import db
from models import (EnrollmentApplication, EnrollmentApplicationCoverage,
                    SelfEnrollmentEmailLog, SelfEnrollmentLink, SelfEnrollmentEmailBatch, SelfEnrollmentEmailBatchWithEmails)
from taa.services.cases.models import SelfEnrollmentSetup
from taa.services.cases import CaseService
from taa.services.products import ProductService
from taa.services.agents import AgentService
case_service = CaseService()
product_service = ProductService()
agent_service = AgentService()


class EnrollmentApplicationService(DBService):
    __model__ = EnrollmentApplication

    def __init__(self, *args, **kwargs):
        super(EnrollmentApplicationService, self).__init__(*args, **kwargs)
        self.coverages_service = EnrollmentApplicationCoverageService()
        self.report_service = EnrollmentReportService()

    def save_enrollment_data(self, data, census_record, agent):
        # Update the census record data with the new data
        if census_record:
            case_service.update_census_record_from_enrollment(census_record,
                                                              data)
        else:
            census_record = case_service.create_ad_hoc_census_record(case=None,
                                                                     data=data)
        # Store extra enrollment data on the enrollment
        enrollment = self._create_enrollment(census_record, data, agent)
        # Save coverages
        self._save_coverages(enrollment, data)
        return enrollment

    def delete_case_enrollment_data(self, case):
        for census_record in case.census_records:
            self.delete_enrollment_data(census_record)

    def delete_enrollment_data(self, census_record):
        for enrollment_application in census_record.enrollment_applications:
            for coverage in enrollment_application.coverages:
                self.coverages_service.delete(coverage)
            self.delete(enrollment_application)

    def _create_enrollment(self, census_record, data, agent):
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

        enrollment_data = dict(
            case_id=case_id,
            census_record_id=census_record_id,
            application_status=EnrollmentApplication.APPLICATION_STATUS_ENROLLED,
            agent_code=agent_code,
            agent_name=agent_name,
            agent_id=agent_id,
            method=data['method'],
            payment_mode=data['payment_mode'],
            # Signing info
            signature_time = datetime.datetime.now(),
            signature_city = data['enrollCity'],
            signature_state = data['enrollState'],
            identity_token = data['identityToken'],
            identity_token_type = data['identityType'],
            # Owner
            is_employee_owner = data['employee_owner'] != 'other',
            employee_other_owner_name = data['employee_other_owner_name'],
            employee_other_owner_ssn = self._strip_ssn(
                data['employee_other_owner_ssn']),
            is_spouse_owner = data['spouse_owner'] != 'other',
            spouse_other_owner_name = data['spouse_other_owner_name'],
            spouse_other_owner_ssn = self._strip_ssn(
                data['spouse_other_owner_ssn']),
            # emp beneficiary
            is_employee_beneficiary_spouse = (data['employee_beneficiary'] ==
                                              'spouse'),
            employee_beneficiary_name = emp_beneficiary_name,
            employee_beneficiary_ssn = emp_beneficiary_ssn,
            employee_beneficiary_relationship = emp_beneficiary_relation,
            employee_beneficiary_birthdate = emp_beneficiary_dob,
            # spouse beneficiary
            is_spouse_beneficiary_employee = (data['spouse_beneficiary'] ==
                                              'spouse'),
            spouse_beneficiary_name = sp_beneficiary_name,
            spouse_beneficiary_ssn = sp_beneficiary_ssn,
            spouse_beneficiary_relationship = sp_beneficiary_relation,
            spouse_beneficiary_birthdate = sp_beneficiary_dob,
        )
        return self.create(**enrollment_data)

    def _strip_ssn(self, ssn):
        return ssn.replace('-', '').strip() if ssn else ''

    def _save_coverages(self, enrollment, data):
        if data['did_decline']:
            return
        product_data = data['product_data']
        product = product_service.get(product_data['id'])

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

    def get_enrollment_records(self, case):
        """
        Combine all the census data with enrollment data (exclude census
        records without enrollments).
        For multiple enrollments, combine the coverages.
        If a coverage overlaps another, use the latest one.
        """
        census_records = CaseService().get_census_records(case)
        data = []
        for census_record in census_records:
            # Export only records with enrollments
            if not census_record.enrollment_applications:
                continue
            export_record = dict()
            export_record.update(self.get_census_data(census_record))
            export_record.update(self.get_enrollment_data(census_record))
            data.append(export_record)
        return data

    def get_enrollment_record_for_census(self, case, census_record_id):
        """
        Does not do any combining data.
        Includes census data for each enrollment, so the same employee in the
        census will show up multiple times, once for each enrollment.
        """
        census_record = CaseService().get_census_record(case, census_record_id)
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
        Does not do any combining data.
        Includes census data for each enrollment, so the same employee in the
        census will show up multiple times, once for each enrollment.
        """
        census_records = CaseService().get_census_records(case)
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
        product_list = case_service.get_products_for_case(enrollment.case)
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
        single enrollment.
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
        # Include total annualized premium also
        total_annual_premium = Decimal('0.00')
        # Export coverages for at most six products
        product_list = case_service.get_products_for_case(enrollment.case)
        for x in range(6):
            if x < len(product_list):
                product = product_list[x]
            else:
                product = None
            prefix = 'product_{0}'.format(x + 1)
            product_data = {'{}_name'.format(prefix): product.name
                            if product else ''}
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
        headers += CaseService().census_records.get_csv_headers()
        return headers

    def get_csv_row(self, record):
        row = []
        # Add enrollment record export
        row += [c.get_value(record) for c in enrollment_columns]
        # Add coverage records
        row += [c.get_value(record) for c in coverage_columns]
        # Add census record export
        row += CaseService().census_records.get_csv_row_from_dict(record)
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
    EnrollmentColumn('identity_token', 'Identity Token', export_string),
    EnrollmentColumn('identity_token_type', 'Token Type', export_string),
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
coverage_columns = []
for product_num in range(1, 6+1):
    product_coverage_cols = [
        EnrollmentColumn('product_{}_name'.format(product_num),
                         'Product {} Name'.format(product_num),
                         export_string)
    ]
    for dependent_abbr, dependent_title in (('emp', 'Employee'),
                                            ('sp', 'Spouse'),
                                            ('ch', 'Child')):
        product_coverage_cols += [
            EnrollmentColumn('product_{}_{}_coverage'.format(product_num,
                                                             dependent_abbr),
                             'Product {} {} Coverage'.format(product_num,
                                                             dependent_abbr),
                             export_string),
            EnrollmentColumn('product_{}_{}_annual_premium'.format(product_num,
                                                                   dependent_abbr),
                             'Product {} {} Annual Premium'.format(product_num,
                                                                   dependent_abbr),
                             export_string),
        ]
    coverage_columns += product_coverage_cols


class EnrollmentApplicationCoverageService(DBService):
    __model__ = EnrollmentApplicationCoverage

    def create_coverage(self, enrollment, product, data, applicant_data,
                        applicant_coverage, applicant_type):
        # Set proper premium
        payment_mode = data.get('payment_mode')
        weekly_premium = None
        biweekly_premium = None
        semimonthly_premium = None
        monthly_premium = None
        if payment_mode == 52:
            weekly_premium = applicant_coverage['premium']
        elif payment_mode == 26:
            biweekly_premium = applicant_coverage['premium']
        elif payment_mode == 24:
            semimonthly_premium = applicant_coverage['premium']
        elif payment_mode == 12:
            monthly_premium = applicant_coverage['premium']
        else:
            raise Exception("Invalid payment mode %s"%payment_mode)

        return self.create(**dict(
            coverage_status=EnrollmentApplicationCoverage.COVERAGE_STATUS_ENROLLED,
            enrollment_application_id=enrollment.id,
            product_id=product.id,
            applicant_type=applicant_type,
            height_inches=(applicant_data['height']
                           if applicant_data['height'] else None),
            weight_pounds=(applicant_data['weight']
                           if applicant_data['weight'] else None),
            is_smoker=applicant_data.get('is_smoker'),
            coverage_face_value=applicant_coverage['face_value'],
            weekly_premium=weekly_premium,
            biweekly_premium=biweekly_premium,
            semimonthly_premium=semimonthly_premium,
            monthly_premium=monthly_premium,
            soh_answers=json.dumps(dict(
                existing_insurance=data['existing_insurance'],
                replacing_insurance=data['replacing_insurance'],
                health_questions=applicant_data['soh_questions'],
            ))
        ))

    def get_coverages_for_employee(self, census_record):
        coverages = self.get_census_record_coverages(census_record)
        return filter_applicant_coverages(coverages,
                                          EnrollmentApplicationCoverage.APPLICANT_TYPE_EMPLOYEE)

    def get_coverages_for_spouse(self, census_record):
        coverages = self.get_census_record_coverages(census_record)
        return filter_applicant_coverages(coverages,
                                          EnrollmentApplicationCoverage.APPLICANT_TYPE_SPOUSE)

    def get_coverages_for_children(self, census_record):
        coverages = self.get_census_record_coverages(census_record)
        return filter_applicant_coverages(coverages,
                                          EnrollmentApplicationCoverage.APPLICANT_TYPE_CHILD)

    def get_census_record_coverages(self, census_record):
        coverages = []
        for app in census_record.enrollment_applications:
            coverages += app.coverages
        return coverages


def merge_enrollments(census_record):
    all_coverages = []
    for enrollment in census_record.enrollment_applications:
        all_coverages += enrollment.coverages
    if census_record.enrollment_applications:
        most_recent_enrollment = max(census_record.enrollment_applications,
                                     key=lambda e: e.signature_time)
    else:
        most_recent_enrollment = None
    return {
        'enrollment': most_recent_enrollment,
        'coverages': merge_enrollment_application_coverages(all_coverages),
    }


class EnrollmentReportService(object):
    def get_enrollment_report(self, case):
        report_data = {}
        census_records = CaseService().get_census_records(case)
        merged_enrollment_applications = [merge_enrollments(census_record)
                                          for census_record in census_records]
        enrollment_applications = []
        for census_record in census_records:
            enrollment_applications += [dict(enrollment=e,
                                             coverages=group_coverages_by_product(e.coverages))
                                        for e in census_record.enrollment_applications]
        report_data['company_name'] = case.company_name
        # Enrollment methods used
        report_data['enrollment_methods'] = self._find_enrollment_methods(enrollment_applications)
        # Enrollment period (most recent)
        if case:
            report_data['enrollment_period'] = self._get_enrollment_period_dates(case)
        else:
            report_data['enrollment_period'] = None
        stats = self.get_product_statistics(enrollment_applications)
        report_data['summary'] = self.build_report_summary(stats, case,
                                                           merged_enrollment_applications,
                                                           enrollment_applications)
        # Product data
        report_data['product_report'] = self.build_report_by_product(stats)
        # Agents on case
        report_data['case_owner'] = case_service.get_case_owner(case)
        report_data['case_agents'] = case_service.get_case_partner_agents(case)
        return report_data

    def _find_enrollment_methods(self, case_enrollments):
        return list({e['enrollment'].method for e in case_enrollments
                     if e['enrollment'] and
                     self._is_enrollment_finished(e['enrollment'])})

    def _is_enrollment_finished(self, e):
        return e.application_status in [
            EnrollmentApplication.APPLICATION_STATUS_ENROLLED,
            EnrollmentApplication.APPLICATION_STATUS_DECLINED
        ]

    def _get_enrollment_period_dates(self, case):
        most_recent = case_service.get_most_recent_enrollment_period(case)
        if not most_recent:
            start = None,
            end = None
        else:
            start = most_recent.get_start_date()
            end = most_recent.get_end_date()
        return dict(start=start, end=end)

    def build_report_summary(self, stats, case, merged_enrollment_applications,
                             unmerged_enrollment_applications):
        """
        Summary data
          - % (#) processed (Enrolled + Declined) (for uploaded census only)
          - % (#) taken (status=Enrolled (not Declined or blank)) (for uploaded census only)
          - total annualized premium
        """
        if self._is_uploaded_census(merged_enrollment_applications):
            return dict(
                processed_enrollments=self.get_num_processed_enrollments(
                    merged_enrollment_applications),
                taken_enrollments=self.get_num_taken_enrollments(
                    merged_enrollment_applications),
                declined_enrollments=self.get_num_declined_enrollments(
                    merged_enrollment_applications),
                total_census=self.get_num_census_records(
                    merged_enrollment_applications),
                total_annualized_premium=self.get_total_annualized_premium(
                    unmerged_enrollment_applications),
                is_census_report=True,
                only_one_product=self.has_only_one_product(case),
                product_names=self.get_product_names(case),
            )
        else:
            return dict(
                processed_enrollments=self.get_num_processed_enrollments(
                    merged_enrollment_applications),
                total_census=self.get_num_census_records(
                    merged_enrollment_applications),
                total_annualized_premium = self.get_total_annualized_premium(
                    unmerged_enrollment_applications),
                is_census_report=False,
                only_one_product=self.has_only_one_product(case),
                product_names=self.get_product_names(case),
            )

    def has_only_one_product(self, case):
        return len(case.products) == 1

    def get_product_names(self, case):
        return [p.name for p in case.products]

    def _is_uploaded_census(self, merged_enrollment_applications):
        """
        If any census record is uploaded, we consider the whole thing
        a census-enrolled case (Zach's guess)
        """
        return any(map(lambda e:
                       (e['enrollment'].census_record.is_uploaded_census
                        if e['enrollment'] else False),
                       merged_enrollment_applications))

    def get_num_processed_enrollments(self, merged_enrollment_applications):
        return sum(1 for enrollment in merged_enrollment_applications
                   if enrollment['enrollment'] and
                   enrollment['enrollment'].did_process())

    def get_num_taken_enrollments(self, merged_enrollment_applications):
        return sum(1 for e in merged_enrollment_applications
                   if e['enrollment'] and e['enrollment'].did_enroll())

    def get_num_declined_enrollments(self, merged_enrollment_applications):
        return sum(1 for e in merged_enrollment_applications
                   if e['enrollment'] and not e['enrollment'].did_enroll())

    def get_total_annualized_premium(self, unmerged_enrollment_applications):
        total = Decimal('0.00')
        for e in unmerged_enrollment_applications:
            all_coverages = []
            for product, coverages in e['coverages'].iteritems():
                all_coverages += coverages
            total += sum(map(lambda c: c.get_annualized_premium(),
                             all_coverages))
        return total

    def get_num_census_records(self, records):
        return sum(1 for e in records)

    def build_report_by_product(self, stats):
        """
        For each product,
        product name, (%) of census, $ of annual premium
        Example:

        product: count (% of census), total annual premium
        FPP-TI: 128 (27%), $32,577
        Critical Illness: 47 (10%), $22,913
        """
        product_report_data = {}
        for product in stats.get_products_used():
            product_report_data[product.id] = dict(
                product_name=product.name,
                enrolled_count=stats.get_taken_count_for_product(product),
                total_annualized_premium=stats.get_annual_premium_for_product(
                    product),
            )
        return product_report_data

    def get_product_statistics(self, merged_enrollment_applications):
        stats = ProductStatsAccumulator()
        for enrollment_application in merged_enrollment_applications:
            stats.add_coverages_for_enrollment_application(
                enrollment_application)
        return stats


def merge_enrollment_application_coverages(all_coverages):
    """
    Each time the application is filled out, one or more coverages are added
    for this enrollment_application record.

    We want, for each applicant type (emp, spouse, children), the most recent
    coverage by product.

    So, if an employee signs up twice for the same product, we want only the
    latest coverage.
    """
    merged_coverages_by_product = defaultdict(list)
    for applicant_type in [
            EnrollmentApplicationCoverage.APPLICANT_TYPE_EMPLOYEE,
            EnrollmentApplicationCoverage.APPLICANT_TYPE_SPOUSE,
            EnrollmentApplicationCoverage.APPLICANT_TYPE_CHILD]:
        applicant_coverages = filter_applicant_coverages(all_coverages,
                                                         applicant_type)
        coverages_by_product = group_coverages_by_product(applicant_coverages)
        most_recent_coverage_by_product = {
            p: select_most_recent_coverage(coverages)
            for p, coverages in coverages_by_product.iteritems()
        }
        for product, coverage in most_recent_coverage_by_product.iteritems():
            merged_coverages_by_product[product].append(coverage)
    return merged_coverages_by_product


def select_most_recent_coverage(coverages):
    if not coverages:
        return None
    # Return most recently signed coverage out of the groups of coverages
    return max(coverages, key=lambda c: c.enrollment.signature_time)


def filter_applicant_coverages(coverages, applicant_type):
    return filter(lambda c: c.applicant_type == applicant_type, coverages)


def group_coverages_by_product(coverages):
    """
    Groups coverages by their product type.
    Returns a dictionary with products as keys and a list of coverages as values
    """
    product_coverages = defaultdict(list)
    for coverage in coverages:
        product_coverages[coverage.product].append(coverage)
    return product_coverages


class ProductStatsAccumulator(object):
    """
    Gathers the stats needed for the coverages on an application for display
    on reports
    """
    def __init__(self):
        self._product_premiums = defaultdict(Decimal)
        self._product_counts = defaultdict(int)
        #self._product_declines = defaultdict(int)

    def add_coverages_for_enrollment_application(self,
                                                 merged_enrollment_application):
        # Count taken products
        for product in merged_enrollment_application['coverages']:
            self._product_counts[product] += 1
        # Count declined products - TODO: when multiproduct is added,
        # individual products may be declined
        # if merged_enrollment_application['enrollment'] and not merged_enrollment_application['enrollment'].did_enroll():
        #     self._product_declines[product] += 1

        # Count annualized premiums by product
        for product, coverages in merged_enrollment_application['coverages'].iteritems():
            self._product_premiums[product] += sum(
                coverage.get_annualized_premium() for coverage in coverages)

    def get_products_used(self):
        return self._product_counts.keys()

    def get_taken_count_for_product(self, product):
        if product not in self._product_counts:
            return None
        return self._product_counts[product]

    def get_annual_premium_for_product(self, product):
        if product not in self._product_premiums:
            return None
        return self._product_premiums[product]


class SelfEnrollmentLinkService(DBService):
    __model__ = SelfEnrollmentLink

    def get_for_census_record(self, census_record):
        # record = CaseService.get_census_record(census_record_id)
        return self.find(census_record_id=census_record.id).first()

    @staticmethod
    def _slugify(s):
        slug = unicodedata.normalize('NFKD', s)
        slug = slug.encode('ascii', 'ignore').lower()
        slug = re.sub(r'[^a-z0-9]+', '-', slug).strip('-')
        slug = re.sub(r'[-]+', '-', slug)
        return slug

    def generate_link(self, prefix, case, record=None):
        url = '{}self-enroll/{}/{}'.format(
            prefix,
            SelfEnrollmentLinkService._slugify(case.company_name or ''),
            uuid.uuid4().hex)
        link = self.create(
            self_enrollment_setup_id=case.self_enrollment_setup.id,
            census_record_id=None if record is None else record.id,
            url=url)
        db.session.commit()
        return link

    def get_self_enrollment_data_for(self, uuid, increment_clicks=True):
        """
        :param uuid: the unique id at the end of self-enroll links
        :param increment_clicks: whether or not to count this as a page view
        :return: The enrollment setup object and the linked census record, if any
        """
        link = self.get_link_by_uuid(uuid)
        if link is None or not link.is_active():
            # Link no longer exists or is not active.
            return None, None

        if increment_clicks:
            link.clicks += 1
            db.session.commit()
        return link.self_enrollment_setup, link.census_record

    def get_link_by_uuid(self, uuid):
        return db.session.query(SelfEnrollmentLink).filter(
            SelfEnrollmentLink.url.endswith(uuid)).first()

    def get_case_for_link(self, uuid):
        link = self.get_link_by_uuid(uuid)
        if not link or not link.is_linked():
            return None

        return link.self_enrollment_setup.case

    def get_generic_link(self, prefix, case):
        link = db.session.query(SelfEnrollmentLink
            ).filter_by(census_record_id=None
            ).filter(SelfEnrollmentLink.self_enrollment_setup.has(SelfEnrollmentSetup.case_id==case.id)
            ).first()

        if not link and not case.is_self_enrollment:
            return None

        if not link:
            link = self.generate_link(prefix, case, record=None)

        return link.url

def build_email_greeting(record, setup):
    salutation = ''
    if setup.email_greeting_salutation:
        salutation = '{} '.format(setup.email_greeting_salutation)
    greeting_end = ''
    if setup.email_greeting_type == SelfEnrollmentSetup.EMAIL_GREETING_FIRST_NAME:
        greeting_end = "{},".format(record.employee_first)
    elif setup.email_greeting_type == SelfEnrollmentSetup.EMAIL_GREETING_FULL_NAME:
        greeting_end = "{} {},".format(record.employee_first, record.employee_last)
    elif setup.email_greeting_type == SelfEnrollmentSetup.EMAIL_GREETING_LAST_NAME:
        greeting_end = "{},".format(record.employee_last)
    elif setup.email_greeting_type == SelfEnrollmentSetup.EMAIL_GREETING_TITLE_LAST:
        if not record.employee_gender:
            title = 'Mr./Ms.'
        elif record.employee_gender.lower()[0] == 'm':
            title = 'Mr.'
        else:
            title = 'Mrs.'

        greeting_end = "{} {},".format(title, record.employee_last)
    elif setup.email_greeting_type == SelfEnrollmentSetup.EMAIL_GREETING_BLANK:
        greeting_end = ''
    greeting = "{}{}".format(salutation, greeting_end)
    return greeting

class SelfEnrollmentEmailBatchService(DBService):
    __model__ = SelfEnrollmentEmailBatch


class SelfEnrollmentEmailService(DBService):
    __model__ = SelfEnrollmentEmailLog

    def get_batches_for_case(self, case):
        return db.session.query(SelfEnrollmentEmailBatch
        ).filter(SelfEnrollmentEmailBatch.case_id==case.id
        ).all()

    def get_batch_for_case(self, case, batch_id):
        return db.session.query(SelfEnrollmentEmailBatchWithEmails
        ).get(batch_id)

    def create_batch_for_case(self, case, census_records):
        self_enrollment_link_service = SelfEnrollmentLinkService()
        self_enrollment_batch_service = SelfEnrollmentEmailBatchService()
        setup = case.self_enrollment_setup
        agent = agent_service.get_logged_in_agent()


        batch = self_enrollment_batch_service.create(**dict(
            email_from_address=setup.email_sender_email,
            email_from_name=setup.email_sender_name,
            email_subject= setup.email_subject,
            email_body= setup.email_message,
            agent_id = agent.id,
            case_id = case.id
        ))

        results = []

        for record in census_records:
            if record.employee_email is None or '@' not in record.employee_email:
                # Census record does not has a valid email; TODO insert into email log in as failure.
                #results.append("{} {} did not have a valid email.".format(record.employee_first, record.employee_last))
                continue

            # Get previously generated link if available
            link = self_enrollment_link_service.get_for_census_record(record)
            if link is None:
                # Otherwise generate one
                link = self_enrollment_link_service.generate_link(request.url_root,
                                                                  case, record)
            if link is None:
                abort(500, "Could not retrieve or create self-enrollment link")

            name = '{} {}'.format(record.employee_first, record.employee_last)
            #print("Emailing {}".format(name))

            email_body = render_template(
                "emails/enrollment_email.html",
                custom_message=setup.email_message,
                greeting=build_email_greeting(record, setup),
                enrollment_url=link.url,
                company_name=record.case.company_name,
            )
            email_subject = setup.email_subject if setup.email_subject else 'Benefit Enrollment - your action needed'
            email_log = self.create_pending_email(
                agent, link, record, batch,
                to_email=record.employee_email,
                to_name=name
                )
            results.append(email_log)
        return results

    def get_for_census_record(self, census_record):
        # record = CaseService.get_census_record(census_record_id)
        # return self.find(census_id=census_record.id)
        return self.find(census_id=census_record.id).all()

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

    def send(self, agent, link, census, **kwargs):

        success = self._send_email(**kwargs)

        email_record = self.create(**dict(
            link_id=link.id,
            census_id=census.id,
            agent_id=agent.id,
            email_to_address=kwargs.get('to_email'),
            email_to_name=kwargs.get('to_name'),
            email_from_address=kwargs.get('from_email'),
            email_from_name=kwargs.get('from_name'),
            email_subject=kwargs.get('subject'),
            email_body=kwargs.get('body'),
            is_success=success,
        ))

        db.session.commit()
        return success

    def queue_email(self, email_log_id):
        "Sends a pending email log to the queue for sending"

        # Import the tasks module here so we don't have circular import.
        from taa import tasks
        # Queue up the task to be run in the background
        tasks.send_email.delay(email_log_id)


    def create_pending_email(self, agent, link, census, batch, **kwargs):
        # Create a pending record in the database
        return self.create(**dict(
            link_id=link.id,
            census_id=census.id,
            agent_id=agent.id,
            email_to_address=kwargs.get('to_email'),
            email_to_name=kwargs.get('to_name'),
            batch_id = batch.id,
            is_success=False,
            status=SelfEnrollmentEmailLog.STATUS_PENDING
        ))
