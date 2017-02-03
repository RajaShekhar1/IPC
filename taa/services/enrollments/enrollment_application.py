import StringIO
from datetime import date as date_type
from datetime import datetime
import json
from decimal import Decimal
from itertools import ifilter

import dateutil.parser
from taa.services.docusign.docusign_envelope import EnrollmentDataWrap

from taa.services.docusign.service import DocusignEnvelope

from enrollment_application_coverages import (
    filter_applicant_coverages,
    group_coverages_by_product,
    select_most_recent_coverage,
)
from taa.helpers import UnicodeCsvWriter
from models import EnrollmentApplication, EnrollmentApplicationCoverage, EnrollmentSubmission
from taa import JSONEncoder
from taa.core import DBService, db
from taa.services import RequiredFeature
from taa.services.cases import Case


def load_standardized_data_from_application(application):
    return json.loads(application.standardized_data)


class EnrollmentApplicationService(DBService):
    __model__ = EnrollmentApplication

    report_service = RequiredFeature('EnrollmentReportService')
    coverages_service = RequiredFeature('EnrollmentApplicationCoverageService')
    case_service = RequiredFeature('CaseService')
    product_service = RequiredFeature('ProductService')
    batch_item_service = RequiredFeature('EnrollmentImportBatchItemService')

    def get_enrollment_by_id(self, enrollment_application_id):
        return db.session.query(EnrollmentApplication).get(enrollment_application_id)

    def search_enrollments(self,
                           by_agent_id=None,
                           by_agent_ids=None,
                           by_envelope_url=None,
                           by_applicant_signing_status=None, by_agent_signing_status=None):
        
        q = db.session.query(EnrollmentApplication
                             ).filter(EnrollmentApplication.is_preview == False)
        
        q = q.options(db.eagerload('coverages').joinedload('product')
                      ).options(db.joinedload('census_record')
                                ).options(db.joinedload('case'))

        if by_envelope_url:
            q = q.filter(EnrollmentApplication.docusign_envelope_id == by_envelope_url)

        if by_agent_id:
            q = q.filter(EnrollmentApplication.agent_id == by_agent_id)

        if by_agent_ids:
            q = q.filter(EnrollmentApplication.agent_id.in_(by_agent_ids))

        if by_applicant_signing_status:
            q = q.filter(EnrollmentApplication.applicant_signing_status == by_applicant_signing_status)

        if by_agent_signing_status:
            q = q.filter(EnrollmentApplication.agent_signing_status == by_agent_signing_status)

        return q

    def save_enrollment_data(self, data, case, census_record, agent, received_data=None):
        """
        Save all the enrollment and coverage information, including creating a census record if required.
            - received_data is to record the raw imported data that we have not standardized
        """

        if not census_record:
            census_record = self.case_service.create_ad_hoc_census_record(case=case, data=data)
        else:
            # Update the census record data with the new data
            self.case_service.update_census_record_from_enrollment(census_record, data)

        # Store enrollment data on the enrollment record.
        enrollment = self._create_enrollment(census_record, data, agent, received_data=received_data)

        # Save coverages
        self._save_coverages(enrollment, data)

        return enrollment

    def save_multiproduct_enrollment_data(self, data, case, census_record, agent, received_data=None):
        """
        Same idea as save_enrollment_data, but adapted for an enrollment in multiple products.

        Create and save the enrollment data. Creates a census record if this is a generic link, and in
             either case updates the census record with the latest enrollment data.
        """

        first_product_data = data[0]

        if not census_record:
            census_record = self.case_service.create_ad_hoc_census_record(case=case, data=first_product_data)
        else:
            # Update the census record data with the new data
            self.case_service.update_census_record_from_enrollment(census_record, first_product_data)

        # Store enrollment data on the enrollment record.
        enrollment = self._create_enrollment(census_record, data, agent, received_data=received_data)

        # Save coverages
        self._save_coverages(enrollment, data)
        return enrollment

    def save_docusign_envelope(self, enrollment_application, envelope):
        """Records a reference to a DocuSign envelope on our application record in the database."""

        enrollment_application.docusign_envelope_id = envelope.uri
        enrollment_application.applicant_signing_status = EnrollmentApplication.SIGNING_STATUS_PENDING
        db.session.flush()

    def update_applicant_signing_status(self, enrollment_application, status=None):
        "Synchronizes our enrollment record with the signing and application statuses that DocuSign has."
        envelope = DocusignEnvelope(enrollment_application.docusign_envelope_id, enrollment_application)
        envelope.update_enrollment_status()

    def delete_case_enrollment_data(self, case):
        for census_record in case.census_records:
            self.delete_enrollment_data(census_record)

    def delete_enrollment_data(self, census_record):
        for enrollment_application in census_record.enrollment_applications:
            self.delete_enrollment_record(enrollment_application)

    def delete_enrollment_record(self, enrollment_application):
        # Remove coverage data
        for coverage in enrollment_application.coverages:
            self.coverages_service.delete(coverage)

        # Remove any import batch data
        self.batch_item_service.delete_for_enrollment(enrollment_application)
        
        # Remove any logs
        for log in enrollment_application.summary_emails:
            db.session.delete(log)
        
        # Remove the application data row
        self.delete(enrollment_application)

    def _create_enrollment(self, census_record, wizard_data, agent, received_data=None):

        # Link to census record and case
        case_id = census_record.case_id
        census_record_id = census_record.id

        # Link to agent if given
        if agent:
            agent_code = agent.agent_code
            agent_name = agent.name()
            agent_id = agent.id
        else:
            agent_code = None
            agent_name = None
            agent_id = None

        # Use the first record in a multiproduct setting to create the main enrollment record.
        application_status = self.get_application_status(census_record, wizard_data)
        signature_method = self.get_signature_method(census_record, wizard_data)

        data = self.get_first_wizard_data_record(wizard_data)
        first_data_wrap = EnrollmentDataWrap(data, census_record.case)

        is_preview = data.get('is_preview', False)
        given_sig_time = data.get('time_stamp')
        signature_time = given_sig_time if given_sig_time else datetime.now()
        created_time = datetime.now()
        
        if data['employee_beneficiary'] == 'spouse':
            emp_beneficiary_name = u'{} {}'.format(data['spouse']['first'],
                                                   data['spouse']['last'])
            emp_beneficiary_ssn = self._strip_ssn(data['spouse']['ssn'])
            emp_beneficiary_relation = 'spouse'
            emp_beneficiary_dob = data['spouse']['birthdate']
        else:
            emp_beneficiary_name = data.get('employee_beneficiary_name')
            emp_beneficiary_ssn = self._strip_ssn(data.get('employee_beneficiary_ssn', None))
            emp_beneficiary_relation = data.get('employee_beneficiary_relationship')
            emp_beneficiary_dob = data.get('employee_beneficiary_dob', None)

        if data['spouse_beneficiary'] == 'spouse':
            sp_beneficiary_name = u'{} {}'.format(data['employee']['first'],
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
            received_data=json.dumps(received_data, cls=JSONEncoder),
            standardized_data=json.dumps(wizard_data, cls=JSONEncoder),
            case_id=case_id,
            census_record_id=census_record_id,
            application_status=application_status,
            agent_code=agent_code,
            agent_name=agent_name,
            agent_id=agent_id,
            method=data['method'],
            payment_mode=data['payment_mode'],
            is_paylogix=census_record.case.requires_paylogix_export,
            is_preview=is_preview,
            # Signing info
            signature_time=signature_time,
            created_time=created_time,
            signature_city=data['enrollCity'],
            signature_state=data['enrollState'],
            identity_token=data['identityToken'],
            identity_token_type=data['identityType'],
            signature_method=signature_method,
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
        
        # If self-enroll, we need to mark the enrollment as pending agent.
        if first_data_wrap.is_self_enroll():
            enrollment_data['agent_signing_status'] = EnrollmentApplication.SIGNING_STATUS_PENDING
            enrollment_data['applicant_signing_status'] = EnrollmentApplication.SIGNING_STATUS_COMPLETE
            enrollment_data['applicant_signing_datetime'] = datetime.now()
            
        return self.create(**enrollment_data)

    def get_application_status(self, census_record, wizard_data):

        """:type: taa.services.products.ProductService"""
        
        data = self.get_first_wizard_data_record(wizard_data)
        first_data_record = EnrollmentDataWrap(data, census_record.case)
        
        if isinstance(wizard_data, list):
            accepted_data = list(EnrollmentDataWrap(d, census_record.case) for d in wizard_data if not d['did_decline'])
            
            # If this is a preview, mark the status as pending
            if not accepted_data or accepted_data[0].is_preview():
                return ''
            
            elif all(map(lambda d: d['did_decline'], wizard_data)):
                return EnrollmentApplication.APPLICATION_STATUS_DECLINED
            
            
            #elif len(accepted_products) > 0 and all(not p.requires_signature() for p in accepted_products):
            #    return EnrollmentApplication.APPLICATION_STATUS_ENROLLED
            
            #elif all(EnrollmentDataWrap(data, census_record.case).did_finish_signing_in_wizard() for data in accepted_data):
            #    return EnrollmentApplication.APPLICATION_STATUS_ENROLLED
            # No longer valid, we always sign in wizard if call center
            #elif census_record.case.should_use_call_center_workflow:
            #    return EnrollmentApplication.APPLICATION_STATUS_PENDING_AGENT
            
            elif first_data_record.is_self_enroll():
                # Agent still needs to sign the application if self-enrolled.
                return EnrollmentApplication.APPLICATION_STATUS_PENDING_AGENT

            else:
                # All other cases are enrolled status for now.
                return EnrollmentApplication.APPLICATION_STATUS_ENROLLED
        else:
            # TODO: Confirm this is the path imported enrollments take? Otherwise we shouldn't get here anymore.
            if wizard_data['did_decline']:
                application_status = EnrollmentApplication.APPLICATION_STATUS_DECLINED
            else:
                # We are likely importing the data and we don't have to say 'pending' since there is no signing process.
                application_status = EnrollmentApplication.APPLICATION_STATUS_ENROLLED
        return application_status

    def get_signature_method(self, census_record, wizard_data):
        # We no longer have a docusign method of signing.
        return EnrollmentApplication.SIGNATURE_METHOD_WIZARD
        
    def _strip_ssn(self, ssn):
        return ssn.replace('-', '').strip() if ssn else ''

    def _save_coverages(self, enrollment, all_data):
        # Create coverage record for each applicant / product combination where coverage was selected.
        if not isinstance(all_data, list):
            all_data = [all_data]

        for data in all_data:

            if data['did_decline']:
                continue
                
            enroller_effective_date = data.get('effective_date')
            
            if enrollment.is_preview:
                effective_date = None
                
            elif enroller_effective_date:
                if isinstance(enroller_effective_date, date_type) or isinstance(enroller_effective_date, datetime):
                    effective_date = enroller_effective_date
                else:
                    effective_date = dateutil.parser.parse(enroller_effective_date)
            else:
                # This usually means we sourced this from an  import file without explicit effective dates set.
                #  Compute according to the case settings.
                if enrollment.case.effective_date_settings:
                    from taa.services.enrollments.effective_date import calculate_effective_date, get_active_method
                    
                    if get_active_method(enrollment.case.effective_date_settings, datetime.now()) == 'enroller_selects':
                        # Don't need to implement enroller selects for imported files, because it is the same as them
                        # providing it in the first place. If for some reason we get here, just return sig time
                        effective_date = data['time_stamp'] if data.get('time_stamp') else datetime.now()
                    else:
                        # Calculate the effective date
                        effective_date = calculate_effective_date(enrollment.case, datetime.now())
                else:
                    # Fall back to signature time for old case data.
                    effective_date = data['time_stamp'] if data.get('time_stamp') else datetime.now()

            data_wrap = EnrollmentDataWrap(data, enrollment.case)
            product = data_wrap.get_product()

            if data['employee_coverage']:
                self.coverages_service.create_coverage(
                    enrollment, product, data, data['employee'],
                    data['employee_coverage'],
                    EnrollmentApplicationCoverage.APPLICANT_TYPE_EMPLOYEE,
                    effective_date=effective_date)
            if data['spouse_coverage']:
                if data['spouse_coverage'] == '':
                    effective_date = None
                self.coverages_service.create_coverage(
                    enrollment, product, data, data['spouse'],
                    data['spouse_coverage'],
                    EnrollmentApplicationCoverage.APPLICANT_TYPE_SPOUSE,
                    effective_date=effective_date)
            if data_wrap.get_covered_children():
                covered_children, coverages = data_wrap.get_covered_children_with_coverages()
                if data_wrap.get_product().is_children_coverage_grouped():
                    covered_children = covered_children[0:1]
                    coverages = coverages[0:1]
                
                for i, covered_child in enumerate(covered_children):
                    coverage = coverages[i]
                    self.coverages_service.create_coverage(
                        enrollment, product, data, covered_child,
                        coverage,
                        EnrollmentApplicationCoverage.APPLICANT_TYPE_CHILD,
                        effective_date=effective_date)
                
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

        case_products = self.product_service.get_ordered_products_for_case(case.id)
        for enrollment in census_record.enrollment_applications:
            
            # Skip over preview-only records
            if enrollment.is_preview:
                continue
            
            export_record = dict()
            export_record.update(self.get_census_data(census_record))
            export_record.update(self.get_unmerged_enrollment_data(census_record, enrollment, case_products))
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

        case_products_lookup = {}

        for census_record in census_records:

            # Make sure we have looked up the ordered list of products for this case.
            if census_record.case_id not in case_products_lookup:
                case_products_lookup[census_record.case_id] = self.product_service.get_ordered_products_for_case(
                    census_record.case_id)

            # Export only records with enrollments
            if not census_record.enrollment_applications:
                continue
            for enrollment in census_record.enrollment_applications:
                # Skip preview-only records
                if enrollment.is_preview:
                    continue
                
                export_record = dict()
                export_record.update(self.get_census_data(census_record))
                export_record.update(self.get_unmerged_enrollment_data(census_record, enrollment,
                                                                       case_products_lookup[census_record.case_id]))
                data.append(export_record)
        return data

    def retrieve_enrollment_data_for_table(self, case, offset=None, limit=None, search_text=None, order_column=None,
                                           order_dir=None):
        from taa.services.agents.models import Agent
        from taa.services.cases import CaseCensus

        query = db.session.query(
            EnrollmentApplication.signature_time.label('date'),
            EnrollmentApplication.application_status.label('enrollment_status'),
            EnrollmentApplication.id.label('id'),
            EnrollmentApplication.case_id.label('case_id'),
            EnrollmentApplication.census_record_id.label('census_record_id'),
            (Agent.first + " " + Agent.last).label('agent_name'),
            CaseCensus.employee_first.label('employee_first'),
            CaseCensus.employee_last.label('employee_last'),
            CaseCensus.employee_birthdate.label('employee_birthdate'),
            CaseCensus.employee_email.label('employee_email'),
            # Compute the annual premium
            db.select([
                db.case([
                    (db.func.sum(EnrollmentApplicationCoverage.weekly_premium) > 0,
                     db.func.sum(EnrollmentApplicationCoverage.weekly_premium) * 52),
                    (db.func.sum(EnrollmentApplicationCoverage.biweekly_premium) > 0,
                     db.func.sum(EnrollmentApplicationCoverage.biweekly_premium) * 26),
                    (db.func.sum(EnrollmentApplicationCoverage.semimonthly_premium) > 0,
                     db.func.sum(EnrollmentApplicationCoverage.semimonthly_premium) * 24),
                    (db.func.sum(EnrollmentApplicationCoverage.monthly_premium) > 0,
                     db.func.sum(EnrollmentApplicationCoverage.monthly_premium) * 12)
                ],
                    else_=0
                )
            ],
            ).where(db.and_(
                EnrollmentApplicationCoverage.enrollment_application_id == EnrollmentApplication.id,
                # Filter out coverages where the premium shouldn't count.
                # Either no coverage_selection, or count only the employee coverage if it is a product
                # with coverage selection.
                db.or_(EnrollmentApplicationCoverage.coverage_selection == None,
                       db.and_(EnrollmentApplicationCoverage.coverage_selection != None,
                               EnrollmentApplicationCoverage.applicant_type == 'employee')
                       )
                )
            ).correlate(EnrollmentApplication).label('total_premium'),
            db.select(
                [EnrollmentApplicationCoverage.effective_date]
            ).where(EnrollmentApplicationCoverage.enrollment_application_id == EnrollmentApplication.id
                    ).limit(1).correlate(EnrollmentApplication).label('effective_date'),
        )
        query = query.join(CaseCensus, CaseCensus.id == EnrollmentApplication.census_record_id)
        query = query.outerjoin(Agent, Agent.id == EnrollmentApplication.agent_id)

        # Overall filtering
        query = query.filter(CaseCensus.case_id == case.id
            ).filter(EnrollmentApplication.is_preview != True)

        # Data filtering
        if search_text:
            for text_snippet in search_text.split():
                query = query.filter(db.or_(
                    CaseCensus.employee_first.ilike(u'{}%'.format(text_snippet)),
                    CaseCensus.employee_last.ilike(u'{}%'.format(text_snippet)),
                    # CaseCensus.employee_email.ilike('{}%'.format(text_snippet)),
                    EnrollmentApplication.application_status.ilike(u'{}%'.format(text_snippet)),
                    Agent.first.ilike(u'{}%'.format(text_snippet)),
                    Agent.last.ilike(u'{}%'.format(text_snippet)),
                ))

        # Ordering
        order_column_mapping = dict(
            date=EnrollmentApplication.signature_time,
            employee_first=CaseCensus.employee_first,
            employee_last=CaseCensus.employee_last,
            effective_date=EnrollmentApplicationCoverage.effective_date,
            enrollment_status=EnrollmentApplication.application_status,
            total_premium='total_premium',
            agent_name='agent_name',
        )
        order_clause = order_column_mapping.get(order_column, "employee_last")
        if order_dir == 'desc':
            order_clause = db.desc(order_clause)
        query = query.order_by(order_clause)

        # Pagination
        query = query.offset(offset).limit(limit)

        return query

    def retrieve_enrollments_total_visible_count_for_table(self, case):
        return self.retrieve_enrollment_data_for_table(case).count()

    def retrieve_enrollments_filtered_count_for_table(self, case, search_text):
        return self.retrieve_enrollment_data_for_table(case, search_text=search_text).count()

    def get_enrollment_status(self, census_record):
        # Get the flattened enrollment record
        # enrollment_data = self.get_enrollment_data(census_record)
        # return (enrollment_data['application_status']
        #        if enrollment_data else None)

        enrollment_records = [e for e in census_record.enrollment_applications if not e.is_preview]

        # If any is pending, we say the whole record is pending so as to not have more than one pending at a time.
        if any([e for e in enrollment_records if e.is_pending_employee()]):
            return EnrollmentApplication.APPLICATION_STATUS_PENDING_EMPLOYEE
        elif any([e for e in enrollment_records if e.is_pending_agent()]):
            return EnrollmentApplication.APPLICATION_STATUS_PENDING_AGENT
        # Otherwise, we check to see if anyone has ever enrolled for this record
        elif any([e for e in enrollment_records if e.did_enroll()]):
            return EnrollmentApplication.APPLICATION_STATUS_ENROLLED
        elif any([e for e in enrollment_records if e.did_decline()]):
            return EnrollmentApplication.APPLICATION_STATUS_DECLINED
        else:
            return None

    def get_census_data(self, census_record):
        return census_record.to_json()

    def get_standardized_enrollment_json(self, census_record):
        """Normalizes the JSON data as a list of standardized enrollment data."""

        out = []
        for enrollment_application in census_record.enrollment_applications:
            if enrollment_application.is_preview:
                continue
            out += self.get_standardized_json_for_enrollment(enrollment_application)

        return out

    def get_standardized_json_for_enrollment(self, enrollment_application):
        if not enrollment_application.standardized_data:
            return []
        json_data = json.loads(enrollment_application.standardized_data)
        if isinstance(json_data, list):
            return json_data
        else:
            return [json_data]

    def get_wrapped_enrollment_data(self, enrollment_application):
        # Return normalized data with the extra methods added in EnrollmentDataWrap.
        return [
            EnrollmentDataWrap(data, enrollment_application.case, enrollment_record=enrollment_application)
            for data in self.get_standardized_json_for_enrollment(enrollment_application)
        ]

    def get_wrapped_data_for_coverage(self, coverage):
        "Given a specific EnrollmentApplicationCoverage object, find the corresponding JSON data in the enrollment"

        enrollment = coverage.enrollment
        wrapped_enrollment_data = self.get_wrapped_enrollment_data(enrollment)
        # Find the enrollment data that matches the coverage's product. All applicant's data is contained in this data.
        return  next(ifilter(lambda d: d.get_product_id() == coverage.product_id, wrapped_enrollment_data), None)


    def get_unmerged_enrollment_data(self, census_record, enrollment, case_products):
        """
        If we are not merging, we know we are dealing with coverages from
        a single enrollment.
        :param case_products:
        """
        enrollment_data = {}
        if not census_record.enrollment_applications or not enrollment:
            return None

        enrollment_data['enrollment_id'] = enrollment.id

        enrollment_data[
            'signature_method'] = enrollment.signature_method if enrollment.signature_method else EnrollmentApplication.SIGNATURE_METHOD_DOCUSIGN

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
        product_ids = enrollment.get_enrolled_product_ids()
        # Keep this conversion of the set to tuple to prevent SQLAlchemy from throwing an exception due to not being
        # able to accept lists or sets

        for x in range(6):
            if x < len(case_products):
                product = case_products[x]
                """:type: taa.services.products.Product"""
            else:
                product = None
            if product and product.id not in product_ids:
                product = None
            prefix = 'product_{0}'.format(x + 1)
            product_data = {u'{}_name'.format(prefix): product.name if product else ''}

            total_product_premium = Decimal('0.00')

            for applicant_abbr, applicant_coverages in (('emp',
                                                         employee_coverage),
                                                        ('sp',
                                                         spouse_coverage),
                                                        ('ch',
                                                         children_coverage)):
                coverage = ''
                premium = ''
                annualized_premium = ''
                if applicant_coverages.get(product):
                    applicant_coverage = applicant_coverages[product]
                    enrollment_data['effective_date'] = applicant_coverage.effective_date
                    if product is not None and (product.is_simple_coverage() or product.is_static_benefit()):
                        coverage = 'Included' if product.is_applicant_covered(applicant_coverage.applicant_type,
                                                                              applicant_coverage.coverage_selection) \
                            else 'Not Included '
                        if applicant_coverage.applicant_type == 'employee':
                            premium = applicant_coverage.get_premium()
                            annualized_premium = applicant_coverage.get_annualized_premium()
                    else:
                        coverage = applicant_coverage.coverage_face_value
                        premium = applicant_coverage.get_premium()
                        annualized_premium = applicant_coverage.get_annualized_premium()

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

                    # if total_product_premium == Decimal('0.00'):
                    #    total_product_premium = ''

            enrollment_data.update(product_data)
            enrollment_data['{}_total_premium'.format(prefix)] = total_product_premium

        enrollment_data['total_annual_premium'] = total_annual_premium
        if enrollment.docusign_envelope_id:
            envelope = DocusignEnvelope(uri=enrollment.docusign_envelope_id, enrollment_record=enrollment)
            enrollment_data['docusign_envelope_id'] = envelope.get_envelope_id()
        else:
            enrollment_data['docusign_envelope_id'] = None

        enrollment_data['agent_id'] = enrollment.agent_id

        json_data = self.get_standardized_json_for_enrollment(enrollment)
        if len(json_data) > 0:
            children = json_data[0]['children']
            if len(children) > 0:
                for idx in range(len(children)):
                    child = children[idx]
                    gender = child.get('gender', '')
                    if not isinstance(gender, unicode) and not isinstance(gender, unicode):
                        gender = unicode(gender)
                    gender = gender if gender is not None and gender != '' and gender.lower() != 'none' else ''
                    enrollment_data['CH%d_GENDER' % idx] = gender

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
        writer = UnicodeCsvWriter(stream)
        # Write the header row
        writer.writerow(self.get_csv_headers())
        # Write all the data
        writer.writerows(self.get_csv_row(record) for record in data)
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

    def get_export_dictionary(self, census_record, application):
        data = dict()
        data.update(self.get_census_data(census_record))
        data.update(self.get_unmerged_enrollment_data(census_record, application,
                                                      self.product_service.get_ordered_products_for_case(
                                                          census_record.case_id)))
        enrollment_tuples = [(c.column_title, c.get_value(data)) for c in enrollment_columns]
        census_tuples = zip(self.case_service.census_records.get_csv_headers(),
                            self.case_service.census_records.get_csv_row_from_dict(data))
        data.update(dict(enrollment_tuples + census_tuples))
        for k, v in list(data.iteritems()):
            if not isinstance(v, unicode) and not isinstance(v, str):
                v = unicode(v)
            if v is None or v.lower() == 'none':
                data[k] = ''
        return data

    def get_enrollments_by_date(self, from_, to_):
        return self.__model__.query.filter(self.__model__.signature_time >= from_,
                                           self.__model__.signature_time <= to_)

    # noinspection PyMethodMayBeStatic
    def get_applications_by_submission_date(self, start_date=None, end_date=None):
        query = db.session.query(EnrollmentApplication) \
            .join(EnrollmentApplication.enrollment_submissions) \
            .filter(EnrollmentSubmission.submission_type == EnrollmentSubmission.TYPE_DELL_CSV_GENERATION)

        if start_date is not None:
            query = query.filter(EnrollmentApplication.signature_time >= start_date)
        if end_date is not None:
            query = query.filter(EnrollmentApplication.signature_time <= end_date)

        return query.all()

    def sync_enrollment_with_docusign(self, enrollment_application_id):
        enrollment_application = self.get(enrollment_application_id)
        if enrollment_application and enrollment_application.docusign_envelope_id and not enrollment_application.is_terminal_status():
            self.update_applicant_signing_status(enrollment_application)

        # Need to commit all database changes.
        db.session.commit()

    def get_first_wizard_data_record(self, wizard_data):
        if isinstance(wizard_data, list):
            return wizard_data[0]
        else:
            return wizard_data

    def get_paylogix_info(self, enrollment_data):
        """
        Create a dictionary that contains information needed for the Paylogix export in a flat dict
        :param enrollment_data: Dictionary from EnrollmentApplication.standardized_data
        :type enrollment_data: dict
        :return:
        """
        paylogix_info = {
            'Account Holder Name': '',
            'ACH Routing Number': '',
            'ACH Account Number': '',
            'ACH Account Type': '',
            'Bank Name': '',
            'Address One': '',
            'Address Two': '',
            'City': '',
            'State': '',
            'Zip': '',
        }

        if enrollment_data.get('bank_info', None):
            bank_info = enrollment_data['bank_info']
            paylogix_info['Account Holder Name'] = bank_info.get('account_holder_name', '')
            paylogix_info['ACH Routing Number'] = bank_info.get('routing_number', '')
            paylogix_info['ACH Account Number'] = bank_info.get('account_number', '')
            account_type = bank_info.get('account_type', '')
            if account_type.lower() == 'checking':
                account_type = 'C'
            elif account_type.lower() == 'savings':
                account_type = 'S'
            paylogix_info['ACH Account Type'] = account_type
            paylogix_info['Bank Name'] = bank_info.get('bank_name', '')
            paylogix_info['Address One'] = bank_info.get('address_one', '')
            paylogix_info['Address Two'] = bank_info.get('address_two', '')
            paylogix_info['City'] = bank_info.get('city', '')
            paylogix_info['State'] = bank_info.get('state', '')
            paylogix_info['Zip'] = bank_info.get('zip', '')

        return paylogix_info

    def get_between_dates(self, start_date, end_date):
        """
        Get all Enrollment Applications between specified dates

        :param start_date: Start Date
        :param end_date: End Date
        :return:
        """
        return db.session.query(EnrollmentApplication
            ).filter(start_date < EnrollmentApplication.signature_time < end_date
            ).filter(EnrollmentApplication.is_preview != True
            ).all()

    def get_paylogix_applications_between_dates(self, start_date, end_date):
        return db.session.query(EnrollmentApplication
                                ).filter(EnrollmentApplication.is_preview != True
            ).filter(db.and_(
                EnrollmentApplication.signature_time > start_date, EnrollmentApplication.signature_time < end_date)
            ).join(Case
            ).filter(Case.requires_paylogix_export == True
            ).order_by(db.desc(EnrollmentApplication.signature_time)
            ).all()

    def get_paylogix_applications(self):
        return db.session.query(EnrollmentApplication
            ).filter(EnrollmentApplication.is_preview != True
            ).join(Case
            ).filter(Case.requires_paylogix_export == True
            ).order_by(db.desc(EnrollmentApplication.signature_time)
            ).all()


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
        return val[:4] + '-' + val[4:6] + '-' + val[6:]

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
coverage_columns = [
    EnrollmentColumn('total_annual_premium', 'Total Annual Premium', export_string),
    EnrollmentColumn('effective_date', 'Effective Date', export_date),
]
for product_num in range(1, 6 + 1):
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
