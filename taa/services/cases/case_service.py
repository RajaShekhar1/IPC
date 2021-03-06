import StringIO
import json
import uuid
from datetime import datetime

import dateutil.parser
import sqlalchemy as sa
from flask import abort
from flask_stormpath import current_user
from sqlalchemy.orm import joinedload, eagerload

from taa.core import DBService, db
from taa.services import RequiredFeature, LookupService
from taa.services.agents.models import Agent

from models import (Case, CaseCensus, CaseOpenEnrollmentPeriod, CaseOngoingEnrollmentPeriod,
                    SelfEnrollmentSetup)


# noinspection PyMethodMayBeStatic
class CaseService(DBService):
    __model__ = Case

    census_records = RequiredFeature('CensusRecordService')
    enrollment_periods = RequiredFeature('CaseEnrollmentPeriodsService')
    self_enrollment = RequiredFeature('SelfEnrollmentService')
    agent_splits = RequiredFeature('AgentSplitsService')
    rider_service = RequiredFeature('RiderService')

    def __init__(self, *args, **kwargs):
        super(CaseService, self).__init__(*args, **kwargs)

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

        if by_agent and not by_name:
            # Right now, an agent can 'see' a given case if he is either
            # the owner or a 'partner' agent, which is an explicitly maintained
            # list by the HO admin.
            query = query.filter(db.or_(
                Case.agent_id == by_agent,
                Case.partner_agents.any(Agent.id == by_agent)
            )
            )

        # Pre-load products, owner agent, and enrollment periods to speed up most subsequent operations
        query = query.options(
            db.joinedload('enrollment_periods')
        ).options(
            db.joinedload('products')
        ).options(
            db.joinedload('owner_agent')
        )

        results = query.all()
        if only_enrolling:
            results = [case for case in results if self.is_enrolling(case)]
        return results

    def get_case_for_token(self, token):
        query = self.query()
        query = query.filter(Case.case_token.ilike(token))
        return query.first()

    def is_valid_case_token(self, token):
        return bool(self.get_case_for_token(token.strip()))

    def get_products_for_case(self, case):
        # Return the sorted list of products for this case
        # The relationship on the case is sorted explicitly in the relationship.
        return case.products

    def get_rider_codes(self):
        return []  # [c.code for c in self.case_riders.split(",")]

    def update_product_settings(self, case, product_settings):
        # TODO: validate the product settings before saving.
        case.product_settings = product_settings
        db.session.flush()

    def update_products(self, case, products):
        from taa.services.products import ProductService
        products_service = ProductService()
        product_ids = [p['id'] for p in products]
        fetched_products = products_service.get_all(*product_ids)

        case.products = fetched_products
        db.session.flush()

        # Iterate through products and set the ordinal
        from taa.services.cases.models import case_products
        from sqlalchemy import update
        for product in case.products:
            api_product = next(p for p in products if p['id'] == product.id)
            if api_product is not None and api_product['ordinal'] is not None:
                ordinal = int(api_product['ordinal'])
                query = update(case_products) \
                    .where(case_products.c.case_id == case.id) \
                    .where(case_products.c.product_id == product.id) \
                    .values({'ordinal': ordinal})
                db.session.execute(query)

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
        past_periods = filter(lambda p: p.get_start_date() < datetime.now() if p.get_start_date() else None,
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

        # If the enrolling agent is no longer part of the case, remove him
        if (case.self_enrollment_setup and
                    case.self_enrollment_setup.enrolling_agent not in self.get_agents_for_case(case)):
            self.update_enrolling_agent(case, agent_id=None)
            db.session.flush()

    def get_case_owner(self, case):
        return case.owner_agent if case.owner_agent else None

    def get_case_partner_agents(self, case):
        return [a for a in case.partner_agents if a != case.owner_agent]

    def get_agents_for_case(self, case):
        agents = []
        if self.get_case_owner(case):
            agents.append(self.get_case_owner(case))

        agents += self.get_case_partner_agents(case)

        return agents

    def update_enrolling_agent(self, case, agent_id):
        self.self_enrollment.update_enrolling_agent(case, agent_id)

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
        # TODO: Remove
        # for period in periods:
        #     if period['period_type'] == (CaseOpenEnrollmentPeriod.PERIOD_TYPE
        #                                  and case.enrollment_period_type !=
        #             Case.OPEN_ENROLLMENT_TYPE):
        #         case.enrollment_period_type = Case.OPEN_ENROLLMENT_TYPE
        #     elif period['period_type'] == (CaseOngoingEnrollmentPeriod.PERIOD_TYPE
        #                                    and case.enrollment_period_type !=
        #             Case.ONGOING_ENROLLMENT_TYPE):
        #         case.enrollment_period_type = Case.ONGOING_ENROLLMENT_TYPE

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
                           filter_ssn=None,
                           filter_birthdate=None,
                           filter_emp_first=None,
                           filter_emp_last=None,
                           filter_agent=None,
                           include_enrollment_links=False):
        from taa.services.enrollments.models import EnrollmentApplication
        query = self.census_records.find(case_id=case.id)

        # Eager load enrollment applications, coverages, and associated products
        query = query.outerjoin('enrollment_applications').options(
            db.contains_eager('enrollment_applications'
                  ).subqueryload('coverages'
                  ).joinedload('product')
        )

        if include_enrollment_links:
            query = query.options(eagerload('self_enrollment_links'))

        if filter_agent:
            # Only show enrolled census records where this agent was the enrolling agent.
            query = query.join('enrollment_applications'
                               ).filter(EnrollmentApplication.agent_id == filter_agent.id)

        if filter_ssn:
            query = query.filter(CaseCensus.employee_ssn ==
                                 filter_ssn.replace('-', ''))

        if filter_emp_first:
            query = query.filter(CaseCensus.employee_first.ilike(u"{}%".format(filter_emp_first)))

        if filter_emp_last:
            query = query.filter(CaseCensus.employee_last.ilike(u"{}%".format(filter_emp_last)))

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

    def retrieve_census_data_for_table(self, case,
                                       agent_id=None,
                                       offset=0,
                                       limit=None,
                                       search_text=None,
                                       order_column=None,
                                       order_dir=None):
        from taa.services.enrollments.models import EnrollmentApplication

        # Need the following data columns:
        query = db.session.query(
            CaseCensus.id.label('id'),
            CaseCensus.case_id.label('case_id'),
            EnrollmentApplication.application_status.label('enrollment_status'),
            CaseCensus.employee_first.label('employee_first'),
            CaseCensus.employee_last.label('employee_last'),
            CaseCensus.employee_birthdate.label('employee_birthdate'),
            CaseCensus.employee_email.label('employee_email'),
        )

        query = self.join_most_recent_enrollment(query)

        # Overall filtering
        query = query.filter(CaseCensus.case_id == case.id)
        if agent_id is not None:
            query = query.filter(EnrollmentApplication.agent_id == agent_id)

        # Data filtering
        if search_text:
            for text_snippet in search_text.split():
                query = query.filter(db.or_(
                    CaseCensus.employee_first.ilike(u'{}%'.format(text_snippet)),
                    CaseCensus.employee_last.ilike(u'{}%'.format(text_snippet)),
                    CaseCensus.employee_email.ilike(u'{}%'.format(text_snippet)),
                    EnrollmentApplication.application_status.ilike(u'{}%'.format(text_snippet))
                ))

        # Ordering
        order_column_mapping = dict(
            employee_last=CaseCensus.employee_last,
            employee_first=CaseCensus.employee_first,
            status='enrollment_status',
        )
        order_clause = order_column_mapping.get(order_column, "employee_last")
        if order_dir == 'desc':
            order_clause = db.desc(order_clause)
        query = query.order_by(order_clause)

        # Pagination
        query = query.offset(offset).limit(limit)

        return query

    def join_most_recent_enrollment(self, query):
        from taa.services.enrollments import EnrollmentApplication
        valid_enrollment_status = db.and_(
            EnrollmentApplication.application_status != EnrollmentApplication.APPLICATION_STATUS_VOIDED,
            EnrollmentApplication.application_status != None
        )
        query = query.outerjoin(
            EnrollmentApplication,
            db.and_(
                EnrollmentApplication.signature_time == db.select([db.func.max(EnrollmentApplication.signature_time)]
                                                                  ).where(valid_enrollment_status
                                                                          ).where(
                    EnrollmentApplication.census_record_id == CaseCensus.id
                ).correlate(CaseCensus),
                EnrollmentApplication.census_record_id == CaseCensus.id
            )
        )
        return query

    def retrieve_census_total_visible_count_for_table(self, case, agent_id=None):
        query = self.retrieve_census_data_for_table(case=case, agent_id=agent_id)
        return query.count()

    def retrieve_census_filtered_count_for_table(self, case, agent_id=None, search_text=None):
        query = self.retrieve_census_data_for_table(case=case, agent_id=agent_id, search_text=search_text)
        return query.count()

    def match_census_record_to_wizard_data(self, enrollment_data):
        """
        Given enrollment data, try to find a matching census record based on SSN, Name, and Birthdate.
        """
        first = enrollment_data.get_employee_first()
        last = enrollment_data.get_employee_last()
        birthdate = enrollment_data.get_employee_birthdate()
        ssn = enrollment_data.get_employee_ssn()

        matching = self.get_census_records(
            enrollment_data.case,
            filter_emp_first=first,
            filter_emp_last=last,
            filter_birthdate=birthdate,
            filter_ssn=ssn
        )
        if not matching:
            return None
        return matching[0]

    def get_current_user_census_records(self, case):
        if not self.is_current_user_restricted_to_own_enrollments(case):
            return self.get_census_records(case)
        else:
            from taa.services.agents import AgentService
            agent_service = AgentService()
            return self.get_census_records(case, filter_agent=agent_service.get_logged_in_agent())

    def is_current_user_restricted_to_own_enrollments(self, case):
        agent_service = LookupService('AgentService')
        if agent_service.is_user_agent(current_user):
            return self.is_agent_restricted_to_own_enrollments(agent_service.get_logged_in_agent(), case)
        return False

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
            filters.append(getattr(CaseCensus, col).ilike(search_text + u'%'))
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
        from taa.services.agents import AgentService
        agent_service = AgentService()
        current_agent = agent_service.get_logged_in_agent()
        is_restricted = current_agent and self.is_agent_restricted_to_own_enrollments(current_agent, record.case)
        did_agent_enroll_record = current_agent and self.did_agent_enroll_record(current_agent, record)

        if not self.can_current_user_view_case(record.case):
            abort(401)
        elif (current_agent and is_restricted and not did_agent_enroll_record):
            abort(401)

        return record

    def did_agent_enroll_record(self, agent, record):

        # As long as there is an enrollment with this agent ID, we consider the agent to have enrolled this record.
        for enrollment_record in record.enrollment_applications:
            if enrollment_record.agent_id == agent.id:
                return True

        return False

    def can_current_user_view_case(self, case):
        from taa.services.agents import AgentService
        agent_service = AgentService()
        if agent_service.can_manage_all_cases(current_user):
            return True
        elif agent_service.is_user_agent(current_user):
            agent = agent_service.get_agent_from_user(current_user)
            return self.agent_can_view_case(agent, case)
        return False

    def create_ad_hoc_census_record(self, case, data):
        """Creates a census record and updates the data from standardized enrollment data"""
        record = self.census_records.add_record(case, **dict(is_uploaded_census=False))
        self.update_census_record_from_enrollment(record, data)
        db.session.flush()
        return record

    def process_uploaded_census_data(self, case, merge_type, file_obj):
        if merge_type == 'merge-skip':
            return self.merge_census_data(case, self._create_file_buffer(file_obj),
                                          replace_matching=False)
        elif merge_type == 'merge-replace':
            return self.merge_census_data(case, self._create_file_buffer(file_obj),
                                          replace_matching=True)
        else:
            return self.replace_census_data(case, self._create_file_buffer(file_obj))

    def process_uploaded_logo_image(self, case, file_obj):
        case.logo_image_data = self._create_file_buffer(file_obj).read()
        db.session.commit()

    def _create_file_buffer(self, file_obj):
        # Read data into a buffer
        file_data = StringIO.StringIO()
        file_obj.save(file_data)
        file_data.seek(0)
        return file_data

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
            from taa.services.enrollments import EnrollmentApplicationService
            enrollment_service = EnrollmentApplicationService()
            enrollment_service.delete_enrollment_data(record)

        # Remove the attached email logs and links, if any.
        for log in record.email_logs:
            db.session.delete(log)
        for link in record.self_enrollment_links:
            db.session.delete(link)

        return self.census_records.delete(record)

    def generate_token(self):
        return uuid.uuid4().hex

    def populate_case_token(self, case):
        if not case.case_token:
            case.case_token = self.generate_token()

    def delete_case(self, case):
        from taa.services.agents import AgentService
        from taa.services.enrollments import EnrollmentApplicationService
        from taa.services.enrollments import SelfEnrollmentEmailService
        from taa.services.enrollments import SummaryEmailService
        emails_service = SelfEnrollmentEmailService()
        enrollments_service = EnrollmentApplicationService()
        summary_email_service = SummaryEmailService()
        agent_service = AgentService()

        for record in case.enrollment_records:
            summary_email_service.delete_emails_for_enrollment(record)

        # Remove all enrollments if allowed
        if agent_service.can_manage_all_cases(current_user):
            enrollments_service.delete_case_enrollment_data(case)

        # remove all census records and enrollment_periods first
        for record in self.get_census_records(case):
            self.delete_census_record(record)

        self.enrollment_periods.remove_all_for_case(case)

        # remove all email batch records
        emails_service.delete_batches_for_case(case)



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

    def get_agent_splits_setup(self, case):
        return self.agent_splits.find(case_id=case.id)

    def create_agent_splits_setup(self, case, data):
        # Don't flush the session here since this is often called in bulk.
        model = self.agent_splits.new(case_id=case.id, **data)
        db.session.add(model)

    def delete_agent_splits_setup_for_case(self, case):
        return self.agent_splits.find(case_id=case.id).delete()

    def can_current_user_edit_case(self, case):
        from taa.services.agents import AgentService
        agent_service = AgentService()
        logged_in_agent = agent_service.get_logged_in_agent()
        is_case_owner = logged_in_agent and logged_in_agent is self.get_case_owner(case)

        if agent_service.can_manage_all_cases(current_user):
            return True
        if self.is_agent_case_owner(logged_in_agent, case):
            return True

        return False

    def create_new_case(self, **kwargs):
        case = DBService.create(self, **kwargs)

        self.populate_case_token(case)

        # Make sure a self-enrollment setup is created too.
        setup = self.self_enrollment.create(**{
            'case_id': case.id,
            'self_enrollment_type': SelfEnrollmentSetup.TYPE_CASE_GENERIC,
            'use_email': True,
            'use_landing_page': True,
        })

        return case

    def is_agent_case_owner(self, agent, case):
        return agent is self.get_case_owner(case)

    def can_agent_edit_case(self, agent, case):
        return self.is_agent_case_owner(agent, case)

    def is_agent_allowed_to_view_full_census(self, agent, case):
        # Either we own the case, or we are a partner agent with no restrictions turned on.
        return self.is_agent_case_owner(agent, case) or case.can_partner_agent_download_enrollments()

    def is_agent_restricted_to_own_enrollments(self, agent, case):
        return not self.is_agent_allowed_to_view_full_census(agent, case)

    def get_classifications(self, case):
        classifications = []
        if case.occupation_class_settings is not None:
            for c in case.occupation_class_settings:
                classifications.append(c['label'])
        return classifications

    def get_classification_for_label(self, label, case, product_id):
        if case.product_settings is None or label is None or product_id is None:
            return None
        mappings = case.product_settings['classification_mappings']
        mapping = next(mappings[key] for key in mappings if int(key) == int(product_id))
        for key in mapping:
            if key.lower() == label.lower():
                return mapping[key]
        return None

    def is_agent_allowed_to_view_case_setup(self, agent, case):
        if case.can_partners_download_enrollments:
            return True
        else:
            return agent.id == case.agent_id

    def requires_occupation(self, case):
        return len(case.products) > 0 and any(p for p in case.products if p.requires_occupation())

    def get_occupation_classes_in_use(self, case_id):
        """
        Query all the occupation classes that are in use in a census_record
        :param case_id: Id of the case to query for
        :type case_id: int
        :rtype: set[str]
        """
        query = sa.text("""SELECT DISTINCT occupation_class FROM case_census
                WHERE case_id = :case_id AND occupation_class IS NOT NULL AND occupation_class != 'Default'""")
        results = db.engine.execute(query, case_id=case_id)

        occupation_classes = set()
        for row in results:
            occupation_classes.add(row[0])
        return occupation_classes

    def remove_occupation_class_from_census_records(self, case_id, label):
        db.engine.execute(sa.text("""UPDATE case_census SET occupation_class = 'Default'
               WHERE case_id = :case_id AND occupation_class = :occupation_class"""),
                          case_id=case_id, occupation_class=label)

    def update_census_occupation_classes(self, case_id, occupation_classes):
        """
        Update the census record occupation classes if any have been removed
        :param case_id: Id of the case to update census records for
        :type case_id: int
        :param occupation_classes: Occupation classes to update
        :type occupation_classes: list[dict]
        """
        new_labels = set(oc['label'] for oc in occupation_classes)
        old_labels = set(self.get_occupation_classes_in_use(case_id))
        removed_labels = new_labels - old_labels
        for label in removed_labels:
            self.remove_occupation_class_from_census_records(case_id, label)

    def get_product_coverage_limit_data(self, case, product):
        if not case.product_settings:
            return None
        if not case.product_settings.get('coverage_limits'):
            return None
        
        limit_data = case.product_settings['coverage_limits']
        product_limit_data = limit_data.get(str(product.id))
        return product_limit_data
        