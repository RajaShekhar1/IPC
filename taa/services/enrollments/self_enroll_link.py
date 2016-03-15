import re
import unicodedata
import uuid

from taa.core import DBService, db
from models import SelfEnrollmentLink
from taa.services.cases.models import SelfEnrollmentSetup

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
        url = u'{}self-enroll/{}/{}'.format(
            prefix,
            SelfEnrollmentLinkService._slugify(case.company_name or ''),
            uuid.uuid4().hex)
        link = self.create(
            self_enrollment_setup_id=case.self_enrollment_setup.id,
            census_record_id=None if record is None else record.id,
            url=url)
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

        if not link:
            link = self.generate_link(prefix, case, record=None)
            db.session.commit()

        return link.url
