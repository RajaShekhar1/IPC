from flask_login import current_user

from taa import app, db
from taa.services import LookupService


class UserService(object):
    "Deals with authentication, authorization, and some StormPath abstraction"

    ENROLLMENT_IMPORT_GROUP = u'enrollment_importers'

    def __init__(self):
        self._cached_stormpath_app = None

    def get_stormpath_user_by_href(self, href):
        user_account = None
        for account in search_stormpath_accounts(filter_href=href):
            if account.href == href:
                user_account = account
        return user_account

    def search_stormpath_accounts(self, filter_email=None, filter_href=None):
        """
        Replaced Stormpath with Okta
        """
        from taa.services.agents import OktaService

        if filter_href:
            return OktaService().get_user_data(filter_href)
        elif filter_email:
            return OktaService().get_user_by_email(filter_email)
        else:
            return OktaService().get_all_users()

    def can_current_user_submit_enrollments(self):
        return self.can_user_submit_enrollments(current_user)

    def get_current_user(self):
        return current_user

    def get_current_user_href(self):
        if current_user:
            return current_user.href
        else:
            return None

    def can_user_submit_enrollments(self, account):
        return self.ENROLLMENT_IMPORT_GROUP in self.get_user_groupnames(account)

    def get_user_groupnames(self, user):
        if not user.is_anonymous and hasattr(user, 'groups'):
            return {g.group for g in user.groups}
        else:
            return set()

    def get_admin_users(self):
        from taa.services.agents.models import Agent, AgentGroups
        return db.session.query(Agent).filter(Agent.groups.has(AgentGroups.group == 'admins')).all()
        
        #sp_app = self.get_stormpath_application()
        #admin_group = [g for g in self.get_groups() if g.name == "admins"][0]
        #return [u for u in admin_group.accounts]

    def get_groups(self):
        group_names = [
            'admins',
            'home_office',
            'agents',
            'api_users',
            'case_admins',
            'enrollment_importers',
        ]
        return [Group(g) for g in group_names]
    
    
class Group(object):
    def __init__(self, name):
        self.name = name

def search_stormpath_accounts(filter_email=None, filter_href=None):
    user_service = LookupService('UserService')
    return user_service.search_stormpath_accounts(filter_email, filter_href)

def get_stormpath_application():
    user_service = LookupService('UserService')
    return user_service.get_stormpath_application()
