from stormpath.client import Client as SPClient
from taa import app
from taa.services import LookupService


class UserService(object):
    "Deals with authentication, authorization, and some StormPath abstraction"

    ENROLLMENT_IMPORT_GROUP = u'enrollment_importers'

    def get_stormpath_user_by_href(self, href):
        user_account = None
        for account in search_stormpath_accounts():
            if account.href == href:
                user_account = account
        return user_account

    def search_stormpath_accounts(self, filter_email=None, filter_href=None):
        """
        The flask-stormpath extension has some strange caching issues when using the
        manager to query. Use the stormpath library directly here.
        """
        sp_app = self.get_stormpath_application()

        params = {}

        if filter_email:
            params['email'] = filter_email
        if filter_href:
            params['href'] = filter_href

        if params:
            return [a for a in sp_app.accounts.search(params)]
        else:
            return [a for a in sp_app.accounts]

    def can_user_submit_enrollments(self, account):
        return self.ENROLLMENT_IMPORT_GROUP in self.get_user_groupnames(account)

    def get_user_groupnames(self, user):
        if hasattr(user, 'groups'):
            return {g.name for g in user.groups}
        else:
            return set()

    def get_stormpath_application(self):
        app_name = app.config['STORMPATH_APPLICATION']
        c = SPClient(id=app.config['STORMPATH_API_KEY_ID'],
                   secret=app.config['STORMPATH_API_KEY_SECRET'])

        for sp_app in c.applications:
            if sp_app.name == app_name:
                return sp_app

        raise Exception('The configured stormpath application "%s" could not be found'%app_name)


def search_stormpath_accounts(filter_email=None, filter_href=None):
    user_service = LookupService('UserService')
    return user_service.search_stormpath_accounts(filter_email, filter_href)

def get_stormpath_application():
    user_service = LookupService('UserService')
    return user_service.get_stormpath_application()
