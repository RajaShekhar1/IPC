"""Custom data models."""


from flask import current_app
from six import text_type

from blinker import Namespace

from stormpath.resources.account import Account
from stormpath.resources.provider import Provider

from okta import UsersClient
from okta import AuthClient
from taa.afba_okta.afba_okta_group import AfbaOktaGroup
import re


stormpath_signals = Namespace()
user_created = stormpath_signals.signal('user-created')
user_updated = stormpath_signals.signal('user-updated')
user_deleted = stormpath_signals.signal('user-deleted')


class User():
    """
    The base User object.

    This can be used as described in the Stormpath Python SDK documentation:
    https://github.com/stormpath/stormpath-sdk-python
    """
    def __init__(self):
        self.custom_data = {}

    def __repr__(self):
        return u'User <"%s" ("%s")>' % (self.username or self.email, self.href)

    def get_id(self):
        """
        Return the unique user identifier (in our case, the Stormpath resource
        href).
        """
        #return text_type(self.href)
        return  self.id



    def is_active(self):
        """
        A user account is active if, and only if, their account status is
        'ENABLED'.
        """
        return self.status == 'ENABLED'

    def is_anonymous(self):
        """
        We don't support anonymous users, so this is always False.
        """
        return False

    def is_authenticated(self):
        """
        All users will always be authenticated, so this will always return
        True.
        """
        return True

    def has_groups(self,groups,all=True):
        if(len(groups)>0):
            return True
        return False


    def save(self):
        """
        Send signal after user is updated.
        """
        return_value = super(User, self).save()
        user_updated.send(self, user=self)
        return return_value

    def delete(self):
        """
        Send signal after user is deleted.
        """
        return_value = super(User, self).delete()
        user_deleted.send(self, user=self)
        return return_value

    @classmethod
    def create(self, email, password, given_name, surname, username=None, middle_name=None, custom_data=None, status='ENABLED'):
        """
        Create a new User.

        Required Parameters
        -------------------

        :param str email: This user's unique email address.
        :param str password: This user's password, in plain text.
        :param str given_name: This user's first name (Randall).
        :param str surname: This user's last name (Degges).

        Optional Parameters
        -------------------

        :param str username: If no `username` is supplied, the `username` field
            will be set to the user's email address automatically.  Stormpath
            users can log in with either an `email` or `username` (both are
            interchangeable).
        :param str middle_name: This user's middle name ('Clark').
        :param dict custom_data: Any custom JSON data you'd like stored with
            this user.  Must be <= 10MB.
        :param str status: The user's status (*defaults to 'ENABLED'*). Can be
            either 'ENABLED', 'DISABLED', or 'UNVERIFIED'.

        If something goes wrong we'll raise an exception -- most likely -- a
        `StormpathError` (flask.ext.stormpath.StormpathError).
        """
        _user = current_app.stormpath_manager.application.accounts.create({
            'email': email,
            'password': password,
            'given_name': given_name,
            'surname': surname,
            'username': username,
            'middle_name': middle_name,
            'custom_data': custom_data,
            'status': status,
        })

        _user.__class__ = User
        user_created.send(self, user=_user)

        return _user

    @classmethod
    def from_login(self, login, password):
        """
        Create a new User class given a login (`email` or `username`), and
        password.

        If something goes wrong, this will raise an exception -- most likely --
        a `StormpathError` (flask.ext.stormpath.StormpathError).
        """
        #_user = current_app.stormpath_manager.application.authenticate_account(login, password).account

        auth_result = current_app.authClient.authenticate(login, password)
        account = auth_result.embedded.user
        return account


    def UpdateUserWithAccount(self,account):
        groups_info = current_app.relatedResourcesClient.get_user_in_groups(account.id)
        account_info = current_app.relatedResourcesClient.get_user(account.id)

        self.email = account_info['profile']['email']
        self.id = account.id
        self.username  = account.profile.login
        self.href = account.id

        self.given_name = account.profile.firstName
        self.surname = account.profile.lastName

        self.custom_data['activated'] =  account_info['profile']['activated']
        self.custom_data['agency'] = account_info['profile']['agency']
        self.custom_data['agent_code'] = account_info['profile']['agent_code']
        self.custom_data['ds_apikey'] = account_info['profile']['ds_apikey']
        if 'licensed_states' in account_info['profile']:
            self.custom_data['licensed_states'] = account_info['profile']['licensed_states']
        else:
            self.custom_data['licensed_states'] = []
        self.custom_data['signing_name'] = account_info['profile']['signing_name']


        self.status='ENABLED'
        self.groups = []
        for group in groups_info:
            matchObj = re.match(r'group:(.+?):(.*)', group.profile.name)
            if(matchObj):
                self.groups.append(AfbaOktaGroup(matchObj.group(2),group.id))
            elif not ':' in group.profile.name:
                if group.profile.name == 'Everyone':
                    continue
                self.groups.append(AfbaOktaGroup(group.profile.name,group.id))


    @classmethod
    def from_google(self, code):
        """
        Create a new User class given a Google access code.

        Access codes must be retrieved from Google's OAuth service (Google
        Login).

        If something goes wrong, this will raise an exception -- most likely --
        a `StormpathError` (flask.ext.stormpath.StormpathError).
        """
        _user = current_app.stormpath_manager.application.get_provider_account(
            code = code,
            provider = Provider.GOOGLE,
        )
        _user.__class__ = User

        return _user

    @classmethod
    def from_facebook(self, access_token):
        """
        Create a new User class given a Facebook user's access token.

        Access tokens must be retrieved from Facebooks's OAuth service (Facebook
        Login).

        If something goes wrong, this will raise an exception -- most likely --
        a `StormpathError` (flask.ext.stormpath.StormpathError).
        """
        _user = current_app.stormpath_manager.application.get_provider_account(
            access_token = access_token,
            provider = Provider.FACEBOOK,
        )
        _user.__class__ = User

        return _user
