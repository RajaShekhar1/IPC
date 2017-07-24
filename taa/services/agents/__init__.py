
import json
import urllib
import uuid

from flask import current_app
from flask_login import current_user
import requests

from taa.services import RequiredFeature
from taa.core import DBService, db
from models import Agent, ApiToken, AgentGroups



class AgentService(DBService):
    user_service = RequiredFeature("UserService")

    __model__ = Agent

    def create_user(self, email, password, given_name, middle_name, surname, activated, signing_name, agent_code, agency, okta_id):

        # Check duplicate email error, although this should not happen here.
        if db.session.query(Agent).filter(Agent.email == email).first():
            raise Exception(u"Attempt to create a user with email '{}' already existing in database.".format(email))

        agent = Agent(
            email=email,
            password=None,
            first=given_name,
            last=surname,
            custom_data=dict(middle_name=middle_name),
            signing_name=signing_name,
            agent_code=agent_code,
            agency=agency,
            activated=activated,
            is_deleted=False,
            okta_id=okta_id,
        )
        db.session.add(agent)
        db.session.flush()
        return agent

    def user_from_login(self, username, password):

        okta_id = OktaService().authenticate_user(username, password)

        if okta_id is None:
            return None

        # Look up user
        user = db.session.query(Agent).filter(Agent.okta_id == okta_id).first()
        if not user:
            # Sync with Okta
            okta_user = OktaService().get_user_data(okta_id)
            user = self.ensure_agent_in_database(okta_user)

        return user
        
    def get_sorted_agents(self):
        return db.session.query(Agent
            ).filter(Agent.is_deleted != True
            ).order_by(Agent.activated, Agent.last, Agent.first)

    def get_logged_in_agent(self):
        if not current_user:
            return

        return self.get_agent_from_user(current_user)

    def ensure_agent_in_database(self, okta_user):

        existing_agent = self.find(okta_id=okta_user.id).first()
        if not existing_agent:
            existing_agent = self.create(
                first=okta_user.first_name,
                last=okta_user.last_name,
                email=okta_user.email,
                agent_code=okta_user.agent_code,
                okta_id=okta_user.id,
                signing_name=okta_user.signing_name,
                agency=okta_user.agency,
                activated=okta_user.activated,
                is_deleted=False,
            )
        else:
            existing_agent.first = okta_user.first_name
            existing_agent.last = okta_user.last_name
            existing_agent.email = okta_user.email
            existing_agent.agent_code = okta_user.agent_code
            existing_agent.signing_name = okta_user.signing_name
            existing_agent.agency = okta_user.agency
            existing_agent.activated = okta_user.activated
            existing_agent.okta_id = okta_user.id

        # Update groups
        group_names = OktaService().get_user_groups(okta_user.id)

        # Remove existing groups not in the group names
        for existing_group_name in [g.group for g in existing_agent.groups]:
            if existing_group_name not in group_names:
                existing_agent.remove_group(existing_group_name)
        
        # Add new groups
        for g in group_names:
            if not existing_agent.is_in_group(g):
                existing_agent.add_group(g)


        db.session.commit()

        return existing_agent

    def get_agent_from_user(self, user):
        if self.is_user_agent(user):
            return user #self.ensure_agent_in_database(user)
        else:
            # Ensure the user is imported from Stormpath, but since they are
            # not an agent, return nothing
            #self.ensure_agent_in_database(user)
            return None

    def is_user_agent(self, user):
        return 'agents' in self.get_user_groupnames(user)

    def is_user_admin(self, user):
        return 'admins' in self.get_user_groupnames(user)

    def is_user_enrollment_importer(self, user):
        return 'enrollment_importers' in self.get_user_groupnames(user)

    def is_user_home_office(self, user):
        return 'home_office' in self.get_user_groupnames(user)

    def is_user_case_admin(self, user):
        return 'case_admins' in self.get_user_groupnames(user)

    def is_user_third_party(self, user):
        return not any([self.is_user_home_office(user),
                        self.is_user_admin(user),
                        self.is_user_agent(user),
                        self.is_user_case_admin(user)])

    def is_user_third_party_enroller(self, user):
        return self.is_user_third_party(user) and self.is_user_enrollment_importer(user)

    def can_manage_all_cases(self, user):
        return self.is_user_admin(user) or self.is_user_home_office(user)

    def get_user_groupnames(self, user):
        return self.user_service.get_user_groupnames(user)

    def get_agent_stormpath_account(self, agent):
        return self.user_service.get_stormpath_user_by_href(agent.stormpath_url)

    def get_active_agents(self):
        return self.query(
            ).filter(Agent.activated == True
            ).filter(Agent.is_deleted != True
            ).order_by(Agent.last, Agent.first
            ).all()

    def get_active_case_admins(self):
        import flask_stormpath
        return self.query(
        ).filter(Agent.activated == True
                 ).order_by(Agent.last, Agent.first
                            ).all()


class OktaService(object):
    def __init__(self):
        self.last_error_message = None

    def authenticate_user(self, username, password):
        from okta import AuthClient
        from okta.framework.OktaError import OktaError
        auth_client = AuthClient(current_app.config['OKTA_DOMAIN'], current_app.config['OKTA_API_KEY'])

        try:
            resp = auth_client.authenticate(username, password)
        except OktaError as err:
            print "okta exception:"
            print err
            return None

        print "authentication response was:"
        print resp

        if resp.status != u'SUCCESS':
            return None

        return resp.embedded.user.id

    def get_user_data(self, okta_id):

        data = self._get_request(u'/api/v1/users/{}'.format(okta_id))
        if not data:
            return None

        return OktaUser(data)

    def get_user_by_email(self, email):
        import urllib
        data = self._get_request(u'/api/v1/users?q={}'.format(urllib.quote_plus(email)))
        if not data:
            return None

        mapped = [OktaUser(d) for d in data]
        if len(mapped) == 0:
            return None
        else:
            return mapped[0]

    def get_all_users(self):

        data = self._get_paged_request(u'/api/v1/users')
        if not data:
            return []

        return [OktaUser(d) for d in data]

    def _get_request(self, path):
        resp = requests.get(current_app.config['OKTA_DOMAIN'] + path, headers=self._get_headers())
        if resp.status_code >= 400:
            return None

        return resp.json()

    def _get_paged_request(self, path, limit=50):
        resp = requests.get(current_app.config['OKTA_DOMAIN'] + path + u"?limit={}".format(limit), headers=self._get_headers())
        if resp.status_code >= 400:
            return None

        data = resp.json()
        while resp.links.get('next'):
            resp = requests.get(resp.links['next']['url'], headers=self._get_headers())
            if resp.status_code >= 400:
                return None

            data += resp.json()

        return data

    def _get_headers(self):
        return {
            'Accept': 'application/json',
            'Content-Type': 'application/json',
            'Authorization': u'SSWS {}'.format(current_app.config['OKTA_API_KEY'])
        }

    def create_user(self, data):
        resp = requests.post(current_app.config['OKTA_DOMAIN'] + u"/api/v1/users", json=data, headers=self._get_headers())
        if resp.status_code >= 400:
            print(u"Error creating new user with profile: {}\n{}".format(data.get('profile', u''), resp.text))
            error_causes = resp.json().get('errorCauses', [])
            if error_causes:
                self.last_error_message = error_causes[0].get('errorSummary')

            return None

        return resp.json()

    def update_user(self, okta_id, data):
        if not okta_id:
            raise ValueError("Attempted to update user without okta_id")

        # Post to user is like a PATCH operation
        resp = requests.post(
            current_app.config['OKTA_DOMAIN'] + u"/api/v1/users/{}".format(okta_id),
            json=data,
            headers=self._get_headers())
        if resp.status_code >= 400:
            from taa.tasks import send_admin_error_email
            print(u"Error updating user {}".format(okta_id))
            send_admin_error_email("Error updating user {}\n\n{}".format(okta_id, resp.text), [])
            return None

        return True

    def set_user_groups(self, user, groups):

        for okta_user_group in self.get_user_groups(user.okta_id):
            if okta_user_group not in groups:
                okta_group = self.get_group_by_name(okta_user_group)
                self.remove_user_group(user, okta_group)

        for group_name in groups:
            existing_okta_group = self.get_group_by_name(group_name)
            if not existing_okta_group:
                existing_okta_group = self.add_group(group_name)

            # Add user to group
            self.add_user_group(user, existing_okta_group)

    def get_user_groups(self, okta_id):
        groups = self._get_request(u'/api/v1/users/{}/groups'.format(okta_id))
        if not groups:
            return []

        user_groups = set()
        for g in groups:
            if g['profile']['name'].startswith('group') and g['profile']['name'].split(':')[-1] != 'Everyone':
                # When initially importing from stormpath, we user the imported names
                user_groups.add(g['profile']['name'].split(':')[-1])
            elif g['profile']['name'].startswith('EAPP'):
                user_groups.add(g['profile']['name'].split(':')[-1])

        return user_groups

    def add_user_group(self, user, okta_group):
        resp = requests.put(
            current_app.config['OKTA_DOMAIN'] + u"/api/v1/groups/{}/users/{}".format(okta_group['id'], user.okta_id),
            headers=self._get_headers())

        if resp.status_code >= 400:
            from taa.tasks import send_admin_error_email
            msg = u"Error adding group {} to user {}: {}".format(okta_group['profile']['name'], user.okta_id, resp.text)
            print(msg)
            send_admin_error_email(msg, [])
            return None

        return True

    def remove_user_group(self, user, okta_group):
        resp = requests.delete(
            current_app.config['OKTA_DOMAIN'] + u"/api/v1/groups/{}/users/{}".format(okta_group['id'], user.okta_id),
            headers=self._get_headers())

        if resp.status_code >= 400:
            from taa.tasks import send_admin_error_email
            msg = u"Error removing group {} from user {}: {}".format(okta_group['profile']['name'], user.okta_id, resp.text)
            print(msg)
            send_admin_error_email(msg, [])
            return None

        return True

    def get_group_by_name(self, group_name):
        okta_groups = self._get_request(u'/api/v1/groups?q={}'.format(urllib.quote_plus(u'EAPP:'+group_name)))
        if not okta_groups:
            return None
        else:
            return okta_groups[0]

    def add_group(self, group_name):
        resp = requests.post(current_app.config['OKTA_DOMAIN'] + u"/api/v1/groups",
                             json=dict(profile=dict(name=u'EAPP:{}'.format(group_name), description="EApp group")),
                             headers=self._get_headers())
        if resp.status_code >= 400:
            from taa.tasks import send_admin_error_email
            print(u"Error adding group {}".format(group_name))
            send_admin_error_email("Error adding group {}\n\n{}".format(group_name, resp.text), [])
            return None

        return resp.json()

    def delete_user(self, okta_id):
        # Deactivates user in Okta
        resp = requests.post(current_app.config['OKTA_DOMAIN'] + u"/api/v1/users/{}/lifecycle/deactivate".format(okta_id), headers=self._get_headers())
        if resp.status_code >= 400:
            from taa.tasks import send_admin_error_email
            print(u"Error deactivating user {}".format(okta_id))
            send_admin_error_email("Error deactivating user {}\n\n{}".format(okta_id, resp.text), [])
            return None

        return True

    def reset_password(self, email):
        resp = requests.post(current_app.config['OKTA_DOMAIN'] + u"/api/v1/users/{}/lifecycle/reset_password?sendEmail=true".format(
            email), headers = self._get_headers())
        if resp.status_code >= 400:
            from taa.tasks import send_admin_error_email
            print(u"Error sending forgotten user password email to '{}'\n\n{}".format(email, resp.json()['errorSummary']))
            #send_admin_error_email("Error deactivating user {}\n\n{}".format(email, resp.text), [])
            self.last_error_message = "Invalid email address."
            return False

        return True


class OktaUser(object):
    def __init__(self, data):
        self.id = data['id']
        self.first_name = data['profile']['firstName']
        self.middle_name = data['profile'].get('middleName')
        self.last_name = data['profile']['lastName']
        self.email = data['profile']['email']
        self.agent_code = data['profile'].get('agent_code', u'NO CODE')
        self.agency = data['profile'].get('agency', u'')
        self.signing_name = data['profile'].get('signing_name', u'{} {}'.format(self.first_name, self.last_name))
        self.stormpath_href = data['profile'].get('stormpathHref')
        self.activated = bool(data['activated'])
        self.created = data['created']
        self.status = data['status']

class ApiTokenService(DBService):
    __model__ = ApiToken

    user_service = RequiredFeature("UserService")

    def get_token_by_sp_href(self, okta_id):
        "Does a given user's StormPath href have an associated API auth token?"
        return self.find(stormpath_url=okta_id).first()

    def get_sp_user_by_token(self, token):
        token_row = self.get_row_by_token(token)
        if not token_row:
            return None
        else:
            return self.user_service.get_stormpath_user_by_href(token_row.stormpath_url)

    def is_valid_token(self, token):
        return bool(self.get_row_by_token(token))

    def get_row_by_token(self, token):
        return self.find(api_token=token, activated=True).first()

    def create_new_token(self, name, sp_href, activated=False):
        new_token = self.create(**dict(
            api_token=uuid.uuid4().hex,
            name=name,
            stormpath_url=sp_href,
            activated=activated
        ))
        return new_token
