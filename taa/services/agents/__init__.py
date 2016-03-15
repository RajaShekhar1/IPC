
from flask_stormpath import current_user

from taa.services import RequiredFeature
from taa.core import DBService, db
from models import Agent, ApiToken

import uuid


class AgentService(DBService):
    user_service = RequiredFeature("UserService")

    __model__ = Agent

    def get_logged_in_agent(self):
        if not current_user:
            return

        return self.get_agent_from_user(current_user)

    def ensure_agent_in_database(self, user):
        if not self.is_user_agent(user):
            return None

        stormpath_url = user.href

        existing_agent = self.find(stormpath_url=stormpath_url).first()
        if not existing_agent:
            existing_agent = self.create(
                first=user.given_name,
                last=user.surname,
                email=user.email,
                agent_code=user.custom_data.get('agent_code', ""),
                activated=user.custom_data.get('activated', False),
                stormpath_url=stormpath_url,
                signing_name=user.custom_data.get('signing_name', ""),
                agency=user.custom_data.get('agency', ""),
            )
        else:
            existing_agent.first = user.given_name
            existing_agent.last = user.surname
            existing_agent.email = user.email
            existing_agent.agent_code = user.custom_data.get('agent_code', "")
            existing_agent.signing_name = user.custom_data.get('signing_name', "")
            existing_agent.agency = user.custom_data.get('agency', "")

            db.session.commit()

        return existing_agent

    def get_agent_from_user(self, user):
        return self.ensure_agent_in_database(user)

    def is_user_agent(self, user):
        return 'agents' in self.get_user_groupnames(user)

    def is_user_admin(self, user):
        return 'admins' in self.get_user_groupnames(user)

    def is_user_enrollment_importer(self, user):
        return 'enrollment_importers' in self.get_user_groupnames(user)

    def is_user_home_office(self, user):
        return 'home_office' in self.get_user_groupnames(user)

    def is_user_third_party(self, user):
        return not any([self.is_user_home_office(user), self.is_user_admin(user), self.is_user_agent(user)])

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
            ).order_by(Agent.last, Agent.first
            ).all()


class ApiTokenService(DBService):
    __model__ = ApiToken

    user_service = RequiredFeature("UserService")

    def get_token_by_sp_href(self, sp_href):
        "Does a given user's StormPath href have an associated API auth token?"
        return self.find(stormpath_url=sp_href).first()

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
