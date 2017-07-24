from okta.framework import OktaError
from sqlalchemy.dialects.postgresql import JSON
from flask_login import UserMixin

from taa import db
from taa.helpers import JsonSerializable


class AgentJsonSerializable(JsonSerializable):
    __json_public__ = ['id', 'first', 'last']
    __json_hidden__ = ['partner_cases']

class Agent(UserMixin, AgentJsonSerializable, db.Model):
    __tablename__ = 'agents'

    id = db.Column(db.Integer, primary_key=True)
    agent_code = db.Column(db.String(32), nullable=False, index=True)
    signing_name = db.Column(db.String(256))
    agency = db.Column(db.String(256))
    activated = db.Column(db.Boolean, nullable=False)
    is_deleted = db.Column(db.Boolean, server_default='FALSE')

    username = db.Column(db.String)
    password = db.Column(db.String)
    first = db.Column(db.String)
    last = db.Column(db.String)
    email = db.Column(db.String)
    phone = db.Column(db.String)

    stormpath_url = db.Column(db.String, index=True)
    okta_id = db.Column(db.String, index=True)



    custom_data = db.Column(JSON(none_as_null=False), nullable=True)


    def __init__(self, *args, **kwargs):
        db.Model.__init__(self, *args, **kwargs)

    def name(self):
        return self.first + " " + self.last

    def get_status(self):
        if self.is_deleted:
            return 'Deleted'
        elif not self.activated:
            return 'Not Activated'
        else:
            return 'Activated'

    def get_id(self):
        if self.okta_id:
            return self.okta_id
        elif self.is_deleted:
            # Don't bother going to Okta for old deleted accounts, just use old UID
            return self.stormpath_url

        #from okta import UsersClient
        from taa import app
        #users_client = UsersClient(app.config['OKTA_DOMAIN'], app.config['OKTA_API_KEY'])
        #try:

        from taa.services.agents import OktaService
        u = OktaService().get_user_data(self.email)
        #except OktaError:
        #    u = None

        if not u:
            return None

        # Save the Okta ID
        self.okta_id = u.id
        db.session.commit()

        return unicode(u.id)

    def is_in_group(self, group):
        return self.is_in_groups([group])

    def is_in_groups(self, groups, member_of_all=True):
        # Gather group names we are members of
        member_group_names = [g.group for g in self.groups]
        if member_of_all:
            for group_name in groups:
                if group_name not in member_group_names:
                    return False
            return True
        else:
            # Any group is fine
            for group_name in groups:
                if group_name in member_group_names:
                    return True
            return False

    def add_group(self, group_name):
        if not self.is_in_groups([group_name]):
            self.groups.append(AgentGroups(group=group_name))

        db.session.commit()

    def remove_group(self, group_name):
        for g in self.groups:
            if g.group == group_name:
                db.session.delete(g)
        db.session.commit()
class ApiTokenJsonSerializable(JsonSerializable):
    pass

class ApiToken(AgentJsonSerializable, db.Model):
    __tablename__ = 'api_tokens'

    id = db.Column(db.Integer, primary_key=True)
    api_token = db.Column(db.String(64), nullable=False, index=True)
    activated = db.Column(db.Boolean, nullable=False)

    name = db.Column(db.String)

    stormpath_url = db.Column(db.String, index=True)

class AgentGroups(db.Model):
    __tablename__ = "agent_groups"
    
    id = db.Column(db.Integer, primary_key=True)
    agent_id = db.Column(db.Integer, db.ForeignKey('agents.id'), index=True)
    agent = db.relationship('Agent', backref='groups')
    group = db.Column(db.Unicode)
