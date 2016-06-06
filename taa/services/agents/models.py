
from taa import db
from taa.helpers import JsonSerializable


class AgentJsonSerializable(JsonSerializable):
    __json_public__ = ['id', 'first', 'last']
    __json_hidden__ = ['partner_cases']

class Agent(AgentJsonSerializable, db.Model):
    __tablename__ = 'agents'

    id = db.Column(db.Integer, primary_key=True)
    agent_code = db.Column(db.String(32), nullable=False, index=True)
    signing_name = db.Column(db.String(256))
    agency = db.Column(db.String(256))
    activated = db.Column(db.Boolean, nullable=False)

    password = db.Column(db.String)
    first = db.Column(db.String)
    last = db.Column(db.String)
    email = db.Column(db.String)
    phone = db.Column(db.String)

    stormpath_url = db.Column(db.String, index=True)

    def name(self):
        return self.first + " " + self.last


class ApiTokenJsonSerializable(JsonSerializable):
    pass

class ApiToken(AgentJsonSerializable, db.Model):
    __tablename__ = 'api_tokens'

    id = db.Column(db.Integer, primary_key=True)
    api_token = db.Column(db.String(64), nullable=False, index=True)
    activated = db.Column(db.Boolean, nullable=False)

    name = db.Column(db.String)

    stormpath_url = db.Column(db.String, index=True)
