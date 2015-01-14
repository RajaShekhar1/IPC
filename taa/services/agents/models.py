
from taa import db
from taa.helpers import JsonSerializable


class AgentJsonSerializable(JsonSerializable):
    __json_public__ = ['id', 'first', 'last', 'email']
    __json_hidden__ = ['partner_cases']

class Agent(AgentJsonSerializable, db.Model):
    __tablename__ = 'agents'
    
    id = db.Column(db.Integer, primary_key=True)
    agent_code = db.Column(db.String(32), nullable=False)
    activated = db.Column(db.Boolean, nullable=False)
    
    password = db.Column(db.String)
    first = db.Column(db.String)
    last = db.Column(db.String)
    email = db.Column(db.String)
    phone = db.Column(db.String)
    
    stormpath_url = db.Column(db.String)
    
    def name(self):
        return self.first + " " + self.last