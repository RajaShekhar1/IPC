
from taa.core import DBService, db

from models import Agent

class AgentService(DBService):
    
    __model__ = Agent
    
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
            )
        
        return existing_agent
    
    def get_agent_from_user(self, user):
        return self.ensure_agent_in_database(user)
    
    def is_user_agent(self, user):
        return 'agents' in [g.name for g in user.groups]
    
    def get_agent_from_stormpath_account(self, stormpath_account_url):
        pass