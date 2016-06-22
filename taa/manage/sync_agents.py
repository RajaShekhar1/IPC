
from flask import current_app
from flask_script import Command, prompt, prompt_pass
from werkzeug.datastructures import MultiDict

from ..services.agents import AgentService
from ..models import db
from taa.services.users.UserService import search_stormpath_accounts

agent_service = AgentService()


class SyncAgentsCommand(Command):
    """Pull agents from stormpath and add them to the DB"""

    def run(self):
        sync_agents()


def sync_agents():
    all_accounts = search_stormpath_accounts()

    valid_account_hrefs = set()

    for account in all_accounts:
        agent_service.ensure_agent_in_database(account)
        valid_account_hrefs.add(account.href)

    db.session.commit()

    # Second pass to mark as deleted accounts that are not in the stormpath database
    agents = agent_service.all()
    for agent in agents:
        if agent.stormpath_url not in valid_account_hrefs:
            agent.is_deleted = True

    db.session.commit()
