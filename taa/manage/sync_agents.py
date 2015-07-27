
from flask import current_app
from flask_script import Command, prompt, prompt_pass
from werkzeug.datastructures import MultiDict

from ..services.agents import AgentService
from ..models import db
from ..frontend.views.admin import search_stormpath_accounts

agent_service = AgentService()


class SyncAgentsCommand(Command):
    """Pull agents from stormpath and add them to the DB"""

    def run(self):
        sync_agents()


def sync_agents():
    all_accounts = search_stormpath_accounts()
    for account in all_accounts:
        agent_service.ensure_agent_in_database(account)
    db.session.commit()
