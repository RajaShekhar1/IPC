
from flask import current_app
from flask_script import Command, Option, prompt, prompt_pass
from werkzeug.datastructures import MultiDict
from stormpath.client import Client as SPClient

from ..services.agents import AgentService, Agent
from ..models import db
from taa.services.users.UserService import search_stormpath_accounts

agent_service = AgentService()



class TransferStormpathCommand(Command):
    """
    Overwrites old agent Stormpath HREFs with their counterpart record in a new Stormpath directory/account/application thing.
    Should not be run more than once, since it will first purge all accounts that are in the new stormpath account already by Href.
    """
    
    option_list = (
        Option('-o', '--old-key', dest='old_sp_api_key', required=True,
               help="API key for old stormpath"),
        Option('-os', '--old-secret', dest='old_sp_api_secret', required=True,
               help="API secret for old stormpath"),

        Option('-n', '--new-key', dest='new_sp_api_key', required=True,
               help="API key for new stormpath"),
        Option('-ns', '--new-secret', dest='new_sp_api_secret', required=True,
               help="API secret for new stormpath"),
    )

    def run(self, old_sp_api_key, old_sp_api_secret, new_sp_api_key, new_sp_api_secret):
        transfer_stormpath(old_sp_api_key, old_sp_api_secret, new_sp_api_key, new_sp_api_secret)




def transfer_stormpath(old_sp_api_key, old_sp_api_secret, new_sp_api_key, new_sp_api_secret):
    #old_sp = SPClient(id=old_sp_api_key, secret=old_sp_api_secret)
    new_sp = SPClient(id=new_sp_api_key, secret=new_sp_api_secret)
    app_name = current_app.config['STORMPATH_APPLICATION']
    
    #old_app = None
    #for sp_app in old_sp.applications:
    #    if sp_app.name == app_name:
    #        old_app = sp_app
    

    new_app = None
    for sp_app in new_sp.applications:
        if sp_app.name == app_name:
            new_app = sp_app
    
    print("Fetching all stormpath accounts")
    #old_accounts = [a for a in old_app.accounts]
    new_accounts = [a for a in new_app.accounts]
    
    # First mark all users as not-deleted in our database
    mark_all_undeleted()
    
    # Find and remove (after prompting) all accounts that have hrefs in the new SP (duplicates).
    accounts_from_new = find_existing_accounts_in_db(new_accounts)
    if accounts_from_new:
        text_input = prompt("Delete {} accounts? Y/N".format(len(accounts_from_new)))
        if text_input.lower() == 'y':
            delete_db_accounts(accounts_from_new)
    
    # For each user in our agents table, find a matching user in the new SP via email
    #  printout any that do not match
    agent_hrefs = find_new_agent_hrefs(new_accounts)
    
    # Overwrite the old SP href for users with the new SP href
    ans = prompt("Overwrite href for {} users? (Y/n)".format(len(agent_hrefs)))
    if ans.lower() == 'y':
        overwrite_hrefs(agent_hrefs)
    
    # Commit all changes to the database.
    db.session.commit()
    
    # Re-run sync script against new stormpath account as a manual step.
    

def mark_all_undeleted():
    for user in db.session.query(Agent):
        user.is_deleted = False
    
    db.session.flush()


def find_existing_accounts_in_db(new_accounts):
    href_mapping = {
        a.href: a
        for a in new_accounts
    }
    
    return [
        agent
        for agent in db.session.query(Agent)
        if agent.stormpath_url in href_mapping
    ]
    

def delete_db_accounts(accounts_from_new):
    for agent in accounts_from_new:
        db.session.delete(agent)

    db.session.flush()
    

def find_new_agent_hrefs(new_accounts):
    email_mapping = {
        a.email: a
        for a in new_accounts
        }

    agent_hrefs = {}
    
    for agent in db.session.query(Agent):
        if agent.email not in email_mapping:
            print("FAIL: could not find user in new stormpath: '{}'".format(agent.email))
            continue
        agent_hrefs[agent] = email_mapping[agent.email].href
    
    return agent_hrefs


def overwrite_hrefs(agent_hrefs):
    for agent, href in agent_hrefs.iteritems():
        agent.stormpath_url = href
        
    db.session.flush()

