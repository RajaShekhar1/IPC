# before we run this script, please disale all the okta sending email configurations from okta account
# so that user will not receive unexpected eamil
# make sure  python package   okta was installed
# pip install okta
# please also change line 42, the admin email information

import locale

# Make sure this is set for the whole app for formatting dates, times, currency, etc.
locale.setlocale(locale.LC_ALL, 'en_US.UTF-8')
from flask import Flask
from okta import UsersClient
from okta import UserGroupsClient
import os
import sys
import inspect


"""
currentdir = os.path.dirname(
    os.path.abspath(inspect.getfile(inspect.currentframe())))
parentdir = os.path.dirname(currentdir)
sys.path.insert(0, parentdir)

app = Flask(__name__,
            template_folder='frontend/templates',
            static_folder='frontend/static')
app.config.from_object('config_defaults')

usersClient =  UsersClient(app.config['OKTA_BASEURL'], app.config['OKTA_API_KEY_ID'])
groupAgent =  UserGroupsClient(app.config['OKTA_BASEURL'], app.config['OKTA_API_KEY_ID'])
"""

usersClient =  UsersClient('https://dev-56833.oktapreview.com', '00s6SBfuzuBBa_dOHF4zbMEW3Jzljr4xv8FqoeeHbT')
groupAgent =  UserGroupsClient('https://dev-56833.oktapreview.com','00s6SBfuzuBBa_dOHF4zbMEW3Jzljr4xv8FqoeeHbT')


users = usersClient.get_users()
for user in users:
    email = user.profile.login
    # make sure we do not delete the admin account of okta
    if email == "admin@verizon.net":
        continue
    else:
        id = user.id
        strid= id.encode('ascii', 'ignore')
        this_user = usersClient.deactivate_user( strid )
        try:
            usersClient.delete_user(strid)
        except ValueError:
            print("User %s removed" % (strid))

groups =  groupAgent.get_groups()
for group in groups:
    if group.profile.name == "Everyone":
        continue
    try:
        groupAgent.delete_group(group.id)
    except ValueError:
        print("Group %s Removed" % (group.id))

