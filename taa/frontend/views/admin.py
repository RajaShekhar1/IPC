from flask import (
    render_template,
    url_for,
    flash,
    redirect,
    request,
)
from flask.ext.stormpath import groups_required, StormpathError, current_user
from stormpath.client import Client as SPClient

from taa import (
    app,
    stormpath_manager,
)

from nav import get_nav_menu
from taa.models import db
from taa.old_model.Registration import TAA_UserForm
from taa.old_model.Enrollment import AgentActivationEmail
from taa.services import LookupService

agent_service = LookupService('AgentService')
api_token_service = LookupService('ApiTokenService')


def search_stormpath_accounts(filter_email=None, filter_href=None):
    """
    The flask-stormpath extension has some strange caching issues when using the
    manager to query. Use the stormpath library directly here.
    """
    sp_app = get_stormpath_application()

    params = {}

    if filter_email:
        params['email'] = filter_email
    if filter_href:
        params['href'] = filter_href

    if params:
        return [a for a in sp_app.accounts.search(params)]
    else:
        return [a for a in sp_app.accounts]

def get_stormpath_application():
    app_name = app.config['STORMPATH_APPLICATION']
    c = SPClient(id=app.config['STORMPATH_API_KEY_ID'],
               secret=app.config['STORMPATH_API_KEY_SECRET'])

    for sp_app in c.applications:
        if sp_app.name == app_name:
            return sp_app

    raise Exception('The configured stormpath application "%s" could not be found'%app_name)

#  14-Jun-17 WSD
@app.route('/admin', methods = ['GET', 'POST'])
@groups_required(['admins', 'home_office'], all=False)
def admin():
    accounts = []

    for acc in search_stormpath_accounts():
        accounts.append(
            {'fname': acc.given_name,
             'lname': acc.surname,
             'email': acc.email,
             'agency': acc.custom_data.get('agency'),
             'agent_code': acc.custom_data.get('agent_code'),
             'signing_name': acc.custom_data.get('signing_name'),
             'status': "Activated" if acc.custom_data.get('activated') else "Not Activated",
         })
        #print dumps(dict(acc.custom_data), indent=2, sort_keys=True)

    #show the un-activated accounts first
    accounts = sorted(accounts, reverse=True, key=(lambda x: x['status']))
    return render_template('admin/admin.html', accounts=accounts, nav_menu=get_nav_menu(), is_user_admin=agent_service.is_user_admin(current_user))

@app.route('/edituser', methods = ['GET', 'POST'])
@groups_required(['admins', 'home_office'], all=False)
def updateUser():
    user_email = request.args['user']
    form = TAA_UserForm()
    group_membership = []
    token = None

    # initially pre-populate the form with account values
    if request.method == 'GET':
        accounts = search_stormpath_accounts(filter_email=user_email)
        if not accounts:
            flash('Failed to find user ' + user_email)
            return redirect(url_for('admin'))
        else:
            account = accounts[0]

            custom_data = account.custom_data
            keyset = custom_data.keys()
            form.fname.data = account.given_name
            form.lname.data = account.surname
            form.email.data = account.email

            group_membership = account.group_memberships

            token = api_token_service.get_token_by_sp_href(account.href)

            form.agency.data = custom_data['agency'] if 'agency' in keyset else ""
            form.agent_code.data = custom_data['agent_code'] if 'agent_code' in keyset else ""
            form.ds_apikey.data = custom_data['ds_apikey'] if 'ds_apikey' in keyset else ""
            form.signing_name.data = custom_data['signing_name'] if 'signing_name' in keyset else ""
            form.activated.data = custom_data['activated'] if 'activated' in keyset else False
            #form.status.data = "Activated" if custom_data['activated'] else "Not Activated"

    sp_app = get_stormpath_application()
    all_groups = {g.name: g for g in sp_app.groups}
    all_group_names = [g.name for g in sp_app.groups]

    if form.validate_on_submit():
        try:
            accounts = search_stormpath_accounts(filter_email=user_email)
            if not accounts:
                flash('Failed to find user ' + user_email)
            else:
                account = accounts[0]
                data = form.data

                # edit some custom data fields
                account.given_name = data['fname']
                account.surname = data['lname']
                account.email = data['email']

                account.custom_data['agency'] = data['agency']
                account.custom_data['agent_code'] = data['agent_code']
                account.custom_data['signing_name'] = data['signing_name']
                account.custom_data['ds_apikey'] = data['ds_apikey']
                account.custom_data['activated'] = data['activated']

                groups = request.values.getlist("groups")

                token = api_token_service.get_token_by_sp_href(account.href)

                # "api_users" is not a real group, we just pass it along with the groups to indicate if the user
                #  has an active api token or not.
                if "api_users" not in groups:
                    if token:
                        token.activated = False
                        db.session.commit()
                else:
                    if token:
                        token.activated = True
                        db.session.commit()
                    else:
                        full_name = "{} {}".format(account.given_name, account.surname)
                        api_token_service.create_new_token(full_name, account.href, activated=True)
                        db.session.commit()

                # If the account has a group membership that is not in the posted data, delete it
                for gms in account.group_memberships:
                    if gms.group.name not in groups:
                        gms.delete()

                # For each group name that is in the posted data,
                #  add a membership link between the account and the group
                # TODO: See if there is a way to get rid of these list comprehension (something like a .get() function from Stormpath)
                for group in groups:
                    matching_groups = [item
                                       for item in sp_app.groups.items
                                       if item.name==group]
                    if not matching_groups:
                        continue
                    sp_group = matching_groups[0]
                    existing_membership = [acct_mem
                                           for acct_mem in sp_group.account_memberships
                                           if acct_mem.account.email==account.email]
                    if not existing_membership:
                        # Add this account to the group
                        sp_group.add_account(account)

                # save your changes
                account.save()
                flash('User ' + user_email + ' updated successfully!')

                # Update in database also
                if 'agents' in groups:
                    agent = agent_service.ensure_agent_in_database(account)
                    agent_service.update(agent, **{
                        'first': data['fname'],
                        'last': data['lname'],
                        'agent_code': data['agent_code'],
                        'activated': data['activated']
                    })
                db.session.commit()

                # if we've just activated a user, then send a notice
                if data['activated'] and data['send_notice']:
                    try:
                        AgentActivationEmail().send_activation_notice(data['email'], data['fname'], url_for('home'))
                        flash('Activation email sent.')
                    except:
                        flash('>> Problem sending activation email <<')
                        print('>> Problem sending activation email <<')


            return redirect(url_for('admin'))
        except StormpathError as err:
            flash(err.message['message'])


    return render_template('admin/update-user.html',
                           form=form,
                           group_membership=[g.group.name for g in group_membership],
                           groups=all_group_names,
                           token=token,
                           nav_menu=get_nav_menu()
    )
