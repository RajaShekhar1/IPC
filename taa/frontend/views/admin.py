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

from taa.services.agents import AgentService
agent_service = AgentService()


def search_stormpath_accounts(filter_email=None):
    """
    The flask-stormpath extension has some strange caching issues when using the 
    manager to query. Use the stormpath library directly here.
    """
    sp_app = get_stormpath_application()
    
    params = {}
    
    if filter_email:
        params['email'] = filter_email
    
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
             'is_user_admin': agent_service.is_user_admin(current_user),
         })
        #print dumps(dict(acc.custom_data), indent=2, sort_keys=True)
        
    #show the un-activated accounts first
    accounts = sorted(accounts, reverse=True, key=(lambda x: x['status']))
    return render_template('admin/admin.html', accounts=accounts, nav_menu=get_nav_menu())


@app.route('/edituser', methods = ['GET', 'POST'])
@groups_required(['admins'], all=False)
def updateUser():

    user_email = request.args['user']
    form = TAA_UserForm()
    
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
            form.agency.data = custom_data['agency'] if 'agency' in keyset else ""
            form.agent_code.data = custom_data['agent_code'] if 'agent_code' in keyset else ""
            form.ds_apikey.data = custom_data['ds_apikey'] if 'ds_apikey' in keyset else ""
            form.signing_name.data = custom_data['signing_name'] if 'signing_name' in keyset else ""
            form.activated.data = custom_data['activated'] if 'activated' in keyset else False
            #form.status.data = "Activated" if custom_data['activated'] else "Not Activated"
            
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
                
                # save your changes
                account.save()
                flash('User ' + user_email + ' updated successfully!')
                
                # Update in database also
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
                           form = form,
                           nav_menu=get_nav_menu()
    )


