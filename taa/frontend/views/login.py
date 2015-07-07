
from flask import (
    flash,
    redirect,
    render_template,
    request,
    session,
    url_for,
)
from flask.ext.stormpath import (
    StormpathError,
    User,
    login_user,
    logout_user,
    user,
)

from taa import app
from .nav import get_nav_menu
from taa.old_model.Registration import TAA_RegistrationForm, TAA_LoginForm
from taa.old_model.Enrollment import NotifyAdminEmail
from taa.services.agents import AgentService

@app.route("/user_register", methods=['GET', 'POST'])
def register_taa():
    """
    Register a new user with Stormpath, adding TAA customizations.
    Original of this function can be found in
    https://github.com/stormpath/stormpath-flask/blob/master/flask_stormpath/views.py#L21

    """
    form = TAA_RegistrationForm()

    data = form.data
    
    if form.validate_on_submit():
        fail = False

        # Iterate through all fields, grabbing the necessary form data and
        # flashing error messages if required.
        
        data = form.data
        
        if data['password'] != data['repassword']:
            fail = True
            flash("Passwords don't match.")

        # If there are no missing fields (per our settings), continue.
        if not fail:

            # Attempt to create the user's account on Stormpath.
            try:

                # Create the user account on Stormpath.  If this fails, an
                # exception will be raised.
                account = User.create(
                    email = data['email'],
                    password = data['password'],
                    given_name = data['given_name'] or 'Anonymous',
                    middle_name = data.get('middle_name'),
                    surname = data['surname'] or 'Anonymous',
                    custom_data = {
                        'signing_name': data['signing_name'],
                        'agent_code': data['agent_code'],
                        'agency': data['agency'],
                        'ds_apikey': "",
                        'activated': False,
                    },
                )
                
                # Add to the agents group
                account.add_group("agents")
                
                # If successfully created account, notify admin of registration
                #try:
                NotifyAdminEmail().send_registration_notice(data['given_name'] + " " + data['surname'])
                #except Exception as e:
                #    print " -- Problem sending registration notice to admin --\n%s"%e

                session['registered_name'] = data['given_name']

                return redirect(url_for('confirmRegistration'))
            except StormpathError as err:
                flash(err.message['message'])

    return render_template('user_account/register.html', form=form, nav_menu=get_nav_menu())

@app.route("/registration_confirmed")
def confirmRegistration():
    return render_template('user_account/registration_complete.html',
                           name = session['registered_name'],
                           nav_menu=get_nav_menu(),
                           )


@app.route('/login', methods = ['GET', 'POST'])
def login():
    """
    Log in an existing Stormpath user.
    """
    form = TAA_LoginForm()

    # If we received a POST request with valid information, we'll continue
    # processing.
    if form.validate_on_submit():
        try:
            # Try to fetch the user's account from Stormpath.  If this
            # fails, an exception will be raised.
            account = User.from_login(form.login.data, form.password.data)

            # If we're able to successfully retrieve the user's account,
            # we'll log the user in (creating a secure session using
            # Flask-Login), then redirect the user to the ?next=<url>
            # query parameter, or just the HOME page

            print "LOGIN: %s %s, (%s  activated=%s)" % (account.given_name, account.surname, account.email, account.custom_data.get('activated'))
            account_groups = [g.name for g in account.groups]
            is_agent = 'agents' in account_groups
            is_home_office = 'home_office' in account_groups
            is_admin = 'admins' in account_groups
            print "Is ADMIN: %s, GROUPS: %s"%(is_admin, account_groups)
            if is_admin or is_home_office or (is_agent and account.custom_data.get('activated')):
                login_user(account, remember=True) 
                session['username'] = user.given_name + " " + user.surname
                session['headername'] = session['username']

                if is_agent and user.custom_data.get('agency') and user.custom_data['agency'].strip(): 
                    session['headername'] += ", " + user.custom_data['agency']
                elif is_admin:
                    session['headername'] += ', Global Administrator'
                elif is_home_office:
                    session['headername'] += ', Home Office Administrator' 
                
                session['active_case'] = {
                    'company_name': "",
                    'situs_state': "",
                    'situs_city': "",
                    'product_code': ""
                }
                
                if is_agent:
                    AgentService().ensure_agent_in_database(user)
                
                if is_admin:
                    return redirect(request.args.get('next') or url_for('admin'))
                elif is_agent:
                    return redirect(request.args.get('next') or url_for('home'))
                else:
                    # Home office
                    return redirect(request.args.get('next') or url_for('home_office_dashboard'))
            else:
                flash(account.given_name + ", your account (" + account.email + ") has not yet been activated.  Please wait for an email confirmation from the Enrollment Administrator.  If you submitted your registration more than 24 hours ago, feel free to contact admin@5StarEnroll.com with any questions.  Thank you.")
                return redirect(url_for('login'))

        except StormpathError, err:
            if 'message' in err.message:
                flash(err.message['message'])
            else:
                flash(err.message)
                
    return render_template('user_account/login.html',
                           form = form,
                           nav_menu=get_nav_menu(),
    )

@app.route("/exit")
def taa_logout():
    """ Not sure how to hook into Stormpath /login URL in base.html template, so this is here just to be a handle for that
    """
    logout_user()
    session.pop('username', None)
    session.pop('headername', None)

    try:
        print "LOGOUT: ", user.email
    except:
        print "LOGOUT: <error on accessing user object>"
            
    return redirect('logout')
    