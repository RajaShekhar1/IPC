
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
    user,
)

from taa import app
from taa.model.Registration import TAA_RegistrationForm, TAA_LoginForm
from taa.model.Enrollment import NotifyAdminEmail

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

                # If successfully created account, the notify admin of registration
                try:
                    NotifyAdminEmail().send_registration_notice(data['given_name'] + " " + data['surname'])
                except:
                    print " -- Problem sending registration notice to admin --"


                # If we're able to successfully create the user's account,
                # we'll log the user in (creating a secure session using
                # Flask-Login), then redirect the user to the
                # STORMPATH_REDIRECT_URL setting.
                #login_user(account, remember=True)

                session['registered_name'] = data['given_name']

                return redirect(url_for('confirmRegistration'))
            except StormpathError, err:
                flash(err.user_message)

    return render_template('register.html', 
                           form = form
            )

@app.route("/registration_confirmed")
def confirmRegistration():
    return render_template('registration_complete.html',
                           name = session['registered_name'])
#app.config['STORMPATH_REDIRECT_URL'] = '/registration_confirmed'


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

            print "LOGIN: %s %s, (%s  activated=%s)" % (account.given_name, account.surname, account.email, account.custom_data['activated'])
            is_admin =  account.email=="admin@5starenroll.com"
            
            if account.custom_data['activated'] or is_admin:
                login_user(account, remember=True) 
                session['username'] = user.given_name + " " + user.surname
                session['headername'] = session['username']

                if user.custom_data['agency'].strip() != "": 
                    session['headername'] += ", " + user.custom_data['agency']

                session['active_case'] = {
                    'company_name': "",
                    'situs_state': "",
                    'situs_city': "",
                    'product_code': ""
                }
                
                if is_admin:
                    return redirect(url_for('admin'))
                else:
                    return redirect(request.args.get('next') or url_for('home'))
            else:
                flash(account.given_name + ", your account (" + account.email + ") has not yet been activated.  Please wait for an email confirmation from the Enrollment Administrator.  If you submitted your registration more than 24 hours ago, feel free to contact admin@5StarEnroll.com with any questions.  Thank you.")
                return redirect(url_for('login'))

        except StormpathError, err:
            flash(err.user_message)

    return render_template('login.html',
                           form = form,
    )

@app.route("/exit")
def taa_logout():
    """ Not sure how to hook into Stormpath /login URL in base.html template, so this is here just to be a handle for that
    """
    
    session.pop('username', None)
    session.pop('headername', None)

    try:
        print "LOGOUT: ", user.email
    except:
        print "LOGOUT: <error on accessing user object>"
            
    return redirect('logout')
    