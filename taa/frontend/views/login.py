import traceback

from flask import (
    flash,
    jsonify,
    redirect,
    render_template,
    request,
    session,
    url_for,
    abort)
from flask_login import (
    login_user,
    logout_user,
    current_user, login_required)


from taa import app, db
from taa.errors import email_exception
from .nav import get_nav_menu
from taa.old_model.Registration import TAA_RegistrationForm, TAA_LoginForm
from taa.old_model.Enrollment import NotifyAdminEmail
from taa.services.agents import AgentService, OktaService
from taa.services.users import UserService
from taa.services import RequiredFeature, LookupService


apology_message = "There is a temporary problem with the login process. We apologize for the inconvenience. Please try again in a few minutes."


@app.route("/user_register", methods=['GET', 'POST'])
def register_taa():
    """
    Register a new user with Stormpath, adding TAA customizations.
    Original of this function can be found in
    https://github.com/stormpath/stormpath-flask/blob/master/flask_stormpath/views.py#L21

    """
    agent_service = LookupService("AgentService")
    form = TAA_RegistrationForm()

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
                okta_service = OktaService()
                if okta_service.get_user_by_email(data['email']):
                    flash("This email is already associated with an account in this organization.")
                    return render_template('user_account/register.html', form=form, nav_menu=get_nav_menu())
                
                okta_user_data = okta_service.create_user(dict(
                    profile=dict(
                        firstName=data['given_name'],
                        lastName=data['surname'],
                        #middle_name=data['middle_name'],
                        email=data['email'],
                        login=data['email'],
                        agent_code=data['agent_code'],
                        agency=data['agency'],
                        signing_name=data['signing_name'],
                    ),
                    credentials=dict(
                        password=dict(value=data['password'])
                    )
                ))

                if not okta_user_data:
                    flash(okta_service.last_error_message)
                else:
                # Add to the agents group
                    agent = agent_service.create_user(
                        email = data['email'],
                        password = data['password'],
                        given_name = data['given_name'] or 'Anonymous',
                        middle_name = data.get('middle_name'),
                        surname = data['surname'] or 'Anonymous',
                        signing_name=data['signing_name'],
                        agent_code=data['agent_code'].upper(),
                        agency=data['agency'],
                        activated=False,
                        okta_id = okta_user_data['id'],
                    )

                    # Add to the agents group
                    agent.add_group("agents")
                    OktaService().set_user_groups(agent, ['agents'])

                    NotifyAdminEmail().send_registration_notice(data['given_name'] + " " + data['surname'])

                    session['registered_name'] = data['given_name']

                    db.session.commit()

                    return redirect(url_for('confirmRegistration'))
            except Exception as err:
                #if hasattr(err, 'message'):
                #    flash(err.message)
                #else:
                flash('There was a problem creating the user.')
                from tasks import send_admin_error_email
                send_admin_error_email("User registration error:<br>{}".format(err.message), [traceback.format_exc()])

    return render_template('user_account/register.html', form=form, nav_menu=get_nav_menu())


@app.route("/registration_confirmed")
def confirmRegistration():
    return render_template('user_account/registration_complete.html',
                           name = session.get('registered_name', ''),
                           nav_menu=get_nav_menu(),
                           )


@app.route("/forgot")
def forgot():
    return render_template('user_account/forgot.html')


@app.route("/reset_password_email", methods=['POST'])
def reset_password():
    # If the email exists in the system, we initiate the reset password workflow.
    email = request.json['email']
    okta_service = OktaService()
    result = okta_service.reset_password(email)
    if result:
        return jsonify(dict(success=True))
    else:
        return jsonify(dict(success=False, error=okta_service.last_error_message))


@app.route('/login', methods = ['GET', 'POST'])
def login():
    print("////////NEW REQUEST//////////")
    agent_service = LookupService("AgentService")
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
            account = agent_service.user_from_login(form.login.data, form.password.data)
            if not account:
                flash("Invalid username or password")
                return redirect(url_for('login', next=request.args.get('next')))

            # If we're able to successfully retrieve the user's account,
            # we'll log the user in (creating a secure session using
            # Flask-Login), then redirect the user to the ?next=<url>
            # query parameter, or just the HOME page
            print(u"LOGIN: %s %s, (%s  activated=%s)" % (account.first, account.last, account.email, account.activated))
            account_groups = [g.group for g in account.groups]
            is_agent = agent_service.is_user_agent(account)
            is_home_office = agent_service.is_user_home_office(account)
            is_admin = agent_service.is_user_admin(account)
            is_third_party_enroller = agent_service.is_user_third_party_enroller(account)
            print(u"Is ADMIN: %s, GROUPS: %s"%(is_admin, account_groups))
            if(is_admin or is_home_office or
                   (is_agent and account.activated) or
                   is_third_party_enroller):
                do_login(account)

                if is_admin:
                    return redirect(request.args.get('next') or url_for('admin'))
                elif is_agent:
                    return redirect(request.args.get('next') or url_for('home'))
                elif is_third_party_enroller:
                    return redirect(request.args.get('next') or url_for('home'))
                else:
                    # Home office
                    return redirect(request.args.get('next') or url_for('home_office_dashboard'))
            else:
                flash(u"{name}, your account ({email}) has not yet been activated. "
                      "Please wait for an email confirmation from the Enrollment Administrator. "
                      "If you submitted your registration more than 24 hours ago, "
                      "feel free to contact {admin_email} with any questions. Thank you.".format(
                        name=account.first, email=account.email,
                        admin_email=app.config['ADMIN_EMAIL']))
                return redirect(url_for('login'))

        except Exception, err:
            flash(apology_message, "error")
            email_exception(app, err)

    return render_template('user_account/login.html',
                           form = form,
                           nav_menu=get_nav_menu(),
    )


def do_login(account):
    agent_service = LookupService("AgentService")
    is_agent = agent_service.is_user_agent(account)
    is_home_office = agent_service.is_user_home_office(account)
    is_admin = agent_service.is_user_admin(account)

    login_user(account, remember=True, force=True)
    session['username'] = current_user.first + " " + current_user.last
    session['headername'] = session['username']

    if is_agent and current_user.agency and current_user.agency.strip():
        session['headername'] += ", " + current_user.agency
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

#
#
# @app.route('/reauth', methods = ['POST'])
# def reauth():
#     user_service = LookupService("UserService")
#
#     is_error = False
#     error_message = None
#
#     data = request.json
#     account_href = data.get('account_href')
#     password = data.get('password')
#     session_data = data.get('session_data')
#     success_message = data.get('success_message')
#
#     try:
#         if account_href is not None:
#             user_from_href = user_service.get_stormpath_user_by_href(account_href)
#             if user_from_href is not None:
#                 account = user_service.user_from_login(user_from_href.email, password)
#                 do_login(account)
#             else:
#                 is_error = True
#                 error_message = "Could not login."
#
#         if is_error is False:
#             # Set optional session values
#             session['is_self_enroll'] = session_data.get('is_self_enroll')
#             session['active_case_id'] = session_data.get('active_case_id')
#             session['enrolling_census_record_id'] = session_data.get('enrolling_census_record_id')
#
#     except Error, err:
#         is_error = True
#         error_message = err.message
#
#     resp = {
#         'error': is_error,
#         'message': error_message or success_message,
#     }
#
#     data = jsonify(**resp)
#
#     return data


@app.route("/exit")
def taa_logout():
    """ Not sure how to hook into Stormpath /login URL in base.html template, so this is here just to be a handle for that
    """

    try:
        if not current_user.is_anonymous():
            print "LOGOUT: ", current_user.email
    except:
        print "LOGOUT: <error on accessing user object>"

    logout_user()
    session.pop('username', None)
    session.pop('headername', None)

    return redirect('logout')

@app.route('/logout')
def logout2():
    if not current_user.is_anonymous():
        print "LOGOUT: ", current_user.email
    logout_user()
    session.pop('username', None)
    session.pop('headername', None)

    return redirect('login')