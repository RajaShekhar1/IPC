import os
import json
import sys
from logging import getLogger
from flask import (
    Flask,
    render_template,
    send_from_directory,
    url_for,
    flash,
    session,
    redirect,
    request,
    jsonify,
    abort,
)
from ConfigParser import ConfigParser

from datetime import timedelta
from forms import LoginForm, UserEmailForm, UserDirectForm
from model.DocuSign_config import sessionUserApprovedForDocusign

from docu_embed import signing_sample
from docusign_envelope import create_envelope_and_get_signing_url
from docu_console import console_url
from docu_email import emailing_sample

from model.Database import Database
from model.Enrollment import (
    EnrollmentEmail, 
    Case, 
    Enrollment, 
    AgentActivationEmail,
    NotifyAdminEmail,
)
from model.Product import get_age_from_birthday, get_product_by_code
from model.States import get_states
from model.Registration import (
    TAA_RegistrationForm,
    TAA_LoginForm,
    TAA_UserForm,
)
from flask.ext.stormpath import (
    StormpathError,
    StormpathManager,
    User,
    login_required,
    groups_required,
    login_user,
    logout_user,
    user,
)
from json import dumps
from stormpath.client import Client



# initialization
app = Flask(__name__)
app.config.from_object('config')

# Read in config file globally
config = ConfigParser(defaults={})
try:
    config.readfp(open('config.ini'))
except Exception as e:
    app.logger.critical("No config.ini found")
    raise e
    
if 'email' not in config.sections():
    raise Exception("No email section in config file")
    
if 'database' not in config.sections():
    raise Exception("No database section in config file")

if 'stormpath' not in config.sections():
    raise Exception("No stormpath section in config file")


def get_database():
    return Database(config.get('database', 'connection_string'))

# user management setup
#app.config['SECRET_KEY'] = config.get('stormpath', 'SECRET_KEY')
#app.config['STORMPATH_API_KEY_ID'] = config.get('stormpath', 'STORMPATH_API_KEY_ID')
#app.config['STORMPATH_API_KEY_SECRET'] = config.get('stormpath', 'STORMPATH_API_KEY_SECRET')
##app.config['STORMPATH_API_KEY_FILE'] = config.get('stormpath', 'STORMPATH_API_KEY_FILE')
#app.config['STORMPATH_APPLICATION'] = config.get('stormpath', 'STORMPATH_APPLICATION')

app.config['SECRET_KEY'] = 'george5starboat'
app.config['STORMPATH_API_KEY_ID'] = '5GPLR2SQXVPDJEXKXYE287ZYS'
app.config['STORMPATH_API_KEY_SECRET'] = 'wiZWfjnQu3qBSAYIbQskIn8CKJf/q0A8KxSdMN2NZn8'
app.config['STORMPATH_APPLICATION'] = 'TAA'

app.config['STORMPATH_COOKIE_DURATION'] = timedelta(minutes=15)
app.config['STORMPATH_LOGIN_URL'] = '/login'
app.config['STORMPATH_LOGIN_TEMPLATE'] = 'login.html'
app.config['STORMPATH_ENABLE_REGISTRATION'] = False
app.config['STORMPATH_ENABLE_LOGIN'] = False
stormpath_manager = StormpathManager(app)

"""
stormpathClient = Client(
    id = app.config['STORMPATH_API_KEY_ID'],
    secret = app.config['STORMPATH_API_KEY_SECRET'],
)

stormpathApp = stormpathClient.applications.search('TAA')[0]
"""


"""--------------------------------------------------------------
HOME and util pages
"""

@app.route('/favicon.ico')
def favicon():
    return send_from_directory(os.path.join(app.root_path, 'static'), 'ico/favicon.ico')

@app.errorhandler(404)
def page_not_found(e):
    return render_template('404.html'), 404

@app.route("/")
def index():

    if user and not user.is_anonymous():
        return redirect(url_for('home'))
        #return render_template('index.html')
        
    return redirect(url_for('login'))

@app.route("/home")
@login_required
def home():
    print session.keys()
    print session
    return render_template('home.html')

@app.route("/robots.txt")
def robots():
    return send_from_directory(os.path.join(app.root_path, 'static'), 'robots.txt')


"""--------------------------------------------------------------
ENROLLMENT pages and handling
"""

@app.route("/get_rates", methods=['POST'])
def rates():
    
    # Pull parameters from the request
    employee_birthdate = request.form['employee_birthdate']
    spouse_birthdate = request.form.get('spouse_birthdate', None)
    num_children = int(request.form.get('num_children', 0))
    product_code = request.form['product_type']
    
    product = get_product_by_code(product_code)
    employee_age = get_age_from_birthday(employee_birthdate)
    spouse_age = get_age_from_birthday(spouse_birthdate) if spouse_birthdate else None
    
    response = {
        'employee_rates': product.get_employee_rates(employee_age),
        'spouse_rates': product.get_spouse_rates(spouse_age),
        'children_rates': product.get_children_rates(num_children),
        'recommendations': product.get_recommended_coverages(employee_age, spouse_age, num_children),
    }
    
    return jsonify(**response)

@app.route("/enroll")
@login_required
def enroll_start():
    return render_template('setup-enrollment.html')

@app.route("/in-person-enrollment", methods=['POST'])
@login_required
def in_person_enrollment():
    state = request.form['enrollmentState']
    company_name = request.form['companyName']
    product_code = request.form['productID']
    employee_first = request.form['eeFName']
    employee_last = request.form['eeLName']
    employee_email = request.form['email']
    
    product = get_product_by_code(product_code)
    
    wizard_data = {
        'state': state if state != 'XX' else None,
        'company_name': company_name,
        'product_id':product_code,
        'employee_first':employee_first,
        'employee_last':employee_last,
        'employee_email':employee_email,
        'is_in_person':True,
        'health_questions':product.get_health_questions(),
    }
    
    return render_template(
        'main-wizard.html', 
        wizard_data=wizard_data,
        states=get_states()
    )

@app.route("/email-enrollment", methods=['POST'])
@login_required
def email_enrollment():
    enrollment_state = request.form['enrollmentState']
    company_name = request.form['companyName']
    product_code = request.form['productID']
    employee_first = request.form['eeFName']
    employee_last = request.form['eeLName']
    employee_email = request.form['email']

    db = get_database()
    
    # product = get_product_by_code(product_code)
    product = db.get_product_by_code(product_code)
    
    # May not want to create a case for each time this is called 
    case = Case(
        id=None,
        company_name=company_name,
        situs_state=enrollment_state,
        product=product,
    )
    db.save_case(case)
    
    enrollment = Enrollment(
        id=None,
        case=case,
        employee_first=employee_first,
        employee_last=employee_last,
        employee_email=employee_email,
    )
    db.save_enrollment(enrollment)

    enrollment_request = enrollment.generate_enrollment_request()
    db.save_enrollment_request(enrollment_request)
    
    email_config = dict(
        smtp_server=config.get('email', 'smtp_server'),
        smtp_port=config.get('email', 'smtp_port'),
        smtp_user=config.get('email', 'smtp_username'),
        smtp_password=config.get('email', 'smtp_password'),
        from_address=config.get('email', 'from_address'),
    )

    EnrollmentEmail(**email_config).send_enrollment_request(enrollment_request)
    
    return jsonify(**dict(success=True))

@app.route("/enrollment_request", methods=['GET'])
def email_link_handler():
    """
    Handles someone clicking the link in the enrollment request email
    """
    
    token = request.args['token']
    db = get_database()
    
    enrollment_request = db.get_enrollment_request_by_token(token)
    
    # TODO
    #if enrollment_request.is_expired():
    #    return render_template('token_expired.html')
    
    enrollment = enrollment_request.enrollment
    case = enrollment.case
    # 2014-06-05 product.get_health_questions() blowing up below - not yet handled in DB?
    # product = case.product
    product = get_product_by_code(case.product.code)

    wizard_data = {
        'state': case.situs_state,
        'company_name': case.company_name,
        'product_id': case.product.code,
        'employee_first': enrollment.employee_first,
        'employee_last': enrollment.employee_last,
        'employee_email': enrollment.employee_email,
        'is_in_person':False,
        'health_questions': product.get_health_questions(),
    }

    return render_template('main-wizard.html',
                           wizard_data=wizard_data,
                           states=get_states(),
    )

    
"""--------------------------------------------------------------
AGENT pages
"""

@app.route("/inbox", methods =['GET'])
@login_required
def inbox():
#    return redirect( console_url())
    if sessionUserApprovedForDocusign():
        return render_template('agent-inbox.html',
                               inboxURL = console_url())
    else:
        flash("You are not yet authorized for signing applications.  Please see your Regional Director for assistance.")
        return redirect(url_for("home"))



"""--------------------------------------------------------------
DOCUSIGN interaction
"""

@app.route("/submit-wizard-data", methods=['POST'])
def submit_wizard_data():
    
    data = json.loads(request.data)
    
    wizard_results = data['wizard_results']
    print "--------------------"
    print wizard_results
    print "--------------------"
    sys.stdout.flush()
    
    
    # Do docusign with data in wizard_results
    #
    is_error, error_message, redirect = create_envelope_and_get_signing_url(wizard_results);
    
    # Return the redirect url or error
    resp = {'error': is_error, 'error_message': error_message, "redirect": redirect}
    return jsonify(**resp)
    

@app.route("/application_completed", methods=['GET'])
def ds_landing_page():
    """
    Handles simple responses to completing the enrollment page
    """

    session_type = request.args['type']
    name = request.args['name']
    ds_event = request.args['event']

    return render_template('completed-session.html',
                           session_type=session_type,
                           name=name,
                           ds_event=ds_event)


    
"""
--------------------------------------------------------------
Login & Registration from Stormpath
"""

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
                    NotifyAdminEmail().send_registration_notice(data['given_name'] + " " +  data['surname'])
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

            print "LOGIN: %s %s, (%s  %s)" % (account.given_name, account.surname, account.email, account.custom_data['activated'])
            is_admin =  account.email=="admin@5starenroll.com"
            
            if account.custom_data['activated'] or is_admin:
                login_user(account, remember=True) 
                session['username'] = user.given_name + " " + user.surname
                agencyStr = user.custom_data['agency']
                if agencyStr.strip() != "": 
                    session['headername'] = session['username'] + ", " + user.custom_data['agency']
                else:
                    session['headername'] = session['username']

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
    
"""
--------------------------------------------------------------
ADMIN pages
"""

#  14-Jun-17 WSD 
@app.route('/admin', methods = ['GET', 'POST'])
@groups_required(['admins'])
def admin():
    accounts = []
    for acc in stormpath_manager.application.accounts:
        accounts.append(
            {'fname': acc.given_name,
             'lname': acc.surname,
             'email': acc.email,
             'agency': acc.custom_data['agency'],
             'agent_code': acc.custom_data['agent_code'],
             'signing_name': acc.custom_data['signing_name'],
             'status': "Activated" if acc.custom_data['activated'] else "Not Activated"
         })
        #print dumps(dict(acc.custom_data), indent=2, sort_keys=True)
        
    #show the un-activated accounts first
    accounts = sorted(accounts, reverse=True, key=(lambda x: x['status']))
    return render_template('admin.html', accounts=accounts)


@app.route('/edituser', methods = ['GET', 'POST'])
@groups_required(['admins'])
def updateUser():

    user_email = request.args['user']
    form = TAA_UserForm()
    
    # initially pre-populate the form with account values
    if request.method == 'GET':
        accounts = stormpath_manager.application.accounts.query(email=user_email)
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
            accounts = stormpath_manager.application.accounts.query(email=user_email)
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

                # if we've just activated a user, then send a notice
                if data['activated'] and data['send_notice']:                    
                    email_config = dict(
                        smtp_server=config.get('email', 'smtp_server'),
                        smtp_port=config.get('email', 'smtp_port'),
                        smtp_user=config.get('email', 'smtp_username'),
                        smtp_password=config.get('email', 'smtp_password'),
                        from_address=config.get('email', 'from_address'),
                    )
                    
                    try:
                        AgentActivationEmail(**email_config).send_activation_notice(data['email'], data['fname'], url_for('home'))
                        flash('Activation email sent.')
                    except:
                        flash('>> Problem sending activation email <<')
                    


            return redirect(url_for('admin'))
        except StormpathError, err:
            flash(err.user_message)

    return render_template('update-user.html',
                           form = form,
    )


"""--------------------------------------------------------------
DEMO and testing pages
"""


"""@app.route('/send-app', methods = ['GET', 'POST'])
def sendApp():
    form = UserEmailForm()
    if form.validate_on_submit():
        # print("name: %s\nemail: %s\n" % (form.full_name, form.email_addr))
        # print("s_name: [%s]\n s_email: [%s]\n" % (str(form.full_name), str(form.email_addr)))
         
        if emailing_sample(form.full_name.data, form.employer.data, form.email_addr.data, form.email_comments.data):
            flash(form.full_name.data + " (" + form.email_addr.data + " ) "
                  "will receive a link to the application via email.  The signed application will queue in your agent applications inbox requiring your signature prior to processing.")  

            return redirect('/demo')
    return render_template('emailSendRequest.html', 
                           form = form)
"""

@app.route("/test")
#  14-Apr-22 WSD modified to new test file
#  14-May- 7 WSD change to yes_no test page
#  14-Jun-13 WSD 'decommission' by making 404
def testpage():
    return render_template('404.html'), 404




    

# launch
if __name__ == "__main__":
    port = int(os.environ.get("PORT", 5000))
    app.run(host='0.0.0.0', port=port)


