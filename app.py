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
    redirect,
    request,
    jsonify,
    abort,
)
from ConfigParser import ConfigParser

from datetime import timedelta
from forms import LoginForm, UserEmailForm, UserDirectForm
from docu_embed import signing_sample
from docusign_envelope import create_envelope_and_get_signing_url
from docu_console import console_sample
from docu_email import emailing_sample

from model.Database import Database
from model.Enrollment import EnrollmentEmail, Case, Enrollment
from model.Product import get_age_from_birthday, get_product_by_code
from model.States import get_states
from model.Registration import TAA_RegistrationForm
from flask.ext.stormpath import (
    StormpathError,
    StormpathManager,
    User,
    login_required,
    login_user,
    logout_user,
    user,
)



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

app.config['STORMPATH_COOKIE_DURATION'] = timedelta(minutes=10)
app.config['STORMPATH_LOGIN_URL'] = '/start'
app.config['STORMPATH_LOGIN_TEMPLATE'] = 'login.html'
app.config['STORMPATH_ENABLE_REGISTRATION'] = False
stormpath_manager = StormpathManager(app)


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
    return render_template('index.html')

@app.route("/home")
@login_required
def home():
    return render_template('home.html')


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
#    return redirect( console_sample())
    return render_template('agent-inbox.html',
                           inboxURL = console_sample())


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

#@app.route("/taa_register", methods=['GET', 'POST'])
def register_original():
    """
    Register a new user with Stormpath.

    This view will render a registration template, and attempt to create a new
    user account with Stormpath.

    The fields that are asked for, the URL this view is bound to, and the
    template that is used to render this page can all be controlled via
    Flask-Stormpath settings.
    """
    form = TAA_RegistrationForm()

    # If we received a POST request with valid information, we'll continue
    # processing.
    if form.validate_on_submit():
        fail = False

        # Iterate through all fields, grabbing the necessary form data and
        # flashing error messages if required.
        data = form.data
        for field in data.keys():
            if app.config['STORMPATH_ENABLE_%s' % field.upper()]:
                if app.config['STORMPATH_REQUIRE_%s' % field.upper()] and not data[field]:
                    fail = True

                    # Manually override the terms for first / last name to make
                    # errors more user friendly.
                    if field == 'given_name':
                        field = 'first name'

                    elif field == 'surname':
                        field = 'last name'

                    flash('%s is required.' % field.replace('_', ' ').title())

        # If there are no missing fields (per our settings), continue.
        if not fail:

            # Attempt to create the user's account on Stormpath.
            try:

                # Since Stormpath requires both the given_name and surname
                # fields be set, we'll just set the both to 'Anonymous' if
                # the user has # explicitly said they don't want to collect
                # those fields.
                data['given_name'] = data['given_name'] or 'Anonymous'
                data['surname'] = data['given_name'] or 'Anonymous'

                # Create the user account on Stormpath.  If this fails, an
                # exception will be raised.
                account = User.create(**data)

                # If we're able to successfully create the user's account,
                # we'll log the user in (creating a secure session using
                # Flask-Login), then redirect the user to the
                # STORMPATH_REDIRECT_URL setting.
                login_user(account, remember=True)

                return redirect(app.config['STORMPATH_REDIRECT_URL'])
            except StormpathError, err:
                flash(err.user_message)

    return render_template(app.config['STORMPATH_REGISTRATION_TEMPLATE'],
                           form = form,
    )

@app.route("/taa_register", methods=['GET', 'POST'])
def register_taa():
    """
    Register a new user with Stormpath, adding TAA customizations.
    Original of this function can be found in
    https://github.com/stormpath/stormpath-flask/blob/master/flask_stormpath/views.py#L21

    """
    print "---- Before form stuff ---"
    form = TAA_RegistrationForm()

    data = form.data
    for field in data.keys():
        if data[field]:
            val = data[field]
        else:
            val = "Null"
        if field:
            print 'field ' + field +' is upper=' + field.upper() + ', title=' + field.title() + ', and data=' + val
            sys.stdout.flush()
    print "------ DONE ------"
    sys.stdout.flush()

    # If we received a POST request with valid information, we'll continue
    # processing.
    if form.validate_on_submit():
        print "****** I never see this ******"
        sys.stdout.flush()
        
        fail = False

        # Iterate through all fields, grabbing the necessary form data and
        # flashing error messages if required.
        print "--------- <<Inside Validate>> -----------"
        sys.stdout.flush()
            
    
        data = form.data
        for field in data.keys():
            print 'field ' + field + ' is upper=' + field.upper() + ', title=' + field.title() + ', and data=' + data[field]
            sys.stdout.flush()
    
       
            """
            if app.config['STORMPATH_ENABLE_%s' % field.upper()]:
                if app.config['STORMPATH_REQUIRE_%s' % field.upper()] and not data[field]:
                    fail = True

                    # Manually override the terms for first / last name to make
                    # errors more user friendly.
                    if field == 'given_name':
                        field = 'first name'

                    elif field == 'surname':
                        field = 'last name'

                    flash('%s is required.' % field.replace('_', ' ').title())
            """

        # If there are no missing fields (per our settings), continue.
        if not fail:

            # Attempt to create the user's account on Stormpath.
            try:

                # Since Stormpath requires both the given_name and surname
                # fields be set, we'll just set the both to 'Anonymous' if
                # the user has # explicitly said they don't want to collect
                # those fields.
                data['custom_data'] = data['given_name'] or 'Anonymous'
                data['surname'] = data['given_name'] or 'Anonymous'

                # Create the user account on Stormpath.  If this fails, an
                # exception will be raised.
                account = User.create(**data)

                # If we're able to successfully create the user's account,
                # we'll log the user in (creating a secure session using
                # Flask-Login), then redirect the user to the
                # STORMPATH_REDIRECT_URL setting.
                login_user(account, remember=True)

                return redirect(app.config['STORMPATH_REDIRECT_URL'])
            except StormpathError, err:
                flash(err.user_message)

    return render_template('register-orig.html', 
                           form = form
            )

@app.route("/registration_confirmed")
def confirmRegistration():
    return render_template('registration_complete.html')
app.config['STORMPATH_REDIRECT_URL'] = '/registration_confirmed'


@app.route('/login2', methods = ['GET', 'POST'])
def login():
    form = LoginForm()
    if form.validate_on_submit():
        flash('Login requested for OpenID="' + form.openid.data + '", remember_me=' + str(form.remember_me.data))
        return redirect('/demo')
    return render_template('login.html', 
                           title = 'Sign In',
                           form = form,
                           providers = app.config['OPENID_PROVIDERS'])


"""--------------------------------------------------------------
DEMO and testing pages
"""
@app.route("/demo")
def sample():
    return render_template('sample.html',
                           directSignURL = url_for ('launchDirect'),
                           recipName = 'JoeBob Johnson')


@app.route('/send-app', methods = ['GET', 'POST'])
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


@app.route('/launch-direct', methods = ['GET', 'POST'])
def launchDirect():
    
    form = UserDirectForm()
    if form.validate_on_submit():
        return redirect ( signing_sample(form.full_name.data, form.employer.data, form.email_addr.data, url_for('index')))
    return render_template('inPersonRequest.html', 
                           form = form)


@app.route('/in-person-signing', methods = ['GET', 'POST'])
def inPersonSign():
    form = UserDirectForm()
    if form.validate_on_submit():
        return redirect ( create_envelope_and_get_signing_url(form.full_name.data, form.employer.data, form.email_addr.data, url_for('index')))
    return render_template('inPersonRequest.html', 
                           form = form)

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


