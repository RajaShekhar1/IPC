import os
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

from forms import LoginForm, UserEmailForm, UserDirectForm
from docu_embed import signing_sample
from docusign_envelope import create_envelope_and_get_signing_url
from docu_console import console_sample
from docu_email import emailing_sample

from model.Database import Database
from model.Enrollment import EnrollmentEmail
from model.RateTable import (
    get_product,
    get_age_from_birthday,
)
from model.States import get_states

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

""" the init used to be this
app.config.update(
    DEBUG = True,
)
"""

# controllers
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
def home():
    return render_template('home.html')

@app.route("/enroll")
def enroll_start():
    return render_template('setup-enrollment.html')

@app.route("/in-person-enrollment", methods=['POST'])
def in_person_enrollment():
    state = request.form['enrollmentState']
    company_name = request.form['companyName']
    product_id = request.form['productID']
    employee_first = request.form['eeFName']
    employee_last = request.form['eeLName']
    employee_email = request.form['email']
    
    wizard_data = {
        'state': state if state != 'XX' else None,
        'company_name': company_name,
        'product_id':product_id,
        'employee_first':employee_first,
        'employee_last':employee_last,
        'employee_email':employee_email,
    }
    
    return render_template('main-wizard.html', 
                wizard_data=wizard_data,
                states=get_states(),
           )

@app.route("/email-enrollment", methods=['POST'])
def email_enrollment():
    return None
    
@app.route("/demo")
def sample():
    return render_template('sample.html',
                           directSignURL = url_for ('launchDirect'),
                           recipName = 'JoeBob Johnson')

@app.route ("/email_confirmed_test.html")
def confirmEmail():
    return render_template('email_confirmed.html',
                           who = 'Johnny Worker',
                           email = 'employee@thumbprintcpm.com',
                           success = emailing_sample("Johnny Employee","employee@thumbprintcpm.com"))


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

@app.route("/inbox")
def inbox():
    return redirect( console_sample())

@app.route('/login', methods = ['GET', 'POST'])
def login():
    form = LoginForm()
    if form.validate_on_submit():
        flash('Login requested for OpenID="' + form.openid.data + '", remember_me=' + str(form.remember_me.data))
        return redirect('/demo')
    return render_template('login.html', 
                           title = 'Sign In',
                           form = form,
                           providers = app.config['OPENID_PROVIDERS'])


@app.route("/test")
#  14-Apr-22 WSD modified to new test file
def testpage():
    return render_template(
        'main-wizard.html',
        wizard_data={}, 
        states=get_states(),
    )

@app.route("/submit-wizard-data", methods=['POST'])
def submit_wizard_data():
    wizard_results = request.form['wizard_results']
    
    # is_error = False
    # error_message = "Implement docusign"
    # redirect = url_for('enroll_start')
    # --------
    # Do docusign with data in wizard_results
    #
    is_error, error_message, redirect = create_envelope_and_get_signing_url(wizard_results);
    
    # Return the redirect url or error
    resp = {'error': is_error, 'error_message': error_message, "redirect": redirect}
    return jsonify(**resp)
    

@app.route("/get_rates", methods=['POST'])
def rates():
    # validate required params
    #for required_param in ['gender, age_band', 'marital_status', 'include_spouse', 'num_children']:
    #    if required_param not in request.form:
    #        abort(400)
    
    # Pull parameters from the request
    employee_birthdate = request.form['employee_birthdate']
    spouse_birthdate = request.form.get('spouse_birthdate', None)
    num_children = int(request.form.get('num_children', 0))
    product_type = request.form['product_type']
    
    product = get_product(product_type)
    employee_age = get_age_from_birthday(employee_birthdate)
    spouse_age = get_age_from_birthday(spouse_birthdate) if spouse_birthdate else None
    
    response = {
        'employee_rates': product.get_employee_rates(employee_age),
        'spouse_rates': product.get_spouse_rates(spouse_age),
        'children_rates': product.get_children_rates(num_children),
        'recommendations': product.get_recommended_coverages(employee_age, spouse_age, num_children),
    }
    
    #import pprint
    #app.logger.debug(pprint.pformat(response))
    
    return jsonify(**response)
    

@app.route("/send_enrollment_example", methods=['GET','POST'])
def send_enrollment_email():
    
    enrollment_id = request.args['enrollment_id']
    
    db = Database(config.get('database', 'connection_string'))
    enrollment = db.retrieve_enrollment(enrollment_id)
    if not enrollment:
        abort(400, "Invalid Enrollment ID")
    
    email_config = dict(
        smtp_server=config.get('email', 'smtp_server'), 
        smtp_port=config.get('email', 'smtp_port'), 
        smtp_user=config.get('email', 'smtp_user'), 
        smtp_password=config.get('email', 'smtp_password'), 
        from_address=config.get('email', 'from_address'),
    )
    
    EnrollmentEmail(**email_config).send_enrollment_request(enrollment)

@app.route("/enrollment_request", methods=['GET'])
def handle_email_link():
    token = request.args['token']
    db = Database()
    

# launch
if __name__ == "__main__":
    port = int(os.environ.get("PORT", 5000))
    app.run(host='0.0.0.0', port=port)


