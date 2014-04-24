import os, re, json
from flask import (
    Flask, 
    render_template, 
    send_from_directory, 
    url_for, 
    flash, 
    redirect, 
    request, 
    abort,
    jsonify,
)
from flask_wtf import Form
from forms import LoginForm, UserEmailForm, UserDirectForm
from docu_embed import signing_sample
from docu_console import console_sample
from docu_email import emailing_sample

from model.RateTable import (
    get_age_from_birthday,
    get_weekly_rates_by_coverage,
    get_coverages_by_weekly_rate,
    get_child_rates,
)

# initialization
app = Flask(__name__)
app.config.from_object('config')


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
    return render_template('main-wizard.html')


@app.route("/get_rates", methods=['POST'])
def rates():
    # validate required params
    #for required_param in ['gender, age_band', 'marital_status', 'include_spouse', 'num_children']:
    #    if required_param not in request.form:
    #        abort(400)
    
    employee_birthdate = request.form['employee_birthdate']
    spouse_birthdate = request.form.get('spouse_birthdate', None)
    num_children = int(request.form.get('num_children', 0))
    
    emp_age = get_age_from_birthday(employee_birthdate)
    sp_age = get_age_from_birthday(spouse_birthdate) if spouse_birthdate else None
    
    employee_rates = {
        'weekly_coverages': get_coverages_by_weekly_rate(emp_age),
        'weekly_rates': get_weekly_rates_by_coverage(emp_age),
    }
    spouse_rates = {}
    if sp_age:
        spouse_rates = {
            'weekly_coverages': get_coverages_by_weekly_rate(sp_age),
            'weekly_rates': get_weekly_rates_by_coverage(sp_age),
        }

    children_rates = {}
    if num_children > 0:
        children_rates = {
            get_child_rates()
        }
    response = {
        'employee_rates': employee_rates,
        'spouse_rates': spouse_rates,
        'children_rates': children_rates,
    }
    
    return jsonify(**response)
    

# launch
if __name__ == "__main__":
    port = int(os.environ.get("PORT", 5000))
    app.run(host='0.0.0.0', port=port)


