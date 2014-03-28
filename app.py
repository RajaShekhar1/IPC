import os, re, json
from flask import Flask, render_template, send_from_directory, url_for, flash, redirect
from flask_wtf import Form
from forms import LoginForm, UserEmailForm, UserDirectForm
from docu_embed import signing_sample
from docu_console import console_sample
from docu_email import emailing_sample

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
                           directSignURL = signing_sample('JoeBob Johnson', url_for('index')),
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
         
        if emailing_sample(form.full_name.data, form.email_addr.data, form.email_comments.data):
            flash(form.full_name.data + " (" + form.email_addr.data + " ) "
                  "will receive a link to the application via email.  The signed application will queue in your agent applications inbox requiring your signature prior to processing.")  

            return redirect('/demo')
    return render_template('emailSendRequest.html', 
                           form = form)


@app.route('/launch-direct', methods = ['GET', 'POST'])
def launchDirect():
    form = UserDirectForm()
    if form.validate_on_submit():
        return redirect ( signing_sample(form.full_name.data, url_for('index')))
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



@app.route("/test/")
def testpage():
    return render_template('test.html')


# launch
if __name__ == "__main__":
    port = int(os.environ.get("PORT", 5000))
    app.run(host='0.0.0.0', port=port)
