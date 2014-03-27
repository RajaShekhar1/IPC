import os
from flask import Flask, render_template, send_from_directory, url_for, flash, redirect
from flask_wtf import Form
from forms import LoginForm
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

@app.route ("/email_confirmed.html")
def confirmEmail():
    return render_template('email_confirmed.html',
                           who = 'Johnny Worker',
                           email = 'employee@thumbprintcpm.com',
                           success = emailing_sample("Johnny Employee","employee@thumbprintcpm.com"))


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
