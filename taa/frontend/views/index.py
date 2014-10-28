import os

from flask import render_template, url_for, send_from_directory, redirect
from flask.ext.stormpath import login_required, user

from taa import app


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
        
    return redirect(url_for('login'))

@app.route("/home")
@login_required
def home():
    return render_template('home.html')

@app.route("/robots.txt")
def robots():
    return send_from_directory(os.path.join(app.root_path, 'frontend', 'static'), 'robots.txt')

@app.route("/ping_check.txt")
def pingcheck():
    return send_from_directory(os.path.join(app.root_path, 'frontend', 'static'), 'healthcheck.txt')

