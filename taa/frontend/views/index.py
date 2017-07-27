import os

from flask import render_template, url_for, send_from_directory, redirect
from flask_login import login_required, current_user

from taa import app
from .nav import get_nav_menu
from taa.services.agents import AgentService
from taa.services import LookupService

agent_service = AgentService()

"""--------------------------------------------------------------
HOME and util pages
"""


@app.route('/favicon.ico')
def favicon():
    return send_from_directory(os.path.join(app.root_path, 'static'), 'ico/favicon.ico')


@app.errorhandler(404)
def page_not_found(e):
    return render_template('404.html', include_navmenu=get_nav_menu()), 404


@app.route("/")
def index():

    if current_user:
        return redirect(url_for('home'))

    return redirect(url_for('login'))


@app.route("/home")
@login_required
def home():
    api_token_service = LookupService("ApiTokenService")
    if not current_user or current_user.is_anonymous:
        return redirect(url_for('login'))

    if agent_service.is_user_admin(current_user) or agent_service.is_user_home_office(current_user):
        return render_template('home_office/dashboard.html', nav_menu=get_nav_menu())
    elif agent_service.is_user_agent(current_user):
        return render_template('home.html', nav_menu=get_nav_menu())
    elif agent_service.is_user_third_party_enroller(current_user):
        vars = {}
        vars["current_user_token"] = api_token_service.get_token_by_sp_href(current_user.okta_id)
        vars["nav_menu"] = get_nav_menu()
        return render_template('enrollment/enrollment_importer.html', **vars)
    else:
        raise Exception('unknown type for user "%s"'%current_user)


@app.route("/robots.txt")
def robots():
    return send_from_directory(
            os.path.join(app.root_path, 'frontend', 'static'), 'robots.txt')


@app.route("/ping_check.txt")
def pingcheck():
    if app.config['IS_AFBA']:
        filename = 'healthcheck-afba.txt'
    else:
        filename = 'healthcheck.txt'
    return send_from_directory(
            os.path.join(app.root_path, 'frontend', 'static'), filename)
