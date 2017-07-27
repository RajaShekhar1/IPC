import os
import locale

# Make sure this is set for the whole app for formatting dates, times, currency, etc.
from flask_login import LoginManager, login_required
from functools import wraps
locale.setlocale(locale.LC_ALL, 'en_US.UTF-8')

from flask import Flask, abort
from flask_sslify import SSLify
from flask_sqlalchemy import SQLAlchemy
from flask_compress import Compress

from .helpers import JSONEncoder
from okta import UsersClient
from okta import AuthClient
from okta import UserGroupsClient
from afba_okta import RelatedResourcesClient

# Globals
app = None
db = None
login_manager = None


# initialization and config
def create_app(bind=None):
    global app

    app = Flask(__name__,
                template_folder='frontend/templates',
                static_folder='frontend/static')

    # Load the config from environment variables, defaulting to some dev settings
    # target_platform = os.environ['TARGET_PLATFORM']
    # if target_platform == 'AFBA':
    #     app.config.from_object('taa.config_afba')
    #     app.config['IS_5STAR'] = False
    #     app.config['IS_AFBA'] = True
    # elif target_platform == '5STAR':
    app.config.from_object('taa.config_defaults')
    app.config['IS_5STAR'] = True
    app.config['IS_AFBA'] = False

    app.usersClient =  UsersClient(app.config['OKTA_BASEURL'], app.config['OKTA_API_KEY_ID'])
    app.authClient =  AuthClient(app.config['OKTA_BASEURL'], app.config['OKTA_API_KEY_ID'])
    app.relatedResourcesClient = RelatedResourcesClient(app.config['OKTA_BASEURL'], app.config['OKTA_API_KEY_ID'])
    app.groupsClient = UserGroupsClient(app.config['OKTA_BASEURL'], app.config['OKTA_API_KEY_ID'])
    app.Cache={}
    # Exception error handling
    from .errors import init_exception_emails
    init_exception_emails(app, ['david.meyer@ipconsultinginc.com', 'support@ipconsultinginc.com'])

    # Init compression (only active if debug is False)
    Compress(app)

    # Init SSL redirect (only if debug is False AND IS_SSL is true)
    if app.config.get('IS_SSL', False):
        SSLify(app)

    # Init database - export the db variable here so other parts of the app can access the database
    global db
    db = SQLAlchemy(app)

    # User management
    global login_manager
    login_manager = LoginManager()
    login_manager.login_view = "login"
    login_manager.session_protection = "strong"

    @login_manager.user_loader
    def load_user(user_id):

        from taa.services.agents import AgentService
        if not user_id:
            return None

        try:
            return AgentService().find(okta_id=user_id).first()
        except Exception:
            print("Error fetching user with id {}".format(user_id))
            return None

    login_manager.init_app(app)


    # Set up session expiration and refresh at each request.
    @app.before_request
    def before_request():
        import flask
        import flask_login
        import datetime
        flask.session.permanent = True
        app.permanent_session_lifetime = datetime.timedelta(minutes=app.config['SESSION_TIMEOUT_MINUTES'])
        flask.session.modified = True
        flask.g.user = flask_login.current_user

    # Initialize our model service classes
    from taa.services import initialize_services
    initialize_services()

    # Register API blueprints
    from api.cases import bp as cases_api
    from api.products import bp as products_api
    from api.enrollments import bp as enrollments_api
    from api.envelopes import bp as envelopes_api
    from api.submissions import blueprint as submissions_api
    app.register_blueprint(cases_api)
    app.register_blueprint(products_api)
    app.register_blueprint(enrollments_api)
    app.register_blueprint(envelopes_api)
    app.register_blueprint(submissions_api)

    # API custom JSON encoder
    app.json_encoder = JSONEncoder

    # Register API error handlers
    from core import TAAFormError, TAAError
    from api import on_api_error, on_api_form_error
    app.errorhandler(TAAError)(on_api_error)
    app.errorhandler(TAAFormError)(on_api_form_error)

    # Pull in the full DB structure on start
    import models

    # Import views to register decorator views
    import frontend.views

    # Initialize webassets
    from assets import init_app as init_assets
    init_assets(app)
    
    # Enable rewriting static assets to S3 URLs
    from flask_s3 import FlaskS3
    flasks3 = FlaskS3()
    flasks3.init_app(app)
    
    # Initialize Celery task extension
    from taa.tasks import celery
    celery.init_app(app)

    return app


def groups_required(group_list, all=None):
    def decorator(f):
        @wraps(f)
        @login_required
        def wrapper(*args, **kwargs):
            from flask import session
            from flask_login import current_user
            # If we are not logged in, we always abort
            if current_user.is_anonymous():
                # Unauthenticated
                abort(401)
            if not current_user.is_in_groups(group_list, all):
                # Unauthorized
                abort(403)
            return f(*args, **kwargs)
        return wrapper
    
    return decorator

create_app()
