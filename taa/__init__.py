
import sys
from gzip import GzipFile
from io import BytesIO

from flask import Flask, current_app, request
from flask_sslify import SSLify
from flask_sqlalchemy import SQLAlchemy
from flask.ext.stormpath import StormpathManager
from flask.ext.compress import Compress
from flask_errormail import mail_on_500

from helpers import JSONEncoder

# initialization and config
#def create_app(config_filename):
app = Flask(__name__,
            template_folder='frontend/templates',
            static_folder='frontend/static')

# Load the config from environment variables, defaulting to some dev settings
app.config.from_object('taa.config_defaults')

# Init compression (only active if debug is False)
Compress(app)

# Init SSL redirect (only if debug is False)
SSLify(app)

# Init user management config
stormpath_manager = StormpathManager(app)
stormpath_manager.login_view = 'login'

# Init database - leave the db variable here so other parts of the app can access the database
db = SQLAlchemy(app)

# Register API blueprints
from api.cases import bp as cases_api
from api.products import bp as products_api
app.register_blueprint(cases_api)
app.register_blueprint(products_api)

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

mail_on_500(app, ['zmason@delmarsd.com'])

