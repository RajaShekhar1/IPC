from flask import Flask
from flask_sslify import SSLify
from flask_sqlalchemy import SQLAlchemy
from flask.ext.stormpath import StormpathManager

from helpers import JSONEncoder

# initialization and config
#def create_app(config_filename):
app = Flask(__name__, 
            template_folder='frontend/templates',
            static_folder='frontend/static')

sslify = SSLify(app)
app.config.from_object('taa.config_defaults')
#app.config.from_pyfile(config_filename)
app.config.from_envvar("TAA_CONFIG_FILE", silent=True)
#print(app.config)

# user management config
stormpath_manager = StormpathManager(app)
stormpath_manager.login_view = 'login'

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
