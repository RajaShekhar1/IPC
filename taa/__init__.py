from flask import Flask
from flask_sslify import SSLify
from flask_sqlalchemy import SQLAlchemy
from flask.ext.stormpath import StormpathManager

# initialization and config
#def create_app(config_filename):
app = Flask(__name__)
sslify = SSLify(app)
app.config.from_object('taa.config_defaults')
#app.config.from_pyfile(config_filename)
app.config.from_envvar("TAA_CONFIG_FILE", silent=True)
#print(app.config)

# user management config
stormpath_manager = StormpathManager(app)
stormpath_manager.login_view = 'login'

db = SQLAlchemy(app)

# Init db structure on start 
from taa.model.Database import metadata
metadata.create_all(db.engine)

# Import views to register decorator views
import views
