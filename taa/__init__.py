
from flask import Flask
from flask_sslify import SSLify
from flask_sqlalchemy import SQLAlchemy
from flask.ext.stormpath import StormpathManager
from flask.ext.compress import Compress
from flask.ext.mandrill import Mandrill

from .helpers import JSONEncoder

# initialization and config
#def create_app(config_filename):
app = Flask(__name__,
            template_folder='frontend/templates',
            static_folder='frontend/static')

# Load the config from environment variables, defaulting to some dev settings
app.config.from_object('taa.config_defaults')

# Mandrill emailing
mandrill_flask = Mandrill(app)

# Exception error handling
#   (Import after the mandrill import line for dependency correctness)
from .errors import init_exception_emails
init_exception_emails(app, ['zmason@delmarsd.com', 'jkayser@delmarsd.com'])

# Init compression (only active if debug is False)
Compress(app)

# Init SSL redirect (only if debug is False AND IS_SSL is true)
if app.config.get('IS_SSL', False):
    SSLify(app)

# Init user management config
stormpath_manager = StormpathManager(app)
stormpath_manager.login_view = 'login'

# Init database - export the db variable here so other parts of the app can access the database
db = SQLAlchemy(app)

# Initialize our model service classes
from taa.services import services
from taa.services.agents import AgentService
from taa.services.cases import (
    CaseService,
    CaseEnrollmentPeriodsService,
    CensusRecordService,
    SelfEnrollmentService,
)
from taa.services.enrollments import (
    EnrollmentApplicationService,
    SelfEnrollmentEmailService,
    SelfEnrollmentLinkService,
    SelfEnrollmentEmailBatchService,
    EnrollmentImportService,
    EnrollmentApplicationCoverageService,
    EnrollmentReportService,
)
from taa.services.products import (
    ProductService,
    ProductFormService,
    StatementOfHealthQuestionService,
)

services.Provide('CaseService', CaseService())
services.Provide('CaseEnrollmentPeriodsService', CaseEnrollmentPeriodsService())
services.Provide('CensusRecordService', CensusRecordService())
services.Provide('SelfEnrollmentService', SelfEnrollmentService())

services.Provide('AgentService', AgentService())

services.Provide('ProductService', ProductService())
services.Provide('ProductFormService', ProductFormService())
services.Provide('StatementOfHealthQuestionService', StatementOfHealthQuestionService())

services.Provide('EnrollmentApplicationService', EnrollmentApplicationService())
services.Provide('EnrollmentApplicationCoverageService', EnrollmentApplicationCoverageService())
services.Provide('EnrollmentImportService', EnrollmentImportService())
services.Provide('SelfEnrollmentEmailService', SelfEnrollmentEmailService())
services.Provide('SelfEnrollmentLinkService', SelfEnrollmentLinkService())
services.Provide('SelfEnrollmentEmailBatchService', SelfEnrollmentEmailBatchService())
services.Provide('EnrollmentReportService', EnrollmentReportService())

# Register API blueprints
from api.cases import bp as cases_api
from api.products import bp as products_api
from api.enrollments import bp as enrollments_api
app.register_blueprint(cases_api)
app.register_blueprint(products_api)
app.register_blueprint(enrollments_api)

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

