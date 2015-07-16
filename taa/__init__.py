import locale
# Make sure this is set for the whole app for formatting dates, times, currency, etc.
locale.setlocale(locale.LC_ALL, 'en_US.UTF-8')

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
from taa.services import services_broker
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
    ImagedFormGeneratorService,
    FormPDFRenderer,
    FormTemplateTabRepository,
)
from taa.services.products import (
    ProductService,
    ProductFormService,
    StatementOfHealthQuestionService,
)
from taa.services.data_import import (
    FileImportService,
)

services_broker.Provide('CaseService', CaseService())
services_broker.Provide('CaseEnrollmentPeriodsService', CaseEnrollmentPeriodsService())
services_broker.Provide('CensusRecordService', CensusRecordService())
services_broker.Provide('SelfEnrollmentService', SelfEnrollmentService())

services_broker.Provide('AgentService', AgentService())

services_broker.Provide('ProductService', ProductService())
services_broker.Provide('ProductFormService', ProductFormService())
services_broker.Provide('StatementOfHealthQuestionService', StatementOfHealthQuestionService())

services_broker.Provide('EnrollmentApplicationService', EnrollmentApplicationService())
services_broker.Provide('EnrollmentApplicationCoverageService', EnrollmentApplicationCoverageService())
services_broker.Provide('EnrollmentImportService', EnrollmentImportService())
services_broker.Provide('SelfEnrollmentEmailService', SelfEnrollmentEmailService())
services_broker.Provide('SelfEnrollmentLinkService', SelfEnrollmentLinkService())
services_broker.Provide('SelfEnrollmentEmailBatchService', SelfEnrollmentEmailBatchService())
services_broker.Provide('EnrollmentReportService', EnrollmentReportService())
services_broker.Provide('ImagedFormGeneratorService', ImagedFormGeneratorService())
services_broker.Provide("FormPDFRenderer", FormPDFRenderer())
services_broker.Provide("FormTemplateTabRepository", FormTemplateTabRepository())

services_broker.Provide('FileImportService', FileImportService)

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
