from datetime import timedelta
import os

def parse_bool(val):
    if isinstance(val, bool):
        return val
    return val.lower() in ["true", "1", "yes"] if val else False

def env_get_bool(env_name, default_val=None):
    return parse_bool(os.environ.get(env_name, default_val))

def env_get_text(env_name, default_val=None):
    return os.environ.get(env_name, default_val)

def env_get_int(env_name, default_val=None):
    val = os.environ.get(env_name, default_val)
    return int(val) if val is not None else None

# production should have DEBUG=False
DEBUG = env_get_bool('DEBUG', True)
ALLOW_DUPLICATE_SUBMISSION = env_get_bool('ALLOW_DUPLICATE_SUBMISSION', True)
ASSETS_DEBUG = env_get_bool('ASSETS_DEBUG', True)
ASSETS_AUTO_BUILD = env_get_bool('ASSETS_AUTO_BUILD', True)
SECRET_KEY = env_get_text('SECRET_KEY', 'sSYpq8m5vL68/1VKLQwst6II0PjAIP0cYQ31mzdA')

# Flask-WTF forms extension config
WTF_CSRF_ENABLED = False

IS_SSL = env_get_bool('IS_SSL', False)
HOSTNAME = SERVER_NAME = env_get_text('HOSTNAME', "taa.local:5000")
PREFERRED_URL_SCHEME = 'https' if IS_SSL else 'http'

# Stormpath config
STORMPATH_APPLICATION = env_get_text('STORMPATH_APPLICATION', 'TAA-Sandbox')

# Live stormpath
STORMPATH_API_KEY_ID = env_get_text('STORMPATH_API_KEY_ID', '5GPLR2SQXVPDJEXKXYE287ZYS')
STORMPATH_API_KEY_SECRET = env_get_text('STORMPATH_API_KEY_SECRET', 'wiZWfjnQu3qBSAYIbQskIn8CKJf/q0A8KxSdMN2NZn8')
STORMPATH_COOKIE_DURATION = timedelta(minutes=env_get_int('STORMPATH_COOKIE_DURATION_MINS', 1000))
STORMPATH_ENABLE_REGISTRATION = False
STORMPATH_ENABLE_LOGIN = False
STORMPATH_ENABLE_FORGOT_PASSWORD = True

# DocuSign credentials - this is a test account.
DOCUSIGN_INTEGRATOR_KEY = env_get_text('DOCUSIGN_INTEGRATOR_KEY', 'DELM-0d0ee159-7e61-499f-81ec-5c03bec86ec3')
DOCUSIGN_API_ACCOUNT_ID = env_get_text('DOCUSIGN_API_ACCOUNT_ID', '5988eb5b-bee1-4825-a078-dcac445a22ce')
DOCUSIGN_API_USERNAME = env_get_text('DOCUSIGN_API_USERNAME', 'cb64545b-0bb7-4e77-bb0c-492b02c3dd5b')
DOCUSIGN_API_PASSWORD = env_get_text('DOCUSIGN_API_PASSWORD', '12121212')
# Trailing slash required
DOCUSIGN_API_ENDPOINT = env_get_text('DOCUSIGN_API_ENDPOINT', "https://demo.docusign.net/restapi/v2/accounts/%s/"%DOCUSIGN_API_ACCOUNT_ID)

DOCUSIGN_LIVE_CC_RECIPIENTS = env_get_bool('DOCUSIGN_LIVE_CC_RECIPIENTS', False)
if DOCUSIGN_LIVE_CC_RECIPIENTS:
    DOCUSIGN_CC_RECIPIENTS = [
        ('Archive', 'docusign.transaction.archive@5starenroll.com'),
        ('New Business Team', 'newbusiness@5starenroll.com'),
    ]
else:
    # Demo recipients
    DOCUSIGN_CC_RECIPIENTS = [
        ('Test CC Recipient', 'zmason@delmarsd.com'),
    ]

# Email
EMAIL_SMTP_SERVER = "smtp.mandrillapp.com"
EMAIL_SMTP_PORT = 587
EMAIL_SMTP_USERNAME = env_get_text('MANDRILL_SMTP_USERNAME', "taa_mandrill")
EMAIL_SMTP_PASSWORD =  env_get_text('MANDRILL_SMTP_PASSWORD', "-h0QL63ppE05jaU3aWvRjg")
EMAIL_FROM_ADDRESS = "enrollment@5StarEnroll.com"

MANDRILL_API_KEY = env_get_text('MANDRILL_API_KEY', "-h0QL63ppE05jaU3aWvRjg")
MANDRILL_DEFAULT_FROM = env_get_text('MANDRILL_DEFAULT_FROM', "enrollment@5StarEnroll.com")

# Celery message broker (background task runner)
BROKER_URL = env_get_text('CELERY_BROKER_URL', "amqp://")
if env_get_text('CLOUDAMQP_URL'):
    BROKER_URL = env_get_text('CLOUDAMQP_URL')
# See for config settings for CloudAMQP: https://www.cloudamqp.com/docs/python.html
BROKER_POOL_LIMIT = 1
CELERY_SEND_EVENTS = False
CELERY_EVENT_QUEUE_EXPIRES = 60
CELERY_TASK_SERIALIZER = 'json'
CELERY_ACCEPT_CONTENT = ['json']
CELERY_TIMEZONE = 'US/Eastern'
CELERY_ACKS_LATE = True

# Database
DATABASE_NAME = env_get_text('DATABASE_NAME', 'taa')
SQLALCHEMY_DATABASE_URI = env_get_text('DATABASE_URL', "postgresql://taa:test@localhost/{}".format(DATABASE_NAME))
SQLALCHEMY_ECHO = env_get_bool('SQLALCHEMY_ECHO', True)

# File uploads
MAX_CONTENT_LENGTH = 16777216
