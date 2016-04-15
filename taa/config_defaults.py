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
DOCUSIGN_API_ENDPOINT = env_get_text('DOCUSIGN_API_ENDPOINT',
                                     "https://demo.docusign.net/restapi/v2/accounts/%s/" % DOCUSIGN_API_ACCOUNT_ID)

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
EMAIL_SMTP_PASSWORD = env_get_text('MANDRILL_SMTP_PASSWORD', "-h0QL63ppE05jaU3aWvRjg")
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
SQLALCHEMY_DATABASE_URI = env_get_text('DATABASE_URL',
                                       u"postgresql://taa:fQj9lJTFbOQUBYo@localhost/{}".format(DATABASE_NAME))
SQLALCHEMY_ECHO = env_get_bool('SQLALCHEMY_ECHO', True)

# File uploads
MAX_CONTENT_LENGTH = 16777216

GNUPG_DIR = env_get_text('GNUPG_DIR', '/usr/bin/gpg')

# Dell FTP Information
DELL_FTP_HOSTNAME = env_get_text('DELL_FTP_HOSTNAME', 'delmarsd.com')
DELL_FTP_USERNAME = env_get_text('DELL_FTP_USERNAME', 'testftp')
DELL_FTP_PASSWORD = env_get_text('DELL_FTP_PASSWORD', 'wRIu75P12PzVv8JqVVNLh4Nr')
DELL_FTP_WORKING_DIRECTORY = env_get_text('DELL_FTP_WORKING_DIRECTORY', '')
DELL_FTP_PGP_KEY_ID = env_get_text('DELL_FTP_PGP_KEY_ID', '3A74219B887E37BA')
DELL_FTP_PGP_KEY = env_get_text('DELL_FTP_PGP_KEY', """-----BEGIN PGP PUBLIC KEY BLOCK-----
Version: GnuPG v1

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
=jo/P
-----END PGP PUBLIC KEY BLOCK-----""")