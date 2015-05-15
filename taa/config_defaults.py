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
ASSETS_DEBUG = env_get_bool('ASSETS_DEBUG', True)
ASSETS_AUTO_BUILD = env_get_bool('ASSETS_AUTO_BUILD', True)
SECRET_KEY = env_get_text('SECRET_KEY', 'sSYpq8m5vL68/1VKLQwst6II0PjAIP0cYQ31mzdA')

# Flask-WTF forms extension config
WTF_CSRF_ENABLED = False

IS_SSL = env_get_bool('IS_SSL', False)
HOSTNAME = env_get_text('HOSTNAME', "taa.local:5000")

# Stormpath config
STORMPATH_APPLICATION = env_get_text('STORMPATH_APPLICATION', 'TAA-Sandbox')

# Live stormpath
STORMPATH_API_KEY_ID = env_get_text('STORMPATH_API_KEY_ID', '5GPLR2SQXVPDJEXKXYE287ZYS')
STORMPATH_API_KEY_SECRET = env_get_text('STORMPATH_API_KEY_SECRET', 'wiZWfjnQu3qBSAYIbQskIn8CKJf/q0A8KxSdMN2NZn8')
STORMPATH_COOKIE_DURATION = timedelta(minutes=env_get_int('STORMPATH_COOKIE_DURATION_MINS', 30))
STORMPATH_ENABLE_REGISTRATION = False
STORMPATH_ENABLE_LOGIN = False
STORMPATH_ENABLE_FORGOT_PASSWORD = True

# DocuSign credentials
DOCUSIGN_INTEGRATOR_KEY = env_get_text('DOCUSIGN_INTEGRATOR_KEY', 'DELM-0d0ee159-7e61-499f-81ec-5c03bec86ec3')
DOCUSIGN_API_ACCOUNT_ID = env_get_text('DOCUSIGN_API_ACCOUNT_ID', '5988eb5b-bee1-4825-a078-dcac445a22ce')
DOCUSIGN_API_USERNAME = env_get_text('DOCUSIGN_API_USERNAME', 'cb64545b-0bb7-4e77-bb0c-492b02c3dd5b')
DOCUSIGN_API_PASSWORD = env_get_text('DOCUSIGN_API_PASSWORD', '12121212')
DOCUSIGN_API_ENDPOINT = env_get_text('DOCUSIGN_API_ENDPOINT', "https://demo.docusign.net/restapi/v2/accounts/%s"%DOCUSIGN_API_ACCOUNT_ID)

# Email
EMAIL_SMTP_SERVER = "smtp.mandrillapp.com"
EMAIL_SMTP_PORT = 587
EMAIL_SMTP_USERNAME = env_get_text('MANDRILL_SMTP_USERNAME', "taa_mandrill")
EMAIL_SMTP_PASSWORD =  env_get_text('MANDRILL_SMTP_PASSWORD', "-h0QL63ppE05jaU3aWvRjg")
EMAIL_FROM_ADDRESS = "enrollment@5StarEnroll.com"

MANDRILL_API_KEY = env_get_text('MANDRILL_API_KEY', "-h0QL63ppE05jaU3aWvRjg")
MANDRILL_DEFAULT_FROM = env_get_text('MANDRILL_DEFAULT_FROM', "enrollment@5StarEnroll.com")

# Database
SQLALCHEMY_DATABASE_URI = env_get_text('DATABASE_URL', "postgresql://taa:fQj9lJTFbOQUBYo@localhost/taa")
SQLALCHEMY_ECHO = env_get_bool('SQLALCHEMY_ECHO', True)

# File uploads
MAX_CONTENT_LENGTH = 16777216

