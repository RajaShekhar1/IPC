from datetime import timedelta

# runtime environment config
DEBUG = False
ASSETS_DEBUG = False
ASSETS_AUTO_BUILD = True

SECRET_KEY = 'sSYpq8m5vL68/1VKLQwst6II0PjAIP0cYQ31mzdA'

IS_SSL = True
HOSTNAME = "5starenroll.com"

# Put server name here for formatting external links (not necessary - relative links are fine)
#SERVER_NAME = 'localhost:5000'
#SERVER_NAME = '5starenroll.com'

# Flask-WTF forms extension config
CSRF_ENABLED = True

OPENID_PROVIDERS = [
    { 'name': 'Google', 'url': 'https://www.google.com/accounts/o8/id' },
    { 'name': 'Yahoo', 'url': 'https://me.yahoo.com' },
    { 'name': 'AOL', 'url': 'http://openid.aol.com/<username>' },
    { 'name': 'Flickr', 'url': 'http://www.flickr.com/<username>' },
    { 'name': 'MyOpenID', 'url': 'https://www.myopenid.com' }]

#
#  Stormpath config
#
STORMPATH_APPLICATION = 'TAA'
STORMPATH_API_KEY_ID = '5GPLR2SQXVPDJEXKXYE287ZYS'
STORMPATH_API_KEY_SECRET = 'wiZWfjnQu3qBSAYIbQskIn8CKJf/q0A8KxSdMN2NZn8'
STORMPATH_TIMEOUT_MINS = timedelta(minutes=30)
STORMPATH_ENABLE_REGISTRATION = False
STORMPATH_ENABLE_LOGIN = False
STORMPATH_ENABLE_FORGOT_PASSWORD = True


# Email
EMAIL_SMTP_SERVER = "smtp.gmail.com"
EMAIL_SMTP_PORT = 587
EMAIL_SMTP_USERNAME = "enrollment@5starlifemail.com"
EMAIL_SMTP_PASSWORD = "enrollment55"
EMAIL_FROM_ADDRESS = "enrollment@5StarEnroll.com"


# Database
# -- the real connection string will be pulled from a heroku environment variable
DATABASE_CONNECTION_STRING = "sqlite:///fallback.db"
