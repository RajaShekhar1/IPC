from datetime import timedelta

# runtime environment config
DEBUG = True

# Put server name here for formatting external links
SERVER_NAME = 'localhost:5000'
#lSERVER_NAME = 'taa.herokuapp.com'


# Flask-WTF forms extension config
CSRF_ENABLED = True
SECRET_KEY = 'really-tricky-secret-key'

OPENID_PROVIDERS = [
    { 'name': 'Google', 'url': 'https://www.google.com/accounts/o8/id' },
    { 'name': 'Yahoo', 'url': 'https://me.yahoo.com' },
    { 'name': 'AOL', 'url': 'http://openid.aol.com/<username>' },
    { 'name': 'Flickr', 'url': 'http://www.flickr.com/<username>' },
    { 'name': 'MyOpenID', 'url': 'https://www.myopenid.com' }]

#
#  Stormpath credentials
#
stormpath_SECRET_KEY = 'george5starboat'
stormpath_API_KEY_FILE = '.stormpath/apiKey.properties'
stormpath_APPLICATION = 'TAA'
stormpath_API_KEY_ID = '5GPLR2SQXVPDJEXKXYE287ZYS'
stormpath_API_KEY_SECRET = 'wiZWfjnQu3qBSAYIbQskIn8CKJf/q0A8KxSdMN2NZn8'
stormpath_TIMEOUT_MINS = 15
