from datetime import timedelta

# Change to False for production
DEBUG = True
ASSETS_DEBUG = True
ASSETS_AUTO_BUILD = True

SECRET_KEY = 'sSYpq8m5vL68/1VKLQwst6II0PjAIP0cYQ31mzdA'

# Flask-WTF forms extension config
WTF_CSRF_ENABLED = False


# Stormpath config
STORMPATH_APPLICATION = 'TAA'
STORMPATH_API_KEY_ID = '5GPLR2SQXVPDJEXKXYE287ZYS'
STORMPATH_API_KEY_SECRET = 'wiZWfjnQu3qBSAYIbQskIn8CKJf/q0A8KxSdMN2NZn8'
STORMPATH_TIMEOUT_MINS = timedelta(minutes=30)
STORMPATH_ENABLE_REGISTRATION = False
STORMPATH_ENABLE_LOGIN = False
STORMPATH_ENABLE_FORGOT_PASSWORD = True

# Email
EMAIL_SMTP_SERVER = "smtp.mandrillapp.com"
EMAIL_SMTP_PORT = 587
EMAIL_SMTP_USERNAME = "taa_mandrill"
EMAIL_SMTP_PASSWORD = "tP3JZX1TlF_pxJDU4vx3Pw"
EMAIL_FROM_ADDRESS = "enrollment@5StarEnroll.com"

# Database
SQLALCHEMY_DATABASE_URI = "postgresql://taa:fQj9lJTFbOQUBYo@localhost/taa"

# File uploads
UPLOAD_FOLDER = "taa/uploads"