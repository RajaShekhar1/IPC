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
STORMPATH_API_KEY_ID = env_get_text('STORMPATH_API_KEY_ID', '7C9LUQ28BBH4QPB95B2KNER0K')
STORMPATH_API_KEY_SECRET = env_get_text('STORMPATH_API_KEY_SECRET', 'VfhvzFMKXAolgwswz8vmuMjN8zL7DDeCAAYiTk0E/Z0')
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
                                     "")
                                     #"https://demo.docusign.net/restapi/v2/accounts/%s/" % DOCUSIGN_API_ACCOUNT_ID)

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
EMAIL_SMTP_SERVER = "smtp.sparkpostmail.com"
EMAIL_SMTP_PORT = 587
EMAIL_SMTP_USERNAME = env_get_text('EMAIL_SMTP_USERNAME', "SMTP_Injection")
EMAIL_SMTP_PASSWORD = env_get_text('EMAIL_SMTP_PASSWORD', "0785e8d2791fd5d23076765cc25de75023aa9620")
EMAIL_FROM_ADDRESS = env_get_text('EMAIL_FROM_ADDRESS', "enrollment@5StarEnroll.com")

SPARKPOST_API_KEY = "0785e8d2791fd5d23076765cc25de75023aa9620"
# MANDRILL_API_KEY = env_get_text('MANDRILL_API_KEY', "-h0QL63ppE05jaU3aWvRjg")
# MANDRILL_DEFAULT_FROM = env_get_text('MANDRILL_DEFAULT_FROM', "enrollment@5StarEnroll.com")

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
SQLALCHEMY_TRACK_MODIFICATIONS = env_get_bool('SQLALCHEMY_TRACK_MODIFICATIONS', False)
SQLALCHEMY_POOL_SIZE = env_get_int('SQLALCHEMY_POOL_SIZE', 5)
SQLALCHEMY_MAX_OVERFLOW = env_get_int('SQLALCHEMY_MAX_OVERFLOW', 20)


# File uploads
MAX_CONTENT_LENGTH = 16777216

GNUPG_DIR = env_get_text('GNUPG_DIR', '/usr/bin/gpg')

# Dell FTP Information
DELL_FTP_HOSTNAME = env_get_text('DELL_FTP_HOSTNAME', '192.168.0.115')
DELL_FTP_USERNAME = env_get_text('DELL_FTP_USERNAME', 'testftp')
DELL_FTP_PASSWORD = env_get_text('DELL_FTP_PASSWORD', 'wRIu75P12PzVv8JqVVNLh4Nr')
DELL_FTP_WORKING_DIRECTORY = env_get_text('DELL_FTP_WORKING_DIRECTORY', '')
# Public host key line for an SFTP transfer.
#DELL_HOSTKEY = env_get_text('DELL_HOSTKEY', 'ftp03.tagtpa.com,74.51.221.134 ssh-dss AAAAB3NzaC1kc3MAAACBALX33rlTqAwkapnfv8qVPnFeVuxnEDzcugB9zCT8VkCmLqJj3rDbJuotw0+lmHttMw9JIo46v5yxOTyf1dALePsDwUtlGXKnkYVXAeretO2ov2GdmoZu7XFn4bJzCKk2zQ+VzJdWlDpJ7uREkRz68JRpWQpg4NQPVxutSRGXrSYVAAAAFQD9We61P+aQrQjz6itLVO36rSgg7wAAAIEAnEdEz+Cw6dDStAuNP/V5LmjUiC1RV1Gu1n9clEZfnLzWGG3mwA2fgzqcP0TDnn1MYREcfbm/jXI6x99FWhoqdWKbIJPeDZ9rR2nN9JSAgThzEwXQZzstbGwgVLGfhHuq4hVZo2NdLRVwitYwczmGKIEu3ho0jT/EiYDpT5GpN4oAAAB/E7w223kvOakuj1LZNCePT/vVBzZFvT85NcQooeGC2fMmegzLgjW0fOB8lvadTt5lz/B5dx6Gs9i97k7c+iHsLkGVvTn8T2UkiM5YMP45t8VJxtpluEvHOVMCOTX+rBUfOq5AGElJEFXpMba5sfkhhzcMB9VVQcvVCDGaoCLSuw==')
DELL_HOSTKEY = env_get_text('DELL_HOSTKEY', '192.168.0.115 ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC6L4uEcITdzsjRnC7M935M0MSSQDSIPYlPAV/PHhHU+sAYtFq835FU981ZIUXPp1iox9c7U44V+Camaf0pZRnB+/5dLNkqKF+kHVzTi8D4azmjHaY22mPnu1QRPWN8gO5bA/18+p1PqF0dCF2z9Uq+KOIS6f0LX97p6UfDy4DYcSv4nFxFiYXrjK5ARBd6Sks+cYsqgrINL9frSms89hQUuMmnoVgwht/8biUX7k8qOL8UX5oyKAvrkp5KSqET6gYwzB5Z5rsRmQ4yDKT/J4x38dgcmSlzbo48spal+NmzQH52JP9EpzFGOzjJQ47xhiFxEIz8Wd5srdUdnJ9JkWid')
DELL_FTP_PGP_KEY_ID = env_get_text('DELL_FTP_PGP_KEY_ID', '3A74219B887E37BA')
DELL_FTP_PGP_KEY = env_get_text('DELL_FTP_PGP_KEY', """-----BEGIN PGP PUBLIC KEY BLOCK-----
Version: GnuPG v1

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
=jo/P
-----END PGP PUBLIC KEY BLOCK-----""")

# Paylogix FTP Information
PAYLOGIX_FTP_HOSTNAME = env_get_text('PAYLOGIX_FTP_HOSTNAME', 'delmarsd.com')
PAYLOGIX_FTP_USERNAME = env_get_text('PAYLOGIX_FTP_USERNAME', 'testftp')
PAYLOGIX_FTP_PASSWORD = env_get_text('PAYLOGIX_FTP_PASSWORD', 'wRIu75P12PzVv8JqVVNLh4Nr')
PAYLOGIX_FTP_DIRECTORY = env_get_text('PAYLOGIX_FTP_DIRECTORY', '')
PAYLOGIX_PGP_KEY_ID = env_get_text('PAYLOGIX_PGP_KEY_ID', '140A693259BBE0C8')
PAYLOGIX_PGP_KEY = env_get_text('PAYLOGIX_PGP_KEY', """-----BEGIN PGP PUBLIC KEY BLOCK-----
Version: PGP 7.1

mQGiBD/PjzsRBADiubKxkBbHDcUHGIXlGUfRwepFBczZbVta95rTC0yCPjc2XqOr
X/6bd6XobKTfZVC4/DWkrN4fKphlMHRdS6v4jR0hu5/80hyjKcU+MMCBrqbZzCji
yJRlqu/pCJdSqpv/Yrie07akcjTjdkZaCcmrishywYjzIgNweC4rh6zcFQCg/z1I
tLg27OICHe/aCsr78bXUNbkD+wQiYwgRHjElfavxN1VReOp+2KftzEMDIIisNR8M
mHo95biU5nSfAc7GOgFNFjOk3GCL3KCVMieZqKNdBFko57KSED+vqzKt3X13GGfZ
btbaDWg35nlL2VWxhShja0kBP0SYHs2bQcM+cfYSBFYDRils4GYUDVMHPGWf0MGo
wqRDBADDFPNcGlQ2E+0cPtfCC9TI+adOWJry1AP/UqhPHVxJkfiWMdgrTgkqwULc
vhU29svsDoPyiAKsAbbBPbWtUNhuZI2Rm9K0llou8hYREBjGTW0+T7QM73wnODSI
ymbWlSwfesgt7HW7NUf/vG12lUgv5SY/3EF2qU9pan5shvVwgrQhUGF5bG9naXgg
TExDIDxFcmljSkBQYXlsb2dpeC5jb20+iQBYBBARAgAYBQI/z487CAsDCQgHAgEK
AhkBBRsDAAAAAAoJEBQKaTJZu+DIHJEAoMLLhlHSFQkNugsqHG0pgVUy2xDyAJ9r
kU5m7R0v3CGwmWikRa1VBQeLMbkCDQQ/z487EAgA9kJXtwh/CBdyorrWqULzBej5
UxE5T7bxbrlLOCDaAadWoxTpj0BV89AHxstDqZSt90xkhkn4DIO9ZekX1KHTUPj1
WV/cdlJPPT2N286Z4VeSWc39uK50T8X8dryDxUcwYc58yWb/Ffm7/ZFexwGq01ue
jaClcjrUGvC/RgBYK+X0iP1YTknbzSC0neSRBzZrM2w4DUUdD3yIsxx8Wy2O9vPJ
I8BD8KVbGI2Ou1WMuF040zT9fBdXQ6MdGGzeMyEstSr/POGxKUAYEY18hKcKctaG
xAMZyAcpesqVDNmWn6vQClCbAkbTCD1mpF1Bn5x8vYlLIhkmuquiXsNV6TILOwAC
Agf+NXP31LRKAOiHoRWaG+8j8MHJWxeD0Q0wBARmRTaYQ7nczoJWRPbKV0kFXclS
GfeiQlXORVPJmj/VPre5yH26EU/gZX8KlmVTWMzvzNnnP1EmXUZMIlxlJYf+eFjA
rAVzdNzBsSvO9+LPSRbBZ3A8DoW/xALDbGLAJsMa/A3fmqqDOum7FE/XAYwxkqeD
Z9deFiKNsdi2E2HB2aa+MSeLZhBrDNrRZ6S/2Wl+s0PJ0XnGaLJkx3AERikJ5nVX
wxeu3PA0p/Shh7LCZc6bRo815PKtHVL/a80XRnVVLzDgr7pupOMx6uiexMYEfBLh
yRN7ck8vD/hJTOGQnM0yxRLOCYkATAQYEQIADAUCP8+POwUbDAAAAAAKCRAUCmky
WbvgyFkKAJ9uxIr1WKhD5srgq4PS3y9PcI9iRwCbB06Qr8Y0p+xLgLu+lUWR515V
Sls=
=tuZL
-----END PGP PUBLIC KEY BLOCK-----""")

# Electronic signature discloure link
ESIGN_DISCLOSURE_URI = env_get_text('ESIGN_DISCLOSURE_URI', 'http://5starlifeinsurance.com/esign_disclosure')

# Dell Straight-Through-Processing (STP) settings

# Use Dell's "model office" instead of production if True.
IS_STP_DEBUG = env_get_bool('IS_STP_DEBUG', True)

# If True, Don't even send to "model office", but pretend to.
IS_STP_SIMULATE = env_get_bool('IS_STP_SIMULATE', True)

STP_URL = env_get_text('STP_URL', 'https://extranetapps-mo.tagtpa.com/TxLifeImport/TxLife.asmx?WSDL')
#STP_LIVE_URL = env_get_text('STP_LIVE_URL', 'https://extranetapps.tagtpa.com/TxLifeImport/TxLife.asmx?WSDL')
#STP_URL = STP_DEBUG_URL if IS_STP_DEBUG else STP_LIVE_URL
