import traceback

from flask import request, Response
from flask_login import current_user

from services import LookupService

error_sender = ''
error_recipients = []


def init_exception_emails(app, recipients):
    """
    Rewrote Flask-errormail to use our own mail service
    """

    global error_recipients, error_sender
    error_recipients = recipients
    error_sender = app.config.get('EMAIL_FROM_ADDRESS', 'error@5starenroll.com')

    # Don't attempt if debug mode is on
    if not app.config.get('DEBUG', False):
        app.register_error_handler(500, lambda ex: email_exception(app, ex))


def email_exception(app, exception):
    if not error_recipients:
        # No-op if no recipients.
        return

    if current_user.is_authenticated:
        user_info = [

            u'Email: {}'.format(current_user.email),
            u'Name: {}'.format(current_user.name()),
            u'Groups: {}'.format([g.group for g in current_user.groups]),
            u'ID: {}'.format(current_user.id),
            u'Okta ID: {}'.format(current_user.okta_id),
        ]
    else:
        user_info = [
            'Unauthenticated',
        ]

    msg_contents = [
        'Traceback:',
        '=' * 80,
        traceback.format_exc(),
        '\n',
        'User Information:',
        '\n'.join(user_info),
        '\n',
        'Request Information:',
        '=' * 80,
    ]
    environ = request.environ
    environkeys = sorted(environ.keys())
    for key in environkeys:
        msg_contents.append(u'%s: %s' % (key, environ.get(key)))

    msg = '\n'.join(msg_contents) + '\n'

    # Print to stdout so we can track via normal Heroku logs
    print(msg)

    mailer = LookupService('MailerService')

    try:
        mailer.send_email(
            from_email=error_sender,
            subject=u'5Star Exception ({hostname})'.format(hostname=app.config.get('HOSTNAME')),
            to="5star@ipconsultinginc.com",
            text=msg,
            track_clicks=False,
        )
    except Exception:
        print "failed to send admin email notification:"

    try:
        mailer.send_email(
            from_email=error_sender,
            subject=u'5Star Exception ({hostname})'.format(hostname=app.config.get('HOSTNAME')),
            to=[e for e in error_recipients],
            text=msg,
            track_clicks=False,
        )
    except Exception:
        print "failed to send admin email notification:"


    # Make sure the response still registers as 500
    resp = Response("We're sorry, the server has encountered an error.")
    resp.status_code = 500
    return resp