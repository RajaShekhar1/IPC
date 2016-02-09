import traceback

from flask import request, Response

from taa import mandrill_flask
from flask_stormpath import current_user


error_sender = ''
error_recipients = []


def init_exception_emails(app, recipients, sender='error@5starenroll.com'):
    """
    Rewrote Flask-errormail to use mandrill module rather than flask-mail
    """

    global error_recipients, error_sender
    error_recipients = recipients
    error_sender = sender

    # Don't attempt if debug mode is on
    if not app.config.get('DEBUG', False):
        app.register_error_handler(500, lambda ex: email_exception(app, ex))


def email_exception(app, exception):
    if not error_recipients:
        # No-op if no recipients.
        return

    if current_user.is_authenticated():
        user_info = [
            'Username: {}'.format(current_user.username),
            'Email: {}'.format(current_user.email),
            'Full Name: {}'.format(current_user.full_name),
            'Groups: {}'.format([g.name for g in current_user.groups]),
            'Directory: {}'.format(current_user.directory.name),
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
        msg_contents.append('%s: %s' % (key, environ.get(key)))

    msg = '\n'.join(msg_contents) + '\n'

    mandrill_flask.send_email(
        from_email=error_sender,
        subject='5Star Exception ({hostname})'.format(hostname=app.config.get('HOSTNAME')),
        to=[{'email': e} for e in error_recipients],
        text=msg,
    )

    # Print to stdout so we can track via normal Heroku logs
    print(msg)

    # Make sure the response still registers as 500
    resp = Response("We're sorry, the server has encountered an error.")
    resp.status_code = 500
    return resp