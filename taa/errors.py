import traceback

from flask import request, Response

from taa import mandrill_flask
from flask_stormpath import current_user

def init_exception_emails(app, recipients, sender='error@5starenroll.com'):
    """
    Rewrote Flask-errormail to use mandrill module rather than flask-mail
    """
    
    def email_exception(exception):
        
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
            from_email=sender,
            subject='5Star Exception ({hostname})'.format(hostname=app.config.get('HOSTNAME')),
            to=[{'email': e} for e in recipients],
            text=msg,
        )
        
        # Make sure the response still registers as 500 so production logs show the error
        resp = Response("We're sorry, the server has encountered an error.")
        resp.status_code = 500
        return resp
        
    # Don't attempt if debug mode is on
    if not app.config.get('Debug', False):
        app.register_error_handler(500, email_exception)