import traceback

from flask import request

from taa import mandrill

def init_exception_emails(app, recipients, sender='error@5starenroll.com'):
    """
    Rewrote Flask-errormail to use mandrill module rather than flask-mail
    """
    
    def email_exception(exception):
        
        msg_contents = [
            'Traceback:',
            '=' * 80,
            traceback.format_exc(),
            '\n',
            '\n',
            'Request Information:',
            '=' * 80,
        ]
        environ = request.environ
        environkeys = sorted(environ.keys())
        for key in environkeys:
            msg_contents.append('%s: %s' % (key, environ.get(key)))
        
        msg = '\n'.join(msg_contents) + '\n'
        
        mandrill.send_email(
            from_email=sender,
            subject='5Star Exception ({hostname})'.format(app.config.get('HOSTNAME')),
            to=[{'email': e} for e in recipients],
            text=msg,
        )
        
    # Don't attempt if debug mode is on
    if not app.config.get('Debug', False):
        app.register_error_handler(500, email_exception)