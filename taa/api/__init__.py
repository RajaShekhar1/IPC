# -*- coding: utf-8 -*-
"""
    API setup
"""

from functools import wraps

from flask import jsonify
from flask_stormpath import login_required
from flask_wtf.csrf import CsrfProtect
from werkzeug.wrappers import Response

from ..core import TAAError, TAAFormError
from ..helpers import JSONEncoder


# def create_app(settings_override=None, register_security_blueprint=False):
#     """Returns the API application instance"""
# 
#     app = factory.create_app(__name__, __path__, settings_override,
#                              register_security_blueprint=register_security_blueprint)
# 
#     # Set the default JSON encoder
#     app.json_encoder = JSONEncoder
# 
#     # Register custom error handlers
#     app.errorhandler(TAAError)(on_api_error)
#     app.errorhandler(TAAFormError)(on_api_form_error)
#     app.errorhandler(404)(on_404)
# 
#     return app


def route(bp, *args, **kwargs):
    kwargs.setdefault('strict_slashes', False)
    csrf = CsrfProtect()
    # Wrap all routes with login_required by default, wrap response data using jsonify
    def decorator(f):
        
        @bp.route(*args, **kwargs)
        @login_required
        @wraps(f)
        def wrapper(*args, **kwargs):
            
            # Call the wrapped function
            ret_val = f(*args, **kwargs)
            
            if isinstance(ret_val, Response) and ret_val.status_code == 302 and ret_val.location.startswith('/login'):
                # Return a notice that we are not logged in
                return jsonify(dict(
                    data={}, 
                    errors=[{'msg':'You are not logged in.'}], 
                    redirect=ret_val.location)
                )
            elif isinstance(ret_val, Response):
                return ret_val, ret_val.status_code
            
            # Specially formatted data
            elif isinstance(ret_val, str):
                return Response(ret_val), 200
            
            if isinstance(ret_val, tuple):
                # pull out the response if a status code is given explicitly 
                data = ret_val[0]
                sc = ret_val[1]
            else:
                data = ret_val
                sc = 200

            # Serialize the response object into json data
            return jsonify(dict(data=data)), sc
        
        return f
    
    return decorator


def on_api_error(e):
    return jsonify(dict(error=e.msg)), 400


def on_api_form_error(e):
    return jsonify(dict(errors=e.errors)), 400


def on_api_404(e):
    return jsonify(dict(error='Not found')), 404
