import datetime

from taa import errors, app
from taa.api import *
from flask import Blueprint, request, abort, make_response, Response
from flask_stormpath import current_user, groups_required, login_required


def parse_dates_as_str_from_request():
    start_date_string = datetime.date(1900, 1, 1).strftime("%Y-%m-%d")
    end_date_string = datetime.datetime.max.strftime("%Y-%m-%d")
    if 'start_date' in request.args and \
                    len(request.args.get('start_date')) > 0 and \
                    request.args.get('start_date') != "undefined":
        start_date_string = request.args.get('start_date')
    if 'end_date' in request.args and \
                    len(request.args.get('end_date')) > 0 and \
                    request.args.get('end_date') != "undefined":
        end_date_string = request.args.get('end_date')

    return end_date_string, start_date_string


def parse_dates_from_request():
    end_date_string, start_date_string = parse_dates_as_str_from_request()
    # noinspection PyBroadException
    try:
        start_date = datetime.datetime.strptime(start_date_string, '%Y-%m-%d')
        end_date = datetime.datetime.strptime(end_date_string, '%Y-%m-%d')
    except Exception:
        abort(400, 'Start and end dates must be valid. Dates must be in the form of YYYY-MM-DD.')
        return

    return end_date, start_date


bp = Blueprint('system', __name__, url_prefix='/system')
api_groups = ['admins']


@route(bp, '/error_email_test')
@login_required
@groups_required(api_groups, all=False)
def error_email_test():
    errors.email_exception(app, Exception("test"))
