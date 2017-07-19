import datetime

from flask import request
from werkzeug.exceptions import abort


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


def parse_dates_as_str_from_request():
    start_date_string = datetime.datetime.min.strftime("%Y-%m-%d")
    end_date_string = datetime.datetime.max.strftime("%Y-%m-%d")
    if 'start_date' in request.args and len(request.args.get('start_date')) > 0:
        start_date_string = request.args.get('start_date')
    if 'start_date' in request.args and len(request.args.get('start_date')) > 0:
        end_date_string = request.args.get('start_date')

    return end_date_string, start_date_string
