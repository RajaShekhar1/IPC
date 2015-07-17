import iso8601
from datetime import datetime

def preprocess_date(data, record):
    if data is None or data == '':
        return None
    try:
        d = iso8601.parse_date(data, None)
        if d >= datetime.today():
            # This can happen when you try to parse 2-digit years (excel issue?)
            # Solution should be OK, but if someone puts a future date in
            # (like for an expected child?) it doesn't work, and also won't
            # work for 100+ year-old people. Which can't apply for life
            # insurance, I think.
            d = datetime(d.year - 100, d.month, d.day)
    except:
        # Can't be parsed as a date; return as-is and let validation
        # handle the error
        return data
    return d