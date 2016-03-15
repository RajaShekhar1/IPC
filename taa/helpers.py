import cStringIO
import csv
import datetime
import json
from decimal import Decimal

import codecs
from flask import request
from flask.json import JSONEncoder as FlaskJSONEncoder
from wtforms.fields import SelectField
from wtforms.widgets import html_params, HTMLString
from jinja2 import escape



def get_posted_data():
    """
    Allows for either JSON or form-encoded requests
    """
    form_data = request.form or request.get_json()
    return {k: form_data[k] for k in form_data}
    

# https://github.com/mattupstate/overholt
class JSONEncoder(FlaskJSONEncoder):
    """Custom :class:`JSONEncoder` which respects objects that include the
    :class:`JsonSerializer` mixin.
    """
    def default(self, obj):
        if isinstance(obj, JsonSerializable):
            return obj.to_json()
        # Flask doesn't handle plain dates (it does have datetimes though)
        if isinstance(obj, datetime.date):
            return obj.isoformat()
        
        if isinstance(obj, Decimal):
            return str(obj) 

        # As a last-ditch effort, try a to_json method if it exists.
        if hasattr(obj, 'to_json'):
            return obj.to_json()

        return super(JSONEncoder, self).default(obj)

def json_encode(val):
    return json.dumps(val, cls=JSONEncoder)

# https://github.com/mattupstate/overholt    
class JsonSerializable(object):
    """A mixin that can be used to mark a SQLAlchemy model class which
        implements a :func:`to_json` method. The :func:`to_json` method is used
        in conjuction with the custom :class:`JSONEncoder` class. By default this
        mixin will assume all properties of the SQLAlchemy model are to be visible
        in the JSON output. Extend this class to customize which properties are
        public, hidden or modified before being being passed to the JSON serializer.
        """
    
    __json_public__ = None
    __json_hidden__ = None
    __json_modifiers__ = None
    __json_add__ = None
    
    def get_field_names(self):
        if hasattr(self, '__mapper__'):
            # Iterate through SQLAlchemy properties
            for p in self.__mapper__.iterate_properties:
                yield p.key
        else:
            # Iterate through normal properties
            for k in self.__dict__.keys():
                yield k

    def to_json(self):
        field_names = self.get_field_names()

        # Remove hidden fields first so we don't trigger any SQLAlchemy issues
        hidden = self.__json_hidden__ or []
        public = [k for k in (self.__json_public__ or field_names) if k not in hidden]
        modifiers = self.__json_modifiers__ or dict()
        added = self.__json_add__ or dict()

        retv = dict()
        for key in public:
            retv[key] = getattr(self, key)
        for key, modifier in modifiers.items():
            value = getattr(self, key)
            retv[key] = modifier(value, self)
        for key, modifier in added.items():
            retv[key] = modifier(self)

        # Do one last pass through hidden in case it was added in the modifiers or added
        for key in hidden:
            if key in retv:
                retv.pop(key)

        return retv
    
    
#
# SelectWithDisable and SelectFieldWithDisable from http://stackoverflow.com/questions/8463421/how-to-render-my-select-field-with-wtforms
#
class SelectWithDisable(object):
    """
    Renders a select field.

    If `multiple` is True, then the `size` property should be specified on
    rendering to make the field useful.

    The field must provide an `iter_choices()` method which the widget will
    call on rendering; this method must yield tuples of 
    `(value, label, selected, disabled)`.
    """
    def __init__(self, multiple=False):
        self.multiple = multiple

    def __call__(self, field, **kwargs):
        kwargs.setdefault('id', field.id)
        if self.multiple:
            kwargs['multiple'] = 'multiple'
        html = [u'<select %s>' % html_params(name=field.name, **kwargs)]
        for val, label, selected, disabled in field.iter_choices():
            html.append(self.render_option(val, label, selected, disabled))
        html.append(u'</select>')
        return HTMLString(u''.join(html))

    @classmethod
    def render_option(cls, value, label, selected, disabled):
        options = {'value': value}
        if selected:
            options['selected'] = u'selected'
        if disabled:
            options['disabled'] = u'disabled'
        return HTMLString(u'<option %s>%s</option>' % (html_params(**options), escape(unicode(label, 'utf-8'))))


class SelectFieldWithDisable(SelectField):
    widget = SelectWithDisable()

    def iter_choices(self):
        for value, label, disabled in self.choices:
            yield (value, label, self.coerce(value) == self.data, disabled)


# Slightly modified from https://docs.python.org/2.7/library/csv.html#csv-examples for unicode encoded CSV files.
class UnicodeCsvWriter:
    """
    A CSV writer which will write rows to CSV a stream,
    which is encoded in the given encoding.
    """

    def __init__(self, stream, dialect=csv.excel, encoding="utf-8", **kwds):
        # Redirect output to a queue
        self.queue = cStringIO.StringIO()
        self.writer = csv.writer(self.queue, dialect=dialect, **kwds)
        self.stream = stream
        self.encoding = encoding
        self.encoder = codecs.getincrementalencoder(encoding)()

    def writerow(self, row):
        # First, ensure all the data has been converted to unicode.
        unicode_row = []
        for col in row:
            if not isinstance(col, unicode):
                col = unicode(col, 'utf-8')
            unicode_row.append(col)

        # Use the standard CSV writer to get correct formatting.
        self.writer.writerow([col.encode("utf-8") for col in unicode_row])

        # Fetch UTF-8 output from the queue ...
        data = self.queue.getvalue()
        data = data.decode("utf-8")
        # ... and reencode it into the target encoding
        data = self.encoder.encode(data)
        # write to the target stream
        self.stream.write(data)
        # empty queue
        self.queue.truncate(0)

    def writerows(self, rows):
        for row in rows:
            self.writerow(row)
