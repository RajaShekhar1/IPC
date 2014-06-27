import random
from datetime import datetime
import smtplib
from email.mime.multipart import MIMEMultipart
from email.mime.text import MIMEText

from flask import url_for, render_template
from dateutil.relativedelta import relativedelta
from mailer import Mailer, Message
from flask.ext.stormpath import user

from flask.ext.wtf import Form
from wtforms.fields import StringField, SelectField
from wtforms.validators import InputRequired, Email
from wtforms.widgets import html_params, HTMLString
from jinja2 import escape


class Case(object):
    def __init__(self, id, company_name, situs_state, product):
        self.case_id = id
        self.company_name = company_name
        self.situs_state = situs_state
        self.product = product
        
        
class Enrollment(object):
    def __init__(self, id, case, employee_first, employee_last, employee_email):
        self.enrollment_id = id
        
        self.case = case
        self.employee_first = employee_first
        self.employee_last = employee_last
        self.employee_email = employee_email
        
    def generate_enrollment_request(self):
        
        request = EnrollmentRequest(
            id=None,
            enrollment=self,
            expiration_date=self.get_link_expiration_date(),
            token=self.generate_token()
        )
        
        return request
        
    def generate_token(self, token_length=12):
        chars = "ABCDEF0123456789"
        return ''.join([random.choice(chars) for i in range(token_length)])
    
    def get_link_expiration_date(self):
        # three days from this morning at midnight
        return datetime.today() + relativedelta(days=3)
            
class EnrollmentRequest(object):
    def __init__(self, id, enrollment, expiration_date, token):
        self.id = id
        self.enrollment = enrollment
        self.expiration_date = expiration_date
        self.token = token
        
    def generate_url(self):
        return url_for("email_link_handler", token=self.token, _external=True)
        
class EnrollmentEmail(object):
    def __init__(self, smtp_server, smtp_port, smtp_user, smtp_password, from_address):
        self.smtp_server = smtp_server
        self.smtp_port = smtp_port
        self.smtp_user = smtp_user
        self.smtp_password = smtp_password
        self.from_address = from_address
        
    def send_enrollment_request(self, enrollment_request):
        
        to_user = enrollment_request.enrollment.employee_email
        
        msg = MIMEMultipart()
        msg['From'] = self.from_address
        msg['To'] = to_user
        msg['Subject'] = "Enrollment Request:  {employee_first} {employee_last} ({company_name}) - {product_name}".format(
            employee_first=enrollment_request.enrollment.employee_first,
            employee_last=enrollment_request.enrollment.employee_last,
            company_name=enrollment_request.enrollment.case.company_name,
            product_name=enrollment_request.enrollment.case.product.name,
        )
        body = render_template(
            "enrollment_email.html",
            enrollment=enrollment_request.enrollment,
            enrollment_url=enrollment_request.generate_url()
        )
        
        msg.attach(MIMEText(body, 'html'))
        
        connection = smtplib.SMTP("smtp.gmail.com", 587)
        connection.ehlo()
        if self.smtp_user and self.smtp_password:
            connection.starttls()
            connection.ehlo()
            connection.login(self.smtp_user, self.smtp_password)
        
        connection.sendmail(self.smtp_user, to_user, msg.as_string())
        connection.close()
        
        

class AgentActivationEmail(object):
    """
    This perhaps should go elsewhere, but wanted to send a near-identical email as EnrollmentEmail above.  Probably should abstract the functions, but we can do that in Phase N+1
    """
    def __init__(self, smtp_server, smtp_port, smtp_user, smtp_password, from_address):
        self.smtp_server = smtp_server
        self.smtp_port = smtp_port
        self.smtp_user = smtp_user
        self.smtp_password = smtp_password
        self.from_address = from_address
        
    def send_activation_notice(self, to_email, agent_name, url):
        
        to_user = to_email
        
        msg = MIMEMultipart()
        msg['From'] = self.from_address
        msg['To'] = to_user
        msg['Subject'] = "Activation Notice for 5Star Online Enrollment"
        body = render_template(
            "activation_email.html",
            agent_name=agent_name,
            landing_url=url
        )
        
        print "url is ", url

        msg.attach(MIMEText(body, 'html'))
        
        connection = smtplib.SMTP("smtp.gmail.com", 587)
        connection.ehlo()
        if self.smtp_user and self.smtp_password:
            connection.starttls()
            connection.ehlo()
            connection.login(self.smtp_user, self.smtp_password)
        
        connection.sendmail(self.smtp_user, to_user, msg.as_string())
        connection.close()


class NotifyAdminEmail(object):
    """
    This perhaps should go elsewhere
    """
    def __init__(self):
        self.smtp_user = "enrollment@5starlifemail.com"
        self.smtp_password = "enrollment55"
        
    def send_registration_notice(self, agent_name):
        
        to_user = "admin@5starenroll.com"
        
        msg = MIMEMultipart()
        msg['From'] = "enrollment@5StarEnroll.com"
        msg['To'] = "admin@5StarEnroll.com"
        msg['Subject'] = "Activation Request from " + agent_name
        body = render_template(
            "notify_admin_email.html",
            agent_name=agent_name
        )
        
        msg.attach(MIMEText(body, 'html'))
        
        connection = smtplib.SMTP("smtp.gmail.com", 587)
        connection.ehlo()
        if self.smtp_user and self.smtp_password:
            connection.starttls()
            connection.ehlo()
            connection.login(self.smtp_user, self.smtp_password)
        
        connection.sendmail(self.smtp_user, to_user, msg.as_string())
        connection.close()
        

#
# SelectWithDisable and SelectFieldWithDisable from http://stackoverflow.com/questions/8463421/how-to-render-my-select-field-with-wtforms
#  although I added a bogus X parm in for loop in middle of __call__ method below to accommodate the coerce value in tuple
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
        for val, label, selected, disabled, x in field.iter_choices():
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
        return HTMLString(u'<option %s>%s</option>' % (html_params(**options), escape(unicode(label))))


class SelectFieldWithDisable(SelectField):
    widget = SelectWithDisable()

    def iter_choices(self):
        for value, label, selected, disabled in self.choices:
            yield (value, label, selected, disabled, self.coerce(value) == self.data)


#
# each set of option values for respective Products is a tuple of 
# value, name, selected (True/False), disabled (True/False)) 
#
#[('1', 'VEHICLES', False, True), ('2', 'Cars', False, False), ('3', 'Motorcycles', False, False)]
#
FPPTI_states = [
    ("", ' ', False, False),
    ('AL', 'Alabama', False, False),
    ('AK', 'Alaska', False, False),
    ('AZ', 'Arizona', False, False),
    ('AR', 'Arkansas', False, False),
    ('CA', 'California', False, False),
    ('CO', 'Colorado', False, True),
    ('CT', 'Connecticut', False, True),
    ('DE', 'Delaware', False, False),
    ('FL', 'Florida', False, True),
    ('GA', 'Georgia', False, False),
    ('HI', 'Hawaii', False, False),
    ('ID', 'Idaho', False, False),
    ('IL', 'Illinois', False, True),
    ('IN', 'Indiana', False, True),
    ('IA', 'Iowa', False, False),
    ('KS', 'Kansas', False, False),
    ('KY', 'Kentucky', False, False),
    ('LA', 'Louisiana', False, False),
    ('ME', 'Maine', False, True),
    ('MD', 'Maryland', False, True),
    ('MA', 'Massachusetts', False, True),
    ('MI', 'Michigan', False, False),
    ('MN', 'Minnesota', False, True),
    ('MS', 'Mississippi', False, False),
    ('MO', 'Missouri', False, True),
    ('MT', 'Montana', False, False),
    ('NE', 'Nebraska', False, False),
    ('NV', 'Nevada', False, False),
    ('NH', 'New Hampshire', False, True),
    ('NJ', 'New Jersey', False, True),
    ('NM', 'New Mexico', False, False),
    ('NY', 'New York', False, True),
    ('NC', 'North Carolina', False, True),
    ('ND', 'North Dakota', False, True),
    ('OH', 'Ohio', False, True),
    ('OK', 'Oklahoma', False, False),
    ('OR', 'Oregon', False, False),
    ('PA', 'Pennsylvania', False, True),
    ('RI', 'Rhode Island', False, False),
    ('SC', 'South Carolina', False, False),
    ('SD', 'South Dakota', False, False),
    ('TN', 'Tennessee', False, False),
    ('TX', 'Texas', False, False),
    ('UT', 'Utah', False, False),
    ('VT', 'Vermont', False, False),
    ('VA', 'Virginia', False, False),
    ('WA', 'Washington', False, False),
    ('WV', 'West Virginia', False, False),
    ('WI', 'Wisconsin', False, False),
    ('WY', 'Wyoming', False, False)
]

FPPCI_states = [
    ("", ' ', False, False),
    ('AL', 'Alabama', False, False),
    ('AK', 'Alaska', False, False),
    ('AZ', 'Arizona', False, False),
    ('AR', 'Arkansas', False, False),
    ('CA', 'California', False, False),
    ('CO', 'Colorado', False, False),
    ('CT', 'Connecticut', False, False),
    ('DE', 'Delaware', False, False),
    ('FL', 'Florida', False, False),
    ('GA', 'Georgia', False, False),
    ('HI', 'Hawaii', False, False),
    ('ID', 'Idaho', False, False),
    ('IL', 'Illinois', False, False),
    ('IN', 'Indiana', False, False),
    ('IA', 'Iowa', False, False),
    ('KS', 'Kansas', False, False),
    ('KY', 'Kentucky', False, False),
    ('LA', 'Louisiana', False, False),
    ('ME', 'Maine', False, False),
    ('MD', 'Maryland', False, False),
    ('MA', 'Massachusetts', False, False),
    ('MI', 'Michigan', False, False),
    ('MN', 'Minnesota', False, False),
    ('MS', 'Mississippi', False, False),
    ('MO', 'Missouri', False, False),
    ('MT', 'Montana', False, False),
    ('NE', 'Nebraska', False, False),
    ('NV', 'Nevada', False, False),
    ('NH', 'New Hampshire', False, False),
    ('NJ', 'New Jersey', False, False),
    ('NM', 'New Mexico', False, False),
    ('NY', 'New York', False, False),
    ('NC', 'North Carolina', False, False),
    ('ND', 'North Dakota', False, False),
    ('OH', 'Ohio', False, False),
    ('OK', 'Oklahoma', False, False),
    ('OR', 'Oregon', False, False),
    ('PA', 'Pennsylvania', False, False),
    ('RI', 'Rhode Island', False, False),
    ('SC', 'South Carolina', False, False),
    ('SD', 'South Dakota', False, False),
    ('TN', 'Tennessee', False, False),
    ('TX', 'Texas', False, False),
    ('UT', 'Utah', False, False),
    ('VT', 'Vermont', False, False),
    ('VA', 'Virginia', False, False),
    ('WA', 'Washington', False, False),
    ('WV', 'West Virginia', False, False),
    ('WI', 'Wisconsin', False, False),
    ('WY', 'Wyoming', False, False)
]
    

    


product_choices = [(""," ", False, True),
                   ("FPPTI","Family Protection Plan - Term to 100", False, False),
                   ("FPPCI","Family Protection Plan - with Critical Illness", False, False),
                   ("CIEMP","Group Critical Illness (coming soon)", False, True),
                   ("VGL", "Voluntary Group Life (coming soon)", False, True)
               ]

class EnrollmentSetupForm(Form):
    productID = SelectFieldWithDisable(choices = product_choices)
    enrollmentState = SelectFieldWithDisable(choices = FPPTI_states)
    fppciState = SelectFieldWithDisable(choices = FPPCI_states)
    companyName = StringField('Company Name', validators=[InputRequired()])
    eeFName = StringField('Employee First Name', validators=[InputRequired()])
    eeLName = StringField('Employee Last Name', validators=[InputRequired()])
    email = StringField('Employee email', validators=[Email()])

