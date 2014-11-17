import random
from datetime import datetime
import smtplib
from email.mime.multipart import MIMEMultipart
from email.mime.text import MIMEText

from flask import url_for, render_template
from dateutil.relativedelta import relativedelta
from flask.ext.wtf import Form
from wtforms.fields import StringField, SelectField
from wtforms.validators import InputRequired, Email



from taa.helpers import SelectFieldWithDisable

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
        msg['Subject'] = "Enrollment Request:  {employee_first} {employee_last} ({product_name}) - {product_name}".format(
            employee_first=enrollment_request.enrollment.employee_first,
            employee_last=enrollment_request.enrollment.employee_last,
            company_name=enrollment_request.enrollment.case.company_name,
            product_name=enrollment_request.enrollment.case.product.name,
        )
        body = render_template(
            "emails/enrollment_email.html",
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
            "emails/activation_email.html",
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
            "emails/notify_admin_email.html",
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
# each set of option values for respective Products is a tuple of 
# value, name, disabled (True/False)) 
#
#[('1', 'VEHICLES',  True), ('2', 'Cars',  False), ('3', 'Motorcycles',  False)]
#
FPPTI_states = [
    ("", ' ', False),
    ('AL', 'Alabama', False),
    ('AK', 'Alaska', False),
    ('AZ', 'Arizona', False),
    ('AR', 'Arkansas',  False),
    ('CA', 'California',  False),
    ('CO', 'Colorado',  False),
    ('CT', 'Connecticut',  True),
    ('DE', 'Delaware',  False),
    ('DC', 'District of Columbia',  True),
    ('FL', 'Florida',  False),
    ('GA', 'Georgia',  False),
    ('HI', 'Hawaii',  False),
    ('ID', 'Idaho',  False),
    ('IL', 'Illinois',  False),
    ('IN', 'Indiana',  True),
    ('IA', 'Iowa',  False),
    ('KS', 'Kansas',  False),
    ('KY', 'Kentucky',  False),
    ('LA', 'Louisiana',  False),
    ('ME', 'Maine',  True),
    ('MD', 'Maryland',  True),
    ('MA', 'Massachusetts',  True),
    ('MI', 'Michigan',  False),
    ('MN', 'Minnesota',  True),
    ('MS', 'Mississippi',  False),
    ('MO', 'Missouri',  False),
    ('MT', 'Montana',  False),
    ('NE', 'Nebraska',  False),
    ('NV', 'Nevada',  False),
    ('NH', 'New Hampshire',  True),
    ('NJ', 'New Jersey',  True),
    ('NM', 'New Mexico',  False),
    ('NY', 'New York',  True),
    ('NC', 'North Carolina',  True),
    ('ND', 'North Dakota',  True),
    ('OH', 'Ohio',  False),
    ('OK', 'Oklahoma',  False),
    ('OR', 'Oregon',  False),
    ('PA', 'Pennsylvania',  False),
    ('RI', 'Rhode Island',  False),
    ('SC', 'South Carolina',  False),
    ('SD', 'South Dakota',  False),
    ('TN', 'Tennessee',  False),
    ('TX', 'Texas',  False),
    ('UT', 'Utah',  False),
    ('VT', 'Vermont',  True),
    ('VA', 'Virginia',  False),
    ('WA', 'Washington',  True),
    ('WV', 'West Virginia',  False),
    ('WI', 'Wisconsin',  False),
    ('WY', 'Wyoming',  False)
]

FPPCI_states = [
    ("", ' ',  False),
    ('AL', 'Alabama',  False),
    ('AK', 'Alaska',  False),
    ('AZ', 'Arizona',  False),
    ('AR', 'Arkansas',  False),
    ('CA', 'California',  False),
    ('CO', 'Colorado',  False),
    ('CT', 'Connecticut',  True),
    ('DE', 'Delaware',  False),
    ('DC', 'District of Columbia',  True),
    ('FL', 'Florida',  False),
    ('GA', 'Georgia',  False),
    ('HI', 'Hawaii',  False),
    ('ID', 'Idaho',  False),
    ('IL', 'Illinois',  False),
    ('IN', 'Indiana',  False),
    ('IA', 'Iowa',  False),
    ('KS', 'Kansas',  False),
    ('KY', 'Kentucky',  False),
    ('LA', 'Louisiana',  False),
    ('ME', 'Maine',  True),
    ('MD', 'Maryland',  True),
    ('MA', 'Massachusetts',  True),
    ('MI', 'Michigan',  False),
    ('MN', 'Minnesota',  True),
    ('MS', 'Mississippi',  False),
    ('MO', 'Missouri',  False),
    ('MT', 'Montana',  False),
    ('NE', 'Nebraska',  False),
    ('NV', 'Nevada',  False),
    ('NH', 'New Hampshire',  True),
    ('NJ', 'New Jersey',  True),
    ('NM', 'New Mexico',  False),
    ('NY', 'New York',  True),
    ('NC', 'North Carolina',  True),
    ('ND', 'North Dakota',  True),
    ('OH', 'Ohio',  False),
    ('OK', 'Oklahoma',  False),
    ('OR', 'Oregon',  False),
    ('PA', 'Pennsylvania',  True),
    ('PR', 'Puerto Rico',  True),
    ('RI', 'Rhode Island',  False),
    ('SC', 'South Carolina',  False),
    ('SD', 'South Dakota',  False),
    ('TN', 'Tennessee',  False),
    ('TX', 'Texas',  False),
    ('UT', 'Utah',  False),
    ('VT', 'Vermont',  True),
    ('VA', 'Virginia',  False),
    ('VI', 'Virgin Islands',  False),
    ('WA', 'Washington',  False),
    ('WV', 'West Virginia',  False),
    ('WI', 'Wisconsin',  False),
    ('WY', 'Wyoming',  False)
]
    
def get_all_states():
    return [
        dict(
            shortname=s[0],
            name=s[1]
        ) for s in FPPTI_states
    ]
    


product_choices = [#(""," ",  True),
                   ("FPPTI","Family Protection Plan - Term to 100",  False),
                   ("FPPCI","Family Protection Plan - with Critical Illness",  False),
                   ("CIEMP","Group Critical Illness (coming soon)",  True),
                   ("VGL", "Voluntary Group Life (coming soon)",  True)
               ]

class EnrollmentSetupForm(Form):
    productID = SelectFieldWithDisable(choices = product_choices)
    
    # Include the state drop-down dynamically since it depends on the product
    enrollmentCity = StringField('Enrollment City', validators=[InputRequired()])
    companyName = StringField('Company Name', validators=[InputRequired()])
    eeFName = StringField('Employee First Name', validators=[InputRequired()])
    eeLName = StringField('Employee Last Name', validators=[InputRequired()])
    email = StringField('Employee email', validators=[Email()])
    

def get_enrollment_setup_form_for_product(productID=None):
    """
    Create the setup form based on the product code (some states have no )
    Returns a WTF form class, so you can still instantiate it like normal
    """
    
    # Define a new form that inherits all the fields from the base form
    class EnrollmentSetupFormDynamic(EnrollmentSetupForm):
        pass

    product_states = get_product_states().get(productID, FPPTI_states)
    
    # Add the state drop-down
    EnrollmentSetupFormDynamic.enrollmentState = SelectFieldWithDisable(choices=product_states)
    
    return EnrollmentSetupFormDynamic
    
def get_product_states():
    """Return the mapping of product codes to enabled states (statecode, state name, is_disabled) """
    
    return {
        'FPPTI':FPPTI_states,
        'FPPCI':FPPCI_states,
    }
    
     
def get_product_choices():
    return [dict(
        short_name=p[0],
        name=p[1],
        disabled=p[2],
    ) for p in product_choices]
