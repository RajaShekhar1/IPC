import random
from datetime import datetime
import smtplib
from email.mime.multipart import MIMEMultipart
from email.mime.text import MIMEText

from taa import app

from flask import url_for, render_template
from dateutil.relativedelta import relativedelta

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
    
    def send_registration_notice(self, agent_name):
        
        to_user = "zach@zachmason.com" #"admin@5starenroll.com"
        
        msg = MIMEMultipart()
        msg['From'] = "enrollment@5StarEnroll.com"
        msg['To'] = "admin@5StarEnroll.com"
        msg['Subject'] = "Activation Request from " + agent_name
        body = render_template(
            "emails/notify_admin_email.html",
            agent_name=agent_name
        )
        
        msg.attach(MIMEText(body, 'html'))
        
        connection = smtplib.SMTP(app.config['EMAIL_SMTP_SERVER'], app.config['EMAIL_SMTP_PORT'])
        connection.ehlo()
        #if self.smtp_user and self.smtp_password:
        connection.starttls()
        connection.ehlo()
        connection.login(app.config['EMAIL_SMTP_USERNAME'], app.config['EMAIL_SMTP_PASSWORD'])
        
        connection.sendmail(self.smtp_user, to_user, msg.as_string())
        connection.close()
    