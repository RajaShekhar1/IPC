import random
from datetime import datetime

from dateutil.relativedelta import relativedelta
from mailer import Mailer, Message

class Case(object):
    def __init__(self, db, company_name):
        self.db = db
        
        self.company_name = company_name
    
class Enrollment(object):
    def __init__(self, db, case, employee_first, employee_last, employee_email):
        self.db = db
        
        self.case = case
        self.employee_first = employee_first
        self.employee_last = employee_last
        self.employee_email = employee_email

    @classmethod
    def lookup_enrollment(cls, db, token):
        pass
    
    def generate_enrollment_request(self):
        
        request = EnrollmentRequest(
            enrollment=self,
            expiration_date=self.get_link_expiration_date(),
            token=self.generate_token()
        )
        self.db.save_enrollment_request(request)
        
        return request
        
    def generate_token(self, token_length=12):
        chars = "ABCDEF0123456789"
        return ''.join([random.choice(chars) for i in range(token_length)])
    
    def get_link_expiration_date(self):
        # three days from this morning at midnight
        return datetime.today() + relativedelta(days=3)
            
class EnrollmentRequest(object):
    def __init__(self, enrollment, expiration_date, token):
        self.enrollment = enrollment
        self.expiration_date = expiration_date
        self.token = token
        
class EnrollmentEmail(object):
    def __init__(self, smtp_server, smtp_port, smtp_user, smtp_password, from_address):
        self.smtp_server = smtp_server
        self.smtp_port = smtp_port
        self.smtp_user = smtp_user
        self.smtp_password = smtp_password
        self.from_address = from_address
        
    def send_enrollment_request(self, enrollment):
        
        enrollment_request = enrollment.generate_enrollment_request()
        
        message = Message(From=self.from_address,
                          To=enrollment.employee_email,
                          charset="utf-8")
        
        message.Subject = "An HTML Email"
        message.Html = """This email uses <strong>HTML</strong>!"""
        #message.Body = """This is alternate text for clients that don't do HTML."""
        
        sender = Mailer(self.smtp_server, port=self.smtp_port)
        if self.smtp_user and self.smtp_password:
            sender.login(self.smtp_user, self.smtp_password)
        
        sender.send(message)
        