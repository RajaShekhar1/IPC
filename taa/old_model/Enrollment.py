import random
from datetime import datetime

from flask import url_for, render_template
from dateutil.relativedelta import relativedelta

from taa.services import LookupService
from taa import app


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


class EmailGenerator(object):
    def send(self, recipient, subject, html, from_address=None):
        mailer = LookupService('MailerService')
        if not from_address:
            from_address = app.config.get('EMAIL_FROM_ADDRESS')
        try:
            result = mailer.send_email(
                from_email=from_address,
                subject=subject,
                html=html,
                to=[recipient],
            )
        except mailer.Error as e:
            print u"Error sending email: %s - %s" % (e.__class__, e)
            raise 
        except Exception as e:
            print u"Exception sending email: %s - %s"%(e.__class__, e)


class EnrollmentEmail(object):
    def send_enrollment_request(self, enrollment_request):
        
        to_user = enrollment_request.enrollment.employee_email
        
        subject = "Enrollment Request:  {employee_first} {employee_last} ({company_name}) - {product_name}".format(
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
        
        EmailGenerator().send(to_user, subject, body)
        
class AgentActivationEmail(object):
    """
    This perhaps should go elsewhere, but wanted to send a near-identical email as EnrollmentEmail above.  Probably should abstract the functions, but we can do that in Phase N+1
    """
    def send_activation_notice(self, to_email, agent_name, url):
        
        body = render_template(
            "emails/activation_email.html",
            agent_name=agent_name,
            landing_url=url
        )

        print "url is ", url
        
        subject = "Activation Notice for 5Star Online Enrollment"
        
        EmailGenerator().send(to_email, subject, body)


class NotifyAdminEmail(object):
    """
    This perhaps should go elsewhere
    """
    
    def send_registration_notice(self, agent_name):

        from taa.config_defaults import env_get_text
        recipient = env_get_text(
            'USER_REGISTRATION_NOTICE_EMAIL',
            'enroll-activation-requests-noreply@5starlifeinsurance.com')

        body = render_template(
            "emails/notify_admin_email.html",
            agent_name=agent_name
        )
        
        subject = "Activation Request from " + agent_name

        EmailGenerator().send(recipient, subject, body)