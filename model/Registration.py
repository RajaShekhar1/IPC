"""5Star-specific overrides of Flask-Stormpath classes and functions."""

from flask.ext.wtf import Form
from wtforms.fields import PasswordField, StringField, BooleanField
from wtforms.validators import InputRequired
from flask.ext.stormpath.forms import RegistrationForm, LoginForm


class TAA_RegistrationForm(RegistrationForm):
    """
    Register a new user.  Adds a couple of needed slots to Stormpath default registration
    
    signing_name: full name used for agent signing account setup (so we don't bother with parsing first/last with Jr., III, etc.
    repassword:  used to re-enter password for confirmation check
    agent_code:  whatever they enter, but will be admin-edited to be the 5-char 5Star agent code
    """
    signing_name = StringField('Signature Name', validators=[InputRequired()])
    agent_code = StringField('Agent Code', validators=[InputRequired()])
    repassword = PasswordField('Repeat Password', validators=[InputRequired()])
    agency = StringField('Agency Name', validators=[InputRequired()])
    custom_data = {}



class TAA_LoginForm (LoginForm):
    allowForgotPass = False
    

class TAA_UserForm(Form):
    """
    Form to use for editing existing user accounts from StormPath record.
    Mainly needed in order to edit custom data, including activation.
    """
    fname = StringField('First name')
    lname = StringField('Last name')
    email = StringField('Email')
    signing_name = StringField('Signature Name')
    agent_code = StringField('Agent Code')
    agency = StringField('Agency Name')
    status = StringField('Status')
    activated = BooleanField('activated')
    send_notice = BooleanField('activated', default = False)
    

