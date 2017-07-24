"""5Star-specific overrides of Flask-Stormpath classes and functions."""
from flask_wtf import FlaskForm
from flask_wtf.form import _Auto
from wtforms.fields import PasswordField, StringField, BooleanField, SelectMultipleField
from wtforms.validators import InputRequired, Email

from taa.services.products.states import all_states


class TAA_RegistrationForm(FlaskForm):
    """
    Register a new user.  Adds a couple of needed slots to Stormpath default registration

    signing_name: full name used for agent signing account setup (so we don't bother with parsing first/last with Jr., III, etc.
    repassword:  used to re-enter password for confirmation check
    agent_code:  whatever they enter, but will be admin-edited to be the 5-char 5Star agent code
    """
    username = StringField('Username')
    given_name = StringField('First Name')
    middle_name = StringField('Middle Name')
    surname = StringField('Last Name')
    email = StringField('Email', validators=[
        InputRequired('You must provide an email address.'),
        Email('You must provide a valid email address.')
    ])
    password = PasswordField('Password', validators=[InputRequired('You must supply a password.')])
    signing_name = StringField('Signature Name', validators=[InputRequired()])
    agent_code = StringField('Agent Code', validators=[InputRequired()])
    repassword = PasswordField('Repeat Password', validators=[InputRequired()])
    agency = StringField('Agency Name', validators=[InputRequired()])
    custom_data = {}



class TAA_LoginForm(FlaskForm):
    login = StringField('Login', validators=[InputRequired('Login identifier required.')])
    password = PasswordField('Password', validators=[InputRequired('Password required.')])
    allowForgotPass = False


class TAA_UserForm(FlaskForm):
    """
    Form to use for editing existing user accounts from StormPath record.
    Mainly needed in order to edit custom data, including activation.
    """
    fname = StringField('First name')
    lname = StringField('Last name')
    email = StringField('Email')
    signing_name = StringField('Signature Name')
    agent_code = StringField('Agent Code')
    ds_apikey = StringField('Docusign key')
    agency = StringField('Agency Name')
    status = StringField('Status')
    activated = BooleanField('activated')
    send_notice = BooleanField('activated', default = False)
class AdminNewUserForm(FlaskForm):
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
    send_notice = BooleanField('activated', default=False)
    password = PasswordField('Password', validators=[InputRequired('You must supply a password.')])
    repassword = PasswordField('Repeat Password', validators=[InputRequired()])
    is_restricted_to_licensed_states = BooleanField(
        'Restricted to Licensed States', default=False)
    licensed_states = SelectMultipleField(choices=[[s['statecode'], s['name']]
                                                   for s in all_states])