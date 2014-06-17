"""5Star-specific overrides of Flask-Stormpath classes and functions."""


from wtforms.fields import PasswordField, StringField
from wtforms.validators import InputRequired
from flask.ext.stormpath.forms import RegistrationForm


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
    custom_data = {}


