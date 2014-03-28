from flask.ext.wtf import Form
from wtforms import TextField, TextAreaField, BooleanField, RadioField
from wtforms.validators import Required, Email

class LoginForm(Form):
    openid = TextField('openid', validators = [Required()])
    remember_me = BooleanField('remember_me', default = False)


class UserEmailForm(Form):
    full_name = TextField('full_name', validators = [Required()])
    employer = TextField('employer', validators = [Required()])
    email_addr = TextField('email_addr', validators = [Required(), Email()])
    email_comments = TextAreaField('email_comments')


class UserDirectForm(Form):
    full_name = TextField('full_name', validators = [Required()])
    employer = TextField('employer', validators = [Required()])
    email_addr = TextField('email_addr', validators = [Email()])
    idNum = TextField('idNum', validators = [Required()])
    idType = RadioField('idType', choices=[('dl','DL #'),('employee','Employee #')], validators = [Required()])
  
    
    
