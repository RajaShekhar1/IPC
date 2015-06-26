
from flask_wtf import Form
from wtforms.fields import (
    BooleanField, DateField, FieldList, FileField, FormField, HiddenField,
    IntegerField, RadioField, SelectField, SelectMultipleField, StringField,
    TextAreaField,
)
from wtforms.widgets import CheckboxInput, html_params, HTMLString
from wtforms import validators

from taa.services.cases.models import SelfEnrollmentSetup
from taa.services.products import ProductService, get_all_states
products_service = ProductService()


class ProductMultiSelectField(SelectMultipleField):
    def validate(self, form, extra_validators=()):
        """Override the default validation so we don't use choices here"""
        # TODO: Validate this is a product this user is allowed to enroll
        return True


class _CommonCaseFormMixin(object):
    company_name = StringField('Company Name', [validators.InputRequired()])
    situs_state = SelectField('State', [validators.Optional(),
                                        validators.length(min=2, max=2)])
    situs_city = StringField('City', [validators.Optional()])
    products = ProductMultiSelectField('Products', [])
    agent_id = IntegerField('Agent', [validators.Optional()])
    active = BooleanField('Active')
    is_self_enrollment = BooleanField('Self-enrollment')

    def __init__(self, *args, **kwargs):
        super(_CommonCaseFormMixin, self).__init__(*args, **kwargs)
        # self.products.choices = [(p.id, p.name) for p in products_service.all()]
        self.products.choices = []
        self.situs_state.choices = [(s['statecode'], s['name'])
                                    for s in products_service.get_all_states()]

    def validate_situs_state(self, field):
        if field.data not in products_service.get_all_statecodes():
            raise validators.ValidationError('Invalid State')
        # TODO: Validate product-state mismatch

    def validate_products(self, field):
        pass


class NewCaseForm(_CommonCaseFormMixin, Form):
    pass


class UpdateCaseForm(_CommonCaseFormMixin, Form):
    pass


class SSNField(StringField):
    def process_formdata(self, valuelist):
        self.data = ''
        if valuelist:
            for char in valuelist[0]:
                if char.isdigit():
                    self.data += char

class Editable(object):
    def __call__(self, field, **kwargs):
        kwargs.setdefault('id', field.id)
        # kwargs.setdefault('onsubmit', 'return false;')
        element = HTMLString(u'<div contenteditable="true" %s>%s</div>' %
        (html_params(**kwargs), unicode(field._value())))
        hidden = HTMLString(u'<input type="hidden" id="{id}-value" '
                            u'name="{name}" onclick="document.getElementById('
                            u'\'{id}-value\').value = document.getElementById('
                            u'\'{id}\').innerHTML;">'.format(name=field.name,
                                                             id=field.id))
        return element + hidden


class EditableField(StringField):
    widget = Editable()


class CensusRecordForm(Form):
    employee_first = StringField('Employee First', [validators.InputRequired()])
    employee_last = StringField('Employee Last', [validators.InputRequired()])
    employee_gender = SelectField('Employee Gender', [validators.optional()],
                                  choices=[('', ''), ('male', 'Male'),
                                           ('female', 'Female')])
    employee_ssn = SSNField('Employee SSN', [validators.InputRequired()])
    employee_birthdate = DateField('Employee Birthdate',
                                   [validators.InputRequired()])
    employee_email = StringField('Employee Email', [validators.optional()])
    employee_phone = StringField('Employee Phone')
    employee_street_address = StringField('Employee Street Address')
    employee_street_address2 = StringField('Employee Street Address2')
    employee_city = StringField('Employee Street Address')
    employee_state = SelectField('Employee Statecode',
                                 choices=[(s['statecode'], s['name'])
                                          for s in get_all_states()])
    employee_zip = StringField('Employee Zip', [])
    employee_height_inches = StringField('Employee Height in Inches')
    employee_weight_lbs = StringField('Employee Weight in Lbs')
    employee_smoker = SelectField('Employee is Smoker',
                                  choices=[('', ''), ('N', 'No'), ('Y', 'Yes')])

    spouse_first = StringField('Spouse First')
    spouse_last = StringField('Spouse Last')
    spouse_gender = SelectField('Spouse Gender', [validators.optional()],
                                choices=[('', ''), ('male', 'Male'),
                                         ('female', 'Female')])
    spouse_ssn = SSNField('Spouse SSN')
    spouse_birthdate = DateField('Spouse Birthdate')
    spouse_email = StringField('Spouse Email')
    spouse_phone = StringField('Spouse Phone')
    spouse_street_address = StringField('Spouse Street Address')
    spouse_street_address2 = StringField('Spouse Street Address2')
    spouse_city = StringField('Spouse Street Address')
    spouse_state = SelectField('Spouse Statecode',
                               choices=[(s['statecode'], s['name'])
                                        for s in get_all_states()])
    spouse_zip = StringField('Spouse Zip', [])
    spouse_height_inches = StringField('Spouse Height in Inches')
    spouse_weight_lbs = StringField('Spouse Weight in Lbs')
    spouse_smoker = SelectField('Spouse is Smoker',
                                choices=[('', ''), ('N', 'No'), ('Y', 'Yes')])


for x in range(1, 6+1):
    setattr(CensusRecordForm, 'child{}_first'.format(x),
            StringField('Child {} First'.format(x)))
    setattr(CensusRecordForm, 'child{}_last'.format(x),
            StringField('Child {} Last'.format(x)))
    setattr(CensusRecordForm, 'child{}_birthdate'.format(x),
            DateField('Child {} Birthdate'.format(x), [validators.optional()],
                      default=None))


class AnnualPeriodForm(Form):
    period_start_date = StringField('Start')
    period_end_date = StringField('End')


class NewCaseEnrollmentPeriodForm(Form):
    enrollment_period_type = RadioField('Period Type',
                                        choices=[('open', 'Open Enrollment'),
                                                 ('annual', 'Annual Periods')])
    open_period_start_date = DateField('Start Date', [validators.optional()],
                                       format='%m/%d/%Y')
    open_period_end_date = DateField('End Date', [validators.optional()],
                                     format='%m/%d/%Y')
    annual_period_dates = FieldList(FormField(AnnualPeriodForm))

    def __init__(self, *args, **kwargs):
        super(NewCaseEnrollmentPeriodForm, self).__init__(*args, **kwargs)
        # Default to four entries
        if len(self.annual_period_dates) < 4:
            for x in range(4 - len(self.annual_period_dates)):
                self.annual_period_dates.append_entry()

    def validate_open_period_start_date(self, field):
        pass

    def validate_open_period_end_date(self, field):
        pass


class SelfEnrollmentSetupForm(Form):
    CASE_TARGETED = ('case-targeted', 'Targeted (specific for each person)')
    CASE_GENERIC = ('case-generic', 'Generic only (same link for everyone)')
    self_enrollment_type = SelectField('Link type', choices=[CASE_GENERIC, CASE_TARGETED])
    use_email = BooleanField('Enabled')
    email_sender_name = StringField('Targeted Sender Name',
                                    [validators.InputRequired()])
    email_sender_email = StringField('Targeted Sender Email',
                                     [validators.InputRequired(),
                                      validators.Email()])
    email_greeting_salutation = StringField('Targeted Email Greeting', [])
    email_greeting_type = SelectField('', choices=[
        (SelfEnrollmentSetup.EMAIL_GREETING_FIRST_NAME, "First Name (\"John\")"),
        (SelfEnrollmentSetup.EMAIL_GREETING_FULL_NAME, "First & Last Name (\"John Doe\")"),
        (SelfEnrollmentSetup.EMAIL_GREETING_LAST_NAME, "Last Name (\"Doe\")"),
        (SelfEnrollmentSetup.EMAIL_GREETING_TITLE_LAST, "Title & Last Name (\"Mr./Ms. Doe\")"),
        (SelfEnrollmentSetup.EMAIL_GREETING_BLANK, "Leave blank"),
    ])
    email_subject = StringField('Targeted Email Subject',
                                     [validators.InputRequired()])

    email_message = EditableField('Targeted Email Message', [validators.InputRequired()])
    use_landing_page = BooleanField('Enabled')
    page_title = StringField('Title', [validators.InputRequired()])
    page_text = EditableField('Message', [validators.InputRequired()])

