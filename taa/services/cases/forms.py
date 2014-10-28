
from flask_wtf import Form
from wtforms.fields import (
    StringField, SelectField, SelectMultipleField, 
    IntegerField, RadioField, FieldList,
    FormField, DateField, BooleanField
)
from wtforms.widgets import CheckboxInput
from wtforms import validators

from taa.model.Enrollment import get_all_states
from taa.services.products import ProductService
products_service = ProductService()

class _CommonCaseFormMixin(object):
    company_name = StringField('Company Name', [validators.InputRequired()])
    situs_state = SelectField('State', [validators.Optional(), validators.length(min=2, max=2)])
    situs_city = StringField('City', [validators.Optional()])
    products = SelectMultipleField('Products', [])
    agent_id = IntegerField('Agent', [validators.DataRequired()])
    active = BooleanField('Active')
    
    def __init__(self, *args, **kwargs):
        super(_CommonCaseFormMixin, self).__init__(*args, **kwargs)

        self.products.choices = [(p.code, p.name) for p in products_service.all()]
        self.situs_state.choices = [(s['shortname'], s['name']) for s in products_service.get_all_states()]
    
    def validate_situs_state(self, field):
        if field.data not in products_service.get_all_statecodes():
            raise validators.ValidationError('Invalid State')
        
        # TODO: Validate product-state mismatch
    
class NewCaseForm(_CommonCaseFormMixin, Form):
    pass
    
class UpdateCaseForm(_CommonCaseFormMixin, Form):
    pass



class CensusRecordForm(Form):
    
    employee_first = StringField('Employee First', [validators.InputRequired()])
    employee_last = StringField('Employee Last', [validators.InputRequired()])
    employee_gender = SelectField('Employee Gender', [validators.optional()], choices=[('', ''), ('Male','Male'), ('Female','Female')])
    employee_ssn = StringField('Employee SSN', [validators.InputRequired()])
    employee_birthdate = StringField('Employee Birthdate', [validators.InputRequired()])
    employee_email = StringField('Employee Email', [validators.InputRequired()])
    employee_phone = StringField('Employee Phone')
    employee_street_address = StringField("Employee Street Address")
    employee_street_address2 = StringField("Employee Street Address2")
    employee_city = StringField("Employee Street Address")
    employee_state = SelectField("Employee Statecode", choices=[(s['shortname'], s['name']) for s in get_all_states()])
    employee_zip = StringField("Employee Zip", [validators.length(max=5, min=5)])
    
    spouse_first = StringField('Spouse First')
    spouse_last = StringField('Spouse Last')
    spouse_gender = SelectField('Spouse Gender', [validators.optional()], choices=[('', ''), ('Male','Male'), ('Female','Female')])
    spouse_ssn = StringField('Spouse SSN')
    spouse_birthdate = StringField("Spouse Birthdate")
    spouse_email = StringField('Spouse Email')
    spouse_phone = StringField('Spouse Phone')
    spouse_street_address = StringField("Spouse Street Address")
    spouse_street_address2 = StringField("Spouse Street Address2")
    spouse_city = StringField("Spouse Street Address")
    spouse_state = SelectField("Spouse Statecode", choices=[(s['shortname'], s['name']) for s in get_all_states()])
    spouse_zip = StringField("Spouse Zip", [validators.length(max=5, min=5)])
    
class AnnualPeriodForm(Form):
    period_start_date = StringField('Start')
    period_end_date = StringField('End')
    
class NewCaseEnrollmentPeriodForm(Form):
    enrollment_period_type = RadioField('Period Type', choices=[('open', 'Open Enrollment'), ('annual', 'Annual Periods')])
    
    open_period_start_date = DateField('Start Date', [validators.optional()], format='%m/%d/%Y')
    annual_period_dates = FieldList(FormField(AnnualPeriodForm))
    
    def __init__(self, *args, **kwargs):
        super(NewCaseEnrollmentPeriodForm, self).__init__(*args, **kwargs)
        
        # Default to four entries
        if len(self.annual_period_dates) < 4:
            for x in range(4 - len(self.annual_period_dates)):
                self.annual_period_dates.append_entry()
    
    def validate_open_period_start_date(self, field):
        pass