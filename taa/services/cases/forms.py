
from flask_wtf import Form
from wtforms.fields import StringField, SelectField, SelectMultipleField, IntegerField
from wtforms import validators

from taa.model.Enrollment import get_all_states
from taa.services.products import ProductService
products_service = ProductService()

class _CommonCaseFormMixin(object):
    company_name = StringField('Company Name', [validators.InputRequired()])
    situs_state = StringField('State', [validators.InputRequired(), validators.length(min=2, max=2)])
    situs_city = StringField('City', [validators.Optional()])
    products = SelectMultipleField('Products', [validators.DataRequired()])
    agent_id = IntegerField('Agent', [validators.DataRequired()])
    
    def __init__(self, *args, **kwargs):
        super(_CommonCaseFormMixin, self).__init__(*args, **kwargs)

        self.products.choices = [(p.code, p.name) for p in products_service.all()]
        print(self.products.choices)
        
    def validate_situs_state(self, field):
        if field.data not in products_service.get_all_statecodes():
            raise validators.ValidationError('Invalid State')
    
class NewCaseForm(_CommonCaseFormMixin, Form):
    pass
    
class UpdateCaseForm(_CommonCaseFormMixin, Form):
    pass



class CensusRecordForm(Form):
    
    employee_first = StringField('Employee First', [validators.InputRequired()])
    employee_last = StringField('Employee Last', [validators.InputRequired()])
    employee_ssn = StringField('Employee SSN', [validators.InputRequired()])
    employee_email = StringField('Employee Email', [validators.InputRequired()])
    
    employee_street_address1 = StringField("Employee Street Address")
    employee_street_address2 = StringField("Employee Street Address")
    employee_city = StringField("Employee Street Address")
    employee_state = SelectField("Employee Statecode", choices=[(s['shortname'], s['name']) for s in get_all_states()])
    employee_zip = StringField("Employee Zip", [validators.length(max=5, min=5)])
    
    spouse_first = StringField('Spouse First')
    spouse_last = StringField('Spouse Last')
    spouse_ssn = StringField('Spouse SSN')
    spouse_email = StringField('Spouse Email')
    