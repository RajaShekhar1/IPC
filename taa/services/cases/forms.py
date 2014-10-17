
from flask_wtf import Form
from wtforms.fields import StringField, SelectMultipleField, IntegerField
from wtforms import validators

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
