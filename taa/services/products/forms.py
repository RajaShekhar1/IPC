import locale
locale.setlocale( locale.LC_ALL, '' )

from flask_wtf import Form
from wtforms.fields import (
    StringField, SelectField, SelectMultipleField, 
    IntegerField, RadioField, FieldList,
    FormField, DateField, BooleanField,
)
from wtforms.widgets import CheckboxInput
from wtforms import validators, ValidationError

from taa.services.products import ProductService
from taa.services.products.models import Product
products_service = ProductService()

class NewProductForm(Form):
    product_name = StringField('Custom Product Name', [validators.InputRequired()])

class EditProductForm(Form):
    name = StringField('Custom Product Name', [validators.InputRequired()])
    code = StringField('Product Code', [])
    base_product_id = SelectField('Base Product', [], coerce=int, default=-1)
    guarantee_issue_amount = SelectField('Guarantee Issue Amount', 
                                         choices=[(-1,'')]+[(i, locale.currency(i, grouping=True)) for i in range(1000, 100000+1, 1000)], default=-1, coerce=int)
    criteria_age_min = SelectField('Minimum:',
                                   choices=[(-1, "N/A")] + [(i, "%s" % i) for i in range(111)], default=-1, coerce=int)
    criteria_age_max = SelectField('Maximum:',
                                   choices=[(-1, "N/A")] + [(i, "%s" % i) for i in range(111)], default=-1, coerce=int)
    criteria_height_min = SelectField('Minimum:',
                                      choices=[(-1, "N/A")]+[(i, "%s\""%i) for i in range(1, 101)], default=-1, coerce=int)
    criteria_height_max = SelectField('Maximum:', choices=[(-1, "N/A")]+[(i, "%s\""%i) for i in range(1, 101)], default=-1, coerce=int)
    criteria_weight_min = SelectField('Minimum:',
                                      choices=[(-1, "N/A")] + [(i, "%s Lbs." % i) for i in range(50, 351)], default=-1, coerce=int)
    criteria_weight_max = SelectField('Maximum:',
                                      choices=[(-1, "N/A")] + [(i, "%s Lbs." % i) for i in range(50, 351)], default=-1, coerce=int)
    statement_of_health_bypass_type = StringField(validators=[validators.length(max=32)])
    
    def __init__(self, *args, **kwargs):
        super(EditProductForm, self).__init__(*args, **kwargs)
        
        self.base_product_id.choices = [(-1,'')] + [(p.id, p.name) for p in products_service.get_base_products()]

    def validate_base_product_id(self, field):
        if not field.data:
            raise ValidationError('Base Product Required')
        
        product_id = int(field.data)
        product = products_service.get(product_id)
        if not product or not product.product_type == products_service.BASE_PRODUCT_TYPE:
            raise ValidationError('Invalid Base Product: {0}'.format(product))
        