from flask import Blueprint, request
from flask_stormpath import user, groups_required

from taa.core import TAAFormError
from taa.api import route
from taa.helpers import get_posted_data
from taa.services.products import ProductService
from taa.services.products.forms import NewProductForm, EditProductForm

bp = Blueprint("products", __name__, url_prefix='/products')
read_product_api_groups = ['agents', 'home_office', 'admins']
write_product_groups = ['home_office', 'admins']

# TODO: Will need to eventually add a group type for people doing self-enroll 
read_product_rate_groups = ['agents', 'home_office', 'admins']

product_service = ProductService()

@route(bp, "/", methods=['GET'])
@groups_required(read_product_api_groups, all=False)
def get_products():
    # TODO: Limit products returned if agent to base products and assigned products
    search_params = dict(
        by_name=request.args.get('by_name'),
        by_type=request.args.get('by_type'),
        by_code=request.args.get('by_code'),
    )
    return product_service.search(**search_params)

@route(bp, "/<product_id>")
@groups_required(read_product_api_groups, all=False)
def get_product(product_id):
    return product_service.get_or_404(product_id)

@route(bp, "/", methods=['POST'])
@groups_required(write_product_groups, all=False)
def create_product():
    data = get_posted_data()
    form = NewProductForm(form_data=data)
    if form.validate_on_submit():
        return product_service.create_custom_product(**data)
    
    raise TAAFormError(form.errors)
 
@route(bp, "/<product_id>", methods=["PUT"])
@groups_required(write_product_groups, all=False)
def update_product(product_id):
    product = product_service.get_if_allowed(product_id)
    data = get_posted_data()
    
    if product.is_base_product():
        # Handle a little differently
        product_service.update_product_restricted_agents(product, **data)
        return product
    
    form = EditProductForm()
    if form.validate_on_submit():
        product_service.update_product_agents(product, **data)
        product_service.update_product_criteria(product, **data)
        product_service.update_product_bypassed_questions(product, **data)
        del data['agents']
        del data['gi_criteria']
        del data['bypassed_questions']
        return product_service.update(product, **data)
    
    raise TAAFormError(form.errors)

@route(bp, "/<product_id>", methods=["DELETE"])
@groups_required(write_product_groups, all=False)
def delete_product(product_id):
    product_service.delete(product_service.edit_if_allowed(product_id))
    return None, 204


# Rates and recommendations for a product given key demographic data
@route(bp, "/<product_id>/rates", methods=['POST'])
@groups_required(read_product_rate_groups, all=False)
def get_product_rates(product_id):
    product = product_service.get_if_allowed(product_id)
    data = get_posted_data()
    
    # Pull parameters from the request
    employee = data['employee']
    spouse = data['spouse']
    num_children = data['num_children']
    
    demographics = dict(
        employee_age=employee['age'],
        employee_smoker=employee['is_smoker'],
        spouse_age=spouse['age'] if spouse else None,
        spouse_smoker=spouse['is_smoker'] if spouse else None,
        num_children=num_children,
    )
    
    # return rates and recommendations
    rates = product_service.get_product_rates(product, demographics)
    
    recommendations = product_service.get_product_recommendations(product, demographics)
    return {
        'employee_rates': rates['employee'],
        'spouse_rates': rates.get('spouse'),
        'children_rates': rates.get('children'),
        'recommendations': recommendations
    }