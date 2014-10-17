from flask import Blueprint, request
from flask_stormpath import user, groups_required

from taa.core import TAAFormError
from taa.api import route
from taa.services.products import ProductService
from taa.services.cases.forms import NewCaseForm, UpdateCaseForm

bp = Blueprint("products", __name__, url_prefix='/products')

product_service = ProductService()

@route(bp, "/", methods=['GET'])
@groups_required(['agents', 'admins'], all=False)
def get_products():
    return product_service.all()

@route(bp, "/<product_id>")
@groups_required(['agents', 'admins'], all=False)
def get_product(product_id):
    return product_service.get_or_404(product_id)
