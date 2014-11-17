
from flask import render_template, redirect, url_for, flash
from flask_stormpath import login_required, groups_required

from taa import app
from taa.services.products import ProductService
from taa.services.products.forms import EditProductForm

home_office_groups = ["home_office", "admins"]

product_service = ProductService()


@app.route("/dashboard")
@groups_required(home_office_groups, all=False)
def home_office_dashboard():
    
    return render_template('home_office/dashboard.html')


@app.route("/custom-products")
@groups_required(home_office_groups, all=False)
def manage_custom_products():
    
    custom_products = product_service.get_custom_products()
    return render_template('home_office/custom_products.html', 
        custom_products = custom_products                       
    )

@app.route("/manage-product/<product_id>")
@groups_required(home_office_groups, all=False)
def manage_custom_product(product_id):
    product = product_service.get_or_404(product_id)
    product_form = EditProductForm(obj=product)
    return render_template('home_office/custom_product.html', product=product, product_form=product_form)
    
@app.route("/manage-agents")
@groups_required(home_office_groups, all=False)
def manage_agents():
    return render_template('home_office/manage_agents.html')