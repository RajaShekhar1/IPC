
from flask import render_template, redirect, url_for, flash
from flask_stormpath import login_required, groups_required

from taa import app
from taa.services.products import ProductService
from taa.services.products.forms import EditProductForm
from taa.services.agents import AgentService

home_office_groups = ["home_office", "admins"]

product_service = ProductService()
agent_service = AgentService()

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
    base_product_options = product_service.get_base_products()
    # TODO: is all agents right for now?
    available_agents = agent_service.all()
    soh_options = product_service.get_soh_labels()
    
    return render_template('home_office/custom_product.html', 
                           product=product, 
                           product_form=product_form,
                           base_product_options=base_product_options,
                           available_agents=available_agents,
                           statement_of_health_options=soh_options,
    )
    
@app.route("/manage-agents")
@groups_required(home_office_groups, all=False)
def manage_agents():
    return render_template('home_office/manage_agents.html')