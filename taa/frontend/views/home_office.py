
from flask import render_template, redirect, url_for, flash
from flask_stormpath import login_required, groups_required

from taa import app
from .nav import get_nav_menu
from taa.services.products import ProductService
from taa.services.products.forms import EditProductForm
from taa.services.agents import AgentService

home_office_groups = ["home_office", "admins"]

product_service = ProductService()
agent_service = AgentService()

@app.route("/home-office")
@groups_required(home_office_groups, all=False)
def home_office_dashboard():
    return render_template('home_office/dashboard.html', nav_menu=get_nav_menu())

@app.route("/manage-products")
@groups_required(home_office_groups, all=False)
def manage_custom_products():
    
    products = product_service.get_base_products() + product_service.get_custom_products()
    return render_template('home_office/manage_products.html', 
        products=products,
        nav_menu=get_nav_menu()
    )

@app.route("/manage-products/<product_id>")
@groups_required(home_office_groups, all=False)
def manage_custom_product(product_id):
    product = product_service.get_or_404(product_id)
    product_form = EditProductForm(obj=product)
    base_product_options = product_service.get_base_products()
    
    available_agents = agent_service.get_active_agents()
    soh_options = product_service.get_soh_labels()
    is_base_product = product.is_base_product()
    is_gi_product = product.is_guaranteed_issue()
    
    cases_using_product = product_service.get_cases_using_product(product)
    
    return render_template('home_office/manage_product.html', 
                           product=product, 
                           product_form=product_form,
                           base_product_options=base_product_options,
                           available_agents=available_agents,
                           statement_of_health_options=soh_options,
                           is_gi_product=is_gi_product,
                           is_base_product=is_base_product,
                           nav_menu=get_nav_menu(),
                           cases_using_product=cases_using_product,
    )
    
@app.route("/manage-agents")
@groups_required(home_office_groups, all=False)
def manage_agents():
    return redirect('admin')
    return render_template('home_office/manage_agents.html', 
                           nav_menu=get_nav_menu())