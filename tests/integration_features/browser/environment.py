from selenium import webdriver

from taa import db, app
from taa.manage.InitializeDatabase import init_basic_data
from taa.manage.init_brochures import init_brochures
from taa.manage.docusign2taa import DocusignImportCommand


def before_all(context):
    context.browser = webdriver.Firefox()


def after_all(context):
    context.browser.quit()


def before_scenario(context, scenario):
    # Create all tables
    db.drop_all()
    db.create_all()

    # Add basic data
    init_basic_data()
    init_brochures()


def after_scenario(context, scenario):
    # Clear the session
    db.session.close()
    db.session.remove()
