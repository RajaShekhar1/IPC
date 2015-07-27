from flask import current_app
from flask_script import Command, prompt, prompt_pass
from werkzeug.datastructures import MultiDict

from ..services.products import ProductService
from ..models import db

product_service = ProductService()


class InitializeDatabaseCommand(Command):
    """Add all the default products and other default data"""

    def run(self):
        init_basic_data()


def init_basic_data():
    product_data = [
        dict(
            code=u"FPPTI",
            name=u"Family Protection Plan - Terminal Illness",
            product_type=u"base",
            visible_to_agents=True,
        ),
        dict(
            code=u"FPPCI",
            name=u"Family Protection Plan - Critical Illness",
            product_type=u"base",
            visible_to_agents=True,
        ),
        dict(
            code=u"Group CI",
            name=u"Group Critical Illness",
            product_type=u"base",
            visible_to_agents=True,
        ),
        dict(
            code=u"FPP-Gov",
            name=u"FPP-Gov",
            product_type=u"base",
            visible_to_agents=False,
        ),
    ]
    for product in product_data:
        print("Checking {}".format(product['code']))
        if not product_service.find(code=product['code']).first():
            product_service.create(**product)
            print("Product '{}' created successfully".format(product['code']))
    db.session.commit()
