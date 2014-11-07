
from flask import current_app
from flask_script import Command, prompt, prompt_pass
from werkzeug.datastructures import MultiDict

from ..services.products import ProductService

product_service = ProductService()

class InitializeDatabaseCommand(Command):
    """Add all the default products and other default data"""

    def run(self):
        
        product_data = [
            dict(
                code="FPPTI", 
                name="Family Protection Plan - Terminal Illness",
            ),
            dict(
                code="FPPCI",
                name="Family Protection Plan - Critical Illness",
            )
        ]
        for product in product_data:
            print("Checking {}".format(product['code']))
            if not product_service.find(code=product['code']).first():
                product_service.create(**product)
                print("Product '{}' created successfully".format(product['code']))
        