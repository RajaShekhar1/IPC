from flask_script import Command, Option

import uuid

from ..services.products import ProductService
from ..services.agents import ApiTokenService
from ..models import db

product_service = ProductService()
api_token_service = ApiTokenService()


class InitializeDatabaseCommand(Command):
    """Add all the default products and other default data"""

    option_list = (
        Option('--only', '-o', dest='task', required=False),
    )

    def run(self, task):
        if task == "basic_data":
            init_basic_data()
        elif task == "drop_box":
            init_drop_box()
        else:
            init_basic_data()
            init_drop_box()


def init_drop_box():
<<<<<<< Updated upstream
    if not api_token_service.find(name="DropBox User").count():
        new_token = uuid.uuid4().hex
        api_token_service.create(**dict(
            api_token=new_token,
            name="DropBox User",
            activated=True,
            stormpath_url=""
        ))
        db.session.commit()

=======
    token = api_token_service.create_new_token(name="DropBox User", sp_href="", activated=True)
    print("The Drop Box API token is {}".format(token))
    db.session.commit()
>>>>>>> Stashed changes

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
        #print("Checking {}".format(product['code']))
        if not product_service.find(code=product['code']).first():
            product_service.create(**product)
            #print("Product '{}' created successfully".format(product['code']))
    db.session.commit()
