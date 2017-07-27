from flask import current_app
from flask_script import Command, Option

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
            init_basic_data(current_app)
        elif task == "drop_box":
            init_drop_box()
        else:
            init_basic_data(current_app)
            init_drop_box()


def init_drop_box():
    if not api_token_service.find(name=u"DropBox User").count():
        token = api_token_service.create_new_token(name=u"DropBox User", sp_href=u"", activated=True)
        print("The Drop Box API token is {}".format(token))
        db.session.commit()


def get_drop_box_token():
    user = api_token_service.find(name=u"DropBox User").first()
    if user:
        return user.api_token
    else:
        return None


class GetDropBoxTokenCommand(Command):
    """
    Retrieve the API Token for the DropBox to use when sending files to the application server.
    """
    def run(self):
        print(get_drop_box_token())


def init_basic_data(app):
    if app.config['IS_5STAR']:
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
                can_override_states=True
            ),
            dict(
                code=u"FPP-Gov",
                name=u"FPP-Gov",
                product_type=u"base",
                visible_to_agents=False,
                is_fpp_gov=True,
            ),
            dict(
                code=u"FPPTIW",
                name=u"FPP-White",
                product_type=u"base",
                visible_to_agents=False,
                is_fpp_gov=True,
            ),
            dict(
                code=u"FPPTIB",
                name=u"FPP-Blue",
                product_type=u"base",
                visible_to_agents=False,
                is_fpp_gov=True,
            ),
            dict(
                code=u"FPPTIY",
                name=u"FPP-Gray",
                product_type=u"base",
                visible_to_agents=False,
                is_fpp_gov=True,
            ),
            dict(
                code=u"ACC",
                name=u"Accident Insurance Plan",
                product_type=u"base",
                visible_to_agents=True,
                is_fpp_gov=False,
            ),
            dict(
                code=u"HI",
                name=u"Family Healthcare Indemnity Plan",
                product_type=u"base",
                visible_to_agents=True,
                is_fpp_gov=False,
            ),
            dict(
                code=u'Static Benefit',
                name=u'Single-Fee Generic Product',
                product_type=u'base',
                visible_to_agents=False,
                is_fpp_gov=False,
            )
        ]
    elif app.config['IS_AFBA']:
        product_data = [
            dict(
                    code=u'Member 2K',
                    name=u"Application - Non-Renewable $2,000",
                    customer_short_name=u"Application - Non-Renewable $2,000",
                    product_type=u"base",
                    visible_to_agents=True,
            ),
            dict(
                    code=u'Member 5K',
                    name=u"Application - Non-Renewable $5,000",
                    customer_short_name=u"Application - Non-Renewable $5,000",
                    product_type=u"base",
                    visible_to_agents=True,
            ),
            dict(
                    code=u'FedTerm',
                    name=u"FedTerm",
                    customer_short_name=u"Fed Term",
                    product_type=u"base",
                    visible_to_agents=True,
                    brochure_url="https://www.afba.com/Portals/0/FedProtect.pdf",
                    brochure_name="Government Protect Group Level Term",
            ),
            dict(
                    code=u'ESP',
                    name=u"First Protect",
                    customer_short_name=u"First Protect",
                    product_type=u"base",
                    visible_to_agents=True,
                    brochure_url=u"https://www.afba.com/Portals/0/FirstProtect.pdf",
                    brochure_name=u'First Protect',
            ),
            dict(
                    code=u'Child LT16',
                    name=u"Child Protect",
                    customer_short_name=u"Children's Protect",
                    product_type=u"base",
                    visible_to_agents=True,
                    brochure_url=u"https://www.afba.com/Portals/0/ChildProtect.pdf",
                    brochure_name=u"Child Protect",
            ),
            dict(
                    code=u'LT121',
                    name=u"Senior Protect",
                    customer_short_name=u"Senior Protect",
                    product_type=u"base",
                    visible_to_agents=True,
                    brochure_url=u"https://www.afba.com/Portals/0/SeniorProtect.pdf",
                    brochure_name=u'Senior Protect',
            ),

        ]
    else:
        product_data = []
    if len(product_data) == 0:
        print("WARNING: No products in target platform")
        print(app.config)
    for product in product_data:
        #print("Checking {}".format(product['code']))
        if not product_service.find(code=product['code']).first():
            product_service.create(**product)
            #print("Product '{}' created successfully".format(product['code']))
        else:
            p = product_service.find(code=product['code']).first()
            product_service.update(p, **product)

    db.session.commit()
