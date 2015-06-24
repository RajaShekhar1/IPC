
from flask_script import Command

from ..services.products import ProductService
from ..models import db

product_service = ProductService()

class InitProductBrochures(Command):
    """Add brochure urls to default / base products"""

    def run(self):

        brochures = [
            dict(product='FPPTI', link='http://5starlifeinsurance.com/wp-content/uploads/2015/02/5Star-Life-FPP-TI-brochure.pdf'),
            dict(product='FPPCI', link='http://5starlifeinsurance.com/wp-content/uploads/2015/02/5Star-Life-FPP-CI-brochure.pdf'),
            dict(product='Group CI', link='http://5starlifeinsurance.com/wp-content/uploads/2015/03/Group-Critical-Illness-Employee-Weekly-Bi-weekly-brochure.pdf'),
            # Gov is same as TI
            dict(product='FPP-Gov', link='http://5starlifeinsurance.com/wp-content/uploads/2015/02/5Star-Life-FPP-TI-brochure.pdf'),
        ]
        for brochure_link in brochures:
            product = product_service.find(code=brochure_link['product']).first()
            if not product:
                print("Could not find product {} to update".format(brochure_link['product']))
            print("Updating product {}".format(brochure_link['product']))
            product.brochure_url = brochure_link['link']

        db.session.commit()