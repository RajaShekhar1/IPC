
from taa.core import DBService

from models import Case

class CaseService(DBService):
    
    __model__ = Case

    def _preprocess_params(self, kwargs):
        kwargs = super(CaseService, self)._preprocess_params(kwargs)
        
        products = kwargs.get('products', [])
        if products and all(isinstance(p, unicode) for p in products):
            from taa.services.products import ProductService
            products_service = ProductService()
            kwargs['products'] = products_service.get_products_by_codes(products)
        
        return kwargs
    