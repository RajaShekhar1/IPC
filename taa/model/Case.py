

class Case(object):
    def __init__(self, id, company_name, statecode, products, is_active=True):
        self.id = id
        self.company_name = company_name
        self.situs_state = statecode
        self.products = products
        
        self.active = is_active
        
    def get_template_data(self):
        return dict(
            id=self.id, 
            company=self.company_name, 
            state=self.situs_state, 
            product=self.products[0].name if self.products else ""
        )
