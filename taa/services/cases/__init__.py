
from flask import abort
from flask_stormpath import current_user

from taa.core import DBService

from models import Case, CaseCensus

class CaseService(DBService):
    
    __model__ = Case
    
    def __init__(self, *args, **kwargs):
        super(CaseService, self).__init__(*args, **kwargs)
        
        self.census_records = CensusRecordService()

    def _preprocess_params(self, kwargs):
        kwargs = super(CaseService, self)._preprocess_params(kwargs)
        
        products = kwargs.get('products', [])
        if products and all(isinstance(p, unicode) for p in products):
            from taa.services.products import ProductService
            products_service = ProductService()
            kwargs['products'] = products_service.get_products_by_codes(products)
        
        return kwargs
    
    def get_if_allowed(self, case_id):
        
        case = self.get_or_404(case_id)

        from taa.services.agents import AgentService
        
        agent_service = AgentService()
        if agent_service.is_user_admin(current_user):
            return case
        elif agent_service.is_user_agent(current_user):
            agent = agent_service.get_agent_from_user(current_user)
            
            if self.agent_can_view_case(agent, case):
                return case
        
        abort(401)
        
    def get_agent_cases(self, agent):
        # TODO: account for sub-agents
        return self.find(agent_id=agent.id).all()
    
    def agent_can_view_case(self, agent, case):
        # TODO: account for sub-agents
        return case.agent_id == agent.id
    
    def get_census_records(self, case):
        return self.census_records.find(case_id=case.id).all()

    
    # Census records
    
    def get_census_record(self, case, census_record_id):
        record = self.census_records.query(
                    ).filter_by(id=census_record_id
                    ).filter_by(case_id=case.id
                    ).first()
        if not record:
            abort(404)
        
        return record
    
    def merge_census_data(self, case, records):
        return self.census_records.merge_census_data(case, records)
        
    def replace_census_data(self, case, records):
        return self.census_records.replace_census_data(case, records)
    
    def update_census_record(self, record, data):
        return self.census_records.update(record, **data)
    
    
class CensusRecordService(DBService):
    __model__ = CaseCensus

    CSV_FIELD_FORMAT = ["EMP_SSN", "EMP_FIRST", "EMP_LAST", "EMP_BIRTHDATE", "EMP_EMAIL",
                        "SP_SSN", "SP_FIRST", "SP_LAST", "SP_BIRTHDATE", ]
    CSV_FIELD_MAPPING = {
        'EMP_SSN': ('employee_ssn', lambda x: x.replace('-', '')),
        'EMP_FIRST':'employee_first',
        'EMP_LAST': 'employee_last',
        'EMP_BIRTHDATE': 'employee_birthdate',
        'EMP_EMAIL': 'employee_email',
        'EMP_ADDRESS1': 'employee_street_address',
        'EMP_ADDRESS2': 'employee_street_address2',
        'EMP_CITY': 'employee_city',
        'EMP_STATE': 'employee_state',
        'EMP_ZIP': ('employee_zip', lambda x: x.replace('-', '')[:5]),
        
        'SP_SSN': ('spouse_ssn', lambda x: x.replace('-', '')),
        'SP_FIRST': 'spouse_first',
        'SP_LAST': 'spouse_last',
        'SP_BIRTHDATE': 'spouse_birthdate',
        'SP_EMAIL': 'spouse_email',
    }
    def merge_census_data(self, case, records):
        # TODO: Implement
        return []

    def replace_census_data(self, case, records):
        self.find(case_id=case.id).delete()
        
        return [self.create(**self.parse_census_record(case, record)) for record in records]
        
    def parse_census_record(self, case, record):
        
        data = {'case_id': case.id}
        
        for field in record:
            if field in self.CSV_FIELD_MAPPING:
                conv = self.CSV_FIELD_MAPPING[field]
                if isinstance(conv, str):
                    data[conv] = record[field] if record[field] else None
                else:
                    record_field_name, conv = conv
                    data[record_field_name] = conv(record[field]) if conv(record[field]) else None
        
        # TODO: verification of data present (SSN, email, names, birthdates)
        
        return data