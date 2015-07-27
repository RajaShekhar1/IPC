import datetime

from taa.core import db
from taa.services import LookupService
from taa.services.cases.models import Case, CaseOpenEnrollmentPeriod
from taa.services.products import payment_modes

# App services
case_service = LookupService('CaseService')
product_service = LookupService('ProductService')
agent_service = LookupService('AgentService')
api_token_service = LookupService('ApiTokenService')

# These will be in the database for two test agents
test_agent_stormpath_url = 'https://api.stormpath.com/v1/accounts/2qQtvZLi6tpUGjXFYtVSRK'
test_agent_stormpath_url2 = 'https://api.stormpath.com/v1/accounts/3vwaoifZRl8Z1pHIzdvjwe'


def create_agent(first, last, agent_code, email, url, activated=True):
    return agent_service.create(**{
        'first': first,
        'last': last,
        'agent_code': agent_code,
        'email': email,
        'stormpath_url':url,
        'activated':activated,

    })


def create_case(company_name='Test Case', case_token='CASE-123123'):

    agent = create_agent(first='TEST', last='AGENT', agent_code='26AGENT', email='test@delmarsd.com',
                         url=test_agent_stormpath_url)
    case = case_service.create_new_case(**dict(
        company_name=company_name,
        group_number="GRP-NUM-EX123",
        situs_state='MI',
        situs_city='Lansing',
        agent_id=agent.id,
        active=True,
        created_date=datetime.datetime(year=2012, month=1, day=1),
        enrollment_period_type=Case.OPEN_ENROLLMENT_TYPE,
        payment_mode=payment_modes.MODE_MONTHLY,
        is_self_enrollment=False,
        case_token=case_token,
    ))
    FPPTI = product_service.search(by_code='FPPTI')[0]
    case.products.append(FPPTI)

    periods = [dict(period_type=CaseOpenEnrollmentPeriod.PERIOD_TYPE, start_date='2000-01-01', end_date=None)]
    case_service.update_enrollment_periods(case, periods)

    db.session.commit()

    return case

def create_user_in_groups(name, api_token=None, groups=None):
    # For now just use agent1
    if api_token:
        api_token_service.create(**dict(
            api_token=api_token,
            name=name,
            stormpath_url=test_agent_stormpath_url,
            activated=True,
        ))


    #if groups:
    #    for group in groups:

