import datetime

from taa.config_defaults import STORMPATH_API_KEY_ID, STORMPATH_API_KEY_SECRET, STORMPATH_APPLICATION
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


def create_agent(first, last, agent_code, email, activated=True):
    from stormpath.client import Client
    client = Client(id=STORMPATH_API_KEY_ID,
                    secret=STORMPATH_API_KEY_SECRET)
    stormpath_application = client.applications.search(STORMPATH_APPLICATION)[0]

    matches = filter(lambda a: a.email == email, stormpath_application.accounts)

    if matches:
        # We have a match, update with info
        account = matches[0]
        account.given_name = first
        account.surname = last

        # account.custom_data['agency'] = data['agency']
        account.custom_data['agent_code'] = agent_code
        # account.custom_data['signing_name'] = data['signing_name']
        # account.custom_data['ds_apikey'] = data['ds_apikey']
        account.custom_data['activated'] = activated
        account.save()
    else:
        account = stormpath_application.accounts.create({
            'given_name': first,
            'surname': last,
            'username': email,
            'email': email,
            'password': '12121212',
            'custom_data': {
                'agent_code': agent_code,
                'activated': True,
            },
        })

    # Make sure in agent group
    agents_group = filter(lambda g: g.name == 'agents', stormpath_application.groups.items)[0]
    existing_membership = [acct_mem
                           for acct_mem in agents_group.account_memberships
                           if acct_mem.account.email == account.email]
    if not existing_membership:
        # Add this account to the group
        agents_group.add_account(account)

    # Create user in database
    agent = agent_service.create(**{
        'first': unicode(first),
        'last': unicode(last),
        'agent_code': unicode(agent_code),
        'email': unicode(email),
        'stormpath_url': unicode(account.href),
        'activated': unicode(activated),
    })

    db.session.commit()
    return agent


def create_case(company_name=u'Test Case', case_token=u'CASE-123123', product_codes=None, agent=None, case_id=None):
    """
    Creates an actively-enrolling case with the given parameters.
    """
    if not agent:
        agent = create_agent(first=u'TEST', last=u'AGENT',
                             agent_code=u'26AGENT',
                             email=u'test-case-owner@delmarsd.com')
    data = dict(
        company_name=unicode(company_name),
        group_number=u"GRP-NUM-EX123",
        situs_state=u'MI',
        situs_city=u'Lansing',
        agent_id=agent.id,
        active=True,
        created_date=datetime.datetime(year=2012, month=1, day=1),
        enrollment_period_type=Case.OPEN_ENROLLMENT_TYPE,
        payment_mode=payment_modes.MODE_MONTHLY,
        is_self_enrollment=False,
        case_token=case_token,
    )
    if case_id:
        data['id'] = case_id

    case = case_service.create_new_case(**data)
    if not product_codes:
        product_codes = ['FPPTI']
    for code in product_codes:
        product = product_service.search(by_code=code)[0]
        case.products.append(product)

    periods = [dict(period_type=CaseOpenEnrollmentPeriod.PERIOD_TYPE, start_date='2000-01-01', end_date=None)]
    case_service.update_enrollment_periods(case, periods)

    db.session.commit()

    return case


def create_case_call_center(company_name=u'Test Case', case_token=u'CASE-123123', call_center=True, product_codes=None, agent=None, case_id=None):
    """
    Creates an actively-enrolling case with the given parameters.
    """
    if not agent:
        agent = create_agent(first=u'TEST', last=u'AGENT',
                             agent_code=u'26AGENT',
                             email=u'test-case-owner@delmarsd.com')
    data = dict(
        company_name=unicode(company_name),
        group_number=u"GRP-NUM-EX123",
        situs_state=u'MI',
        situs_city=u'Lansing',
        agent_id=agent.id,
        active=True,
        created_date=datetime.datetime(year=2012, month=1, day=1),
        enrollment_period_type=Case.OPEN_ENROLLMENT_TYPE,
        payment_mode=payment_modes.MODE_MONTHLY,
        is_self_enrollment=False,
        should_use_call_center_workflow=call_center,
        case_token=case_token,
    )
    if case_id:
        data['id'] = case_id

    case = case_service.create_new_case(**data)
    if not product_codes:
        product_codes = ['FPPTI']
    for code in product_codes:
        product = product_service.search(by_code=code)[0]
        case.products.append(product)

    periods = [dict(period_type=CaseOpenEnrollmentPeriod.PERIOD_TYPE, start_date='2000-01-01', end_date=None)]
    case_service.update_enrollment_periods(case, periods)

    db.session.commit()

    return case


def create_case_self_enroll(company_name=u'Test Case', case_token=u'CASE-123123', call_center=False, product_codes=None, agent=None, case_id=None):
    """
    Creates an actively-enrolling case with the given parameters.
    """
    if not agent:
        agent = create_agent(first=u'TEST', last=u'AGENT',
                             agent_code=u'26AGENT',
                             email=u'test-case-owner@delmarsd.com')
    data = dict(
        company_name=unicode(company_name),
        group_number=u"GRP-NUM-EX123",
        situs_state=u'MI',
        situs_city=u'Lansing',
        agent_id=agent.id,
        active=True,
        created_date=datetime.datetime(year=2012, month=1, day=1),
        enrollment_period_type=Case.OPEN_ENROLLMENT_TYPE,
        payment_mode=payment_modes.MODE_MONTHLY,
        is_self_enrollment=True,
        should_use_call_center_workflow=call_center,
        case_token=case_token,
    )
    if case_id:
        data['id'] = case_id

    case = case_service.create_new_case(**data)
    if not product_codes:
        product_codes = ['FPPTI']
    for code in product_codes:
        product = product_service.search(by_code=code)[0]
        case.products.append(product)

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


        # if groups:
        #    for group in groups:
