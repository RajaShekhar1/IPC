from flask_stormpath import current_user

from taa.services.agents import AgentService

def get_nav_menu():
    is_agent = False
    is_home_office = False
    is_admin = False
    try:
        is_agent = AgentService().is_user_agent(current_user)
        is_home_office = AgentService().is_user_home_office(current_user)
        is_admin = AgentService().is_user_admin(current_user)
        is_third_party_enroller = AgentService().is_user_third_party_enroller(current_user)
    except Exception:
        return []

    if is_agent:
        return [
            dict(nav_name='nav_home', url_name='home', display='Home'),
            dict(nav_name='nav_manage_cases', url_name='manage_cases', display='Enrollment Cases'),
            #dict(nav_name='nav_enroll', url_name='enroll_start', display='Enroll'),
            dict(nav_name='nav_agentsign', url_name='inbox', display='Inbox'),
        ]
    elif is_home_office:
        return [
            dict(nav_name='nav_home', url_name='home', display='Home'),
            dict(nav_name='nav_manage_agents', url_name='manage_agents', display='Users'),
            dict(nav_name='nav_customproducts', url_name='manage_custom_products', display='Products'),
            dict(nav_name='nav_manage_cases', url_name='manage_cases', display='Enrollment Cases'),
        ]
    elif is_admin:
        return [
            dict(nav_name='nav_home', url_name='home', display='Home'),
            dict(nav_name='nav_manage_agents', url_name='manage_agents', display='Users'),
            dict(nav_name='nav_customproducts', url_name='manage_custom_products', display='Products'),
            dict(nav_name='nav_manage_cases', url_name='manage_cases', display='Enrollment Cases'),
        ]
    elif is_third_party_enroller:
        return [
            dict(nav_name='nav_home', url_name='home', display='Home'),
        ]
    return []
