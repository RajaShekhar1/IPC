from flask_stormpath import current_user

from taa.services.agents import AgentService

def get_nav_menu():
    agent_menu = [
        dict(nav_name='nav_home', url_name='home', display='Home'),
        dict(nav_name='nav_manage_cases', url_name='manage_cases', display='Manage Cases'),
        dict(nav_name='nav_enroll', url_name='enroll_start', display='Enroll'),
        dict(nav_name='nav_agentsign', url_name='inbox', display='Sign'),
    ]
    home_office_menu = [
        dict(nav_name='nav_home', url_name='home', display='Home'),
        dict(nav_name='nav_manage_agents', url_name='manage_agents', display='Agents'),
        dict(nav_name='nav_customproducts', url_name='manage_custom_products', display='Products'),

        dict(nav_name='nav_manage_cases', url_name='manage_cases', display='Manage Cases'),
    ]
    
    try:
        is_admin = AgentService().is_user_home_office(current_user)
    except Exception:
        return []
    
    if is_admin:
        return home_office_menu
    else:
        return agent_menu