
from taa.core import DBService
from taa.services import RequiredFeature
from models import SelfEnrollmentSetup

class SelfEnrollmentService(DBService):
    __model__ = SelfEnrollmentSetup

    case_service = RequiredFeature('CaseService')

    def update_enrolling_agent(self, case, agent_id=None):

        from taa.services.agents import AgentService
        agent_service = AgentService()

        enrolling_agent = None
        if not agent_id:
            # We want the owner agent if one currently exists.
            if self.case_service.get_case_owner(case):
                enrolling_agent = self.case_service.get_case_owner(case)
        else:
            # Allow this agent if he is part of this case.
            selected_agent = agent_service.get(agent_id)
            if selected_agent in self.case_service.get_agents_for_case(case):
                enrolling_agent = selected_agent

        # Set the enrolling agent on the self_enrollment_setup.
        case.self_enrollment_setup.enrolling_agent = enrolling_agent
