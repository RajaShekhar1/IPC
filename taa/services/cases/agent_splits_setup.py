from taa.core import DBService
from taa.services import RequiredFeature
from models import AgentSplitsSetup

class AgentSplitsService(DBService):
    __model__ = AgentSplitsSetup
    case_service = RequiredFeature('CaseService')
