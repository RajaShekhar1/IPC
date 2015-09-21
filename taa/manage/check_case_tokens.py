from flask import current_app
from flask_script import Command, prompt, prompt_pass
from werkzeug.datastructures import MultiDict

import os
import random
import urllib, json
import calendar, datetime

from ..services.cases import CaseService, Case
from ..models import db

class CheckCaseTokensCommand(Command):
    """Scramble all data in the database to not contain real information"""

    def run(self):
        case_service = CaseService()
        cases = db.session.query(Case).all()
        for case in cases:
            case_service.populate_case_token(case)
        db.session.commit()
