import csv

from models import (Case, CaseCensus, CaseEnrollmentPeriod,
                    CaseOpenEnrollmentPeriod, CaseOngoingEnrollmentPeriod,
                    SelfEnrollmentSetup, AgentSplitsSetup)

from case_service import CaseService
from enrollment_periods import CaseEnrollmentPeriodsService
from census_records import CensusRecordService
from census_import import CensusRecordParser
from self_enroll_setup import SelfEnrollmentService
from agent_splits_setup import AgentSplitsService

from taa.services import LookupService

def create_census_records_csv(case_id, start, end):
    """
    Create a CSV Export file for a collection Case Census Records

    :param start: the date to start searching for entries
    :param end: the date to stop searching for entries
    :param case_id: Id of case that Case Census records are related to
    :type applications: list[taa.services.cases.models.CaseCensus]
    :return: The CSV
    """
    csv_buffer = csv.StringIO()
    csv_data = csv.writer(csv_buffer)

    # taa.services.cases.models.CaseCensus
    census_record = CensusRecordService()

    # taa.services.cases.models.Case
    case_service = CaseService()

    # Get pre-defined headers for file
    headers = census_record.get_csv_headers()
    csv_data.writerow(headers)

    rows = case_service.get_census_records_csv(case_id, start, end)
    [csv_data.writerow([row[column] for column in headers]) for row in rows]
    return csv_buffer.getvalue()
