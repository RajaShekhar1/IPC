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

from taa.core import db
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


def create_enrollment_records_csv(case_id, start_date, end_date):
    """
    Create a CSV Export file for an Enrollment Report

    :param start_date: the date to start searching for entries
    :param end_date: the date to stop searching for entries
    :param case_id: Id of case that Case Census records are related to
    :return: The CSV
    """

    headers = ["Timestamp",
               "Status",
               "Agent Code",
               "Agent Name",
               "Signature City",
               "Signature State",
               "Payment Mode",
               "Enrollment Method",
               "Is Employee Owner",
               "Other Owner Name",
               "Other Owner SSN",
               "Spouse Other Owner",
               "Spouse Other Owner SSN",
               "Is Employee Beneficiary Spouse",
               "Employee Beneficiary Name",
               "Employee Beneficiary Relationship",
               "Employee Beneficiary Birthdate",
               "Employee Beneficiary SSN",
               "Is Spouse Beneficiary Employee",
               "Spouse Beneficiary Name",
               "Spouse Beneficiary Relationship",
               "Spouse Beneficiary Birthdate",
               "Spouse Beneficiary SSN",
               "EMP_FIRST",
               "EMP_LAST",
               "EMP_SSN",
               "EMP_GENDER",
               "EMP_BIRTHDATE",
               "EMP_EMAIL",
               "EMP_PHONE",
               "EMP_ADDRESS1",
               "EMP_ADDRESS2",
               "EMP_CITY",
               "EMP_STATE",
               "EMP_ZIP",
               "EMP_HEIGHT_IN",
               "EMP_WEIGHT_LBS",
               "EMP_SMOKER_Y_N",
               "CLASSIFICATION",
               "SP_FIRST",
               "SP_LAST",
               "SP_SSN",
               "SP_GENDER",
               "SP_BIRTHDATE",
               "SP_EMAIL",
               "SP_PHONE",
               "SP_ADDRESS1",
               "SP_ADDRESS2",
               "SP_CITY",
               "SP_STATE",
               "SP_ZIP",
               "SP_HEIGHT_IN",
               "SP_WEIGHT_LBS",
               "SP_SMOKER_Y_N",
               "CH1_FIRST",
               "CH1_LAST",
               "CH1_BIRTHDATE",
               "CH2_FIRST",
               "CH2_LAST",
               "CH2_BIRTHDATE",
               "CH3_FIRST",
               "CH3_LAST",
               "CH3_BIRTHDATE",
               "CH4_FIRST",
               "CH4_LAST",
               "CH4_BIRTHDATE",
               "CH5_FIRST",
               "CH5_LAST",
               "CH5_BIRTHDATE",
               "CH6_FIRST",
               "CH6_LAST",
               "CH6_BIRTHDATE",
               ]

    csv_buffer = csv.StringIO()
    csv_data = csv.writer(csv_buffer)

    csv_data.writerow(headers)
    rows = db.session.execute("select * from export_enrollment_report(to_date('" + start_date + "','yyyy-mm-dd'), to_date('" + end_date + "','yyyy-mm-dd'), " + str(case_id) + ")")
    [csv_data.writerow([row[column] for column in headers]) for row in rows]
    return csv_buffer.getvalue()
