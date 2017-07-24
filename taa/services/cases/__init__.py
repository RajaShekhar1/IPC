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

    rows = db.session.execute("""SELECT DISTINCT
  enrollment_applications.signature_time as "Timestamp",
  enrollment_applications.application_status AS "Status",
  enrollment_applications.agent_code AS "Agent Code",
  enrollment_applications.agent_name AS "Agent Name",
  enrollment_applications.signature_city AS "Signature City",
  enrollment_applications.signature_state AS "Signature State",
  enrollment_applications.payment_mode AS "Payment Mode",
  enrollment_applications.method AS "Enrollment Method",
  enrollment_applications.is_employee_owner AS "Is Employee Owner",
  enrollment_applications.employee_other_owner_name AS "Other Owner Name",
  enrollment_applications.employee_other_owner_ssn AS "Other Owner SSN",
  enrollment_applications.is_spouse_owner AS "Spouse Other Owner",
  enrollment_applications.spouse_other_owner_ssn AS "Spouse Other Owner SSN",
  enrollment_applications.is_employee_beneficiary_spouse AS "Is Employee Beneficiary Spouse",
  enrollment_applications.employee_beneficiary_name AS "Employee Beneficiary Name",
  enrollment_applications.employee_beneficiary_relationship AS "Employee Beneficiary Relationship",
  enrollment_applications.employee_beneficiary_birthdate AS "Employee Beneficiary Birthdate",
  enrollment_applications.employee_beneficiary_ssn AS "Employee Beneficiary SSN",
  enrollment_applications.is_spouse_beneficiary_employee AS "Is Spouse Beneficiary Employee",
  enrollment_applications.spouse_beneficiary_name AS "Spouse Beneficiary Name",
  enrollment_applications.spouse_beneficiary_relationship AS "Spouse Beneficiary Relationship",
  enrollment_applications.spouse_beneficiary_birthdate AS "Spouse Beneficiary Birthdate",
  enrollment_applications.spouse_beneficiary_ssn AS "Spouse Beneficiary SSN",
  case_census.employee_first AS "EMP_FIRST",
  case_census.employee_last AS "EMP_LAST",
  case_census.employee_ssn AS "EMP_SSN",
  case_census.employee_gender AS "EMP_GENDER",
  case_census.employee_birthdate as "EMP_BIRTHDATE",
  case_census.employee_email as "EMP_EMAIL",
  case_census.employee_phone as "EMP_PHONE",
  case_census.employee_street_address as "EMP_ADDRESS1",
  case_census.employee_street_address2 as "EMP_ADDRESS2",
  case_census.employee_city as "EMP_CITY",
  case_census.employee_state as "EMP_STATE",
  case_census.employee_zip as "EMP_ZIP",
  case_census.employee_height_inches as "EMP_HEIGHT_IN",
  case_census.employee_weight_lbs as "EMP_WEIGHT_LBS",
  case_census.employee_smoker as "EMP_SMOKER_Y_N",
  case_census.occupation_class as "CLASSIFICATION",
  case_census.spouse_first as "SP_FIRST",
  case_census.spouse_last as "SP_LAST",
  case_census.spouse_ssn as "SP_SSN",
  case_census.spouse_gender as "SP_GENDER",
  case_census.spouse_birthdate as "SP_BIRTHDATE",
  case_census.spouse_email as "SP_EMAIL",
  case_census.spouse_phone as "SP_PHONE",
  case_census.spouse_street_address as "SP_ADDRESS1",
  case_census.spouse_street_address2 as "SP_ADDRESS2",
  case_census.spouse_city as "SP_CITY",
  case_census.spouse_state as "SP_STATE",
  case_census.spouse_zip as "SP_ZIP",
  case_census.spouse_height_inches as "SP_HEIGHT_IN",
  case_census.spouse_weight_lbs as "SP_WEIGHT_LBS",
  case_census.spouse_smoker as "SP_SMOKER_Y_N",
  case_census.child1_first as "CH1_FIRST",
  case_census.child1_last as "CH1_LAST",
  case_census.child1_birthdate as "CH1_BIRTHDATE",
  case_census.child2_first as "CH2_FIRST",
  case_census.child2_last as "CH2_LAST",
  case_census.child2_birthdate as "CH2_BIRTHDATE",
  case_census.child3_first as "CH3_FIRST",
  case_census.child3_last as "CH3_LAST",
  case_census.child3_birthdate as "CH3_BIRTHDATE",
  case_census.child4_first as "CH4_FIRST",
  case_census.child4_last as "CH4_LAST",
  case_census.child4_birthdate as "CH4_BIRTHDATE",
  case_census.child5_first as "CH5_FIRST",
  case_census.child5_last as "CH5_LAST",
  case_census.child5_birthdate as "CH5_BIRTHDATE",
  case_census.child6_first as "CH6_FIRST",
  case_census.child6_last as "CH6_LAST",
  case_census.child6_birthdate as "CH6_BIRTHDATE"
FROM
  cases
    INNER JOIN
  case_census
    ON
      cases.id = case_census.case_id
    INNER JOIN
  enrollment_applications
    ON
      case_census.id = enrollment_applications.census_record_id
    INNER JOIN
  case_products
    ON
  cases.id = case_products.case_id
    LEFT JOIN
  products
    ON
  case_products.product_id = products.id
    LEFT JOIN
    products_custom_guaranteed_issue
    ON
      products.id = products_custom_guaranteed_issue.id
  LEFT JOIN
    products AS base_products
    ON
      products_custom_guaranteed_issue.base_product_id = base_products.id

WHERE
  NOT enrollment_applications.standardized_data IS NULL
  AND NOT cast(enrollment_applications.standardized_data AS TEXT) = 'null'
  AND enrollment_applications.is_preview != TRUE
  AND enrollment_applications.signature_time > to_date('""" + start_date + """','yyyy-mm-dd')
  AND enrollment_applications.signature_time < to_date('""" + end_date   + """','yyyy-mm-dd')
  AND case_census.case_id = """ + str(case_id) + ";")

    [csv_data.writerow([unicode(row[column]).encode('utf-8', 'backslashreplace') for column in headers]) for row in rows]
    return csv_buffer.getvalue()
