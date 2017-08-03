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

    headers = [ "Timestamp",
                "Status",
                "Agent Code",
                "Agent Name",
                "Signature City",
                "Signature State",
                "Date of Hire",
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
                "Total Annual Premium",
                "Effective Date",
                "Product 1 Name",
                "Product 1 Total Modal Premium",
                "Product 1 EMP Coverage",
                "Product 1 EMP Premium",
                "Product 1 SP Coverage",
                "Product 1 SP Premium",
                "Product 1 CH Coverage",
                "Product 1 CH Premium",
                "Product 2 Name",
                "Product 2 Total Modal Premium",
                "Product 2 EMP Coverage",
                "Product 2 EMP Premium",
                "Product 2 SP Coverage",
                "Product 2 SP Premium",
                "Product 2 CH Coverage",
                "Product 2 CH Premium",
                "Product 3 Name",
                "Product 3 Total Modal Premium",
                "Product 3 EMP Coverage",
                "Product 3 EMP Premium",
                "Product 3 SP Coverage",
                "Product 3 SP Premium",
                "Product 3 CH Coverage",
                "Product 3 CH Premium",
                "Product 4 Name",
                "Product 4 Total Modal Premium",
                "Product 4 EMP Coverage",
                "Product 4 EMP Premium",
                "Product 4 SP Coverage",
                "Product 4 SP Premium",
                "Product 4 CH Coverage",
                "Product 4 CH Premium",
                "Product 5 Name",
                "Product 5 Total Modal Premium",
                "Product 5 EMP Coverage",
                "Product 5 EMP Premium",
                "Product 5 SP Coverage",
                "Product 5 SP Premium",
                "Product 5 CH Coverage",
                "Product 5 CH Premium",
                "Product 6 Name",
                "Product 6 Total Modal Premium",
                "Product 6 EMP Coverage",
                "Product 6 EMP Premium",
                "Product 6 SP Coverage",
                "Product 6 SP Premium",
                "Product 6 CH Coverage",
                "Product 6 CH Premium",
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
                "SP_BIRTHDATE",
                "SP_GENDER",
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

    rows = db.session.execute("""WITH coverage AS
(
    SELECT
      enrollment_applications.id                     AS enrollment_application_id,
      products.id                                    AS product_id,
      products.name                                  AS product_name,
      row_number()
      OVER (
        PARTITION BY enrollment_applications.id )    AS row_number,
      coalesce(max(employee.monthly_premium), 0) +
      coalesce(max(spouse.monthly_premium), 0) +
      coalesce(sum(children.monthly_premium), 0)     AS total_modal_premium,
      max(employee.monthly_premium)                  AS employee_premium,
      max(employee.coverage_face_value)              AS employee_face_value,
      max(spouse.monthly_premium)                    AS spouse_premium,
      max(spouse.coverage_face_value)                AS spouse_face_value,
      sum(children.monthly_premium)                  AS children_premium,
      string_agg(children.coverage_face_value, ', ') AS children_face_value,
      cast(enrollment_applications.signature_time AS DATE) + 8 -
      (extract(DOW FROM enrollment_applications.signature_time) :: INT + 3) % 7
                                                     AS coverage_start
    FROM
      cases
      INNER JOIN
      enrollment_applications
        ON
          cases.id = enrollment_applications.case_id
      INNER JOIN
      products ON
                 products.id = 71 OR
                 products.id = 2 OR
                 products.id = 106 OR
                 products.id = 1 OR
                 products.id = 23


      LEFT OUTER JOIN
      enrollment_application_coverage AS employee
        ON
          enrollment_applications.id = employee.enrollment_application_id AND
          employee.applicant_type = 'employee' AND
          products.id = employee.product_id

      LEFT OUTER JOIN
      enrollment_application_coverage AS spouse
        ON
          enrollment_applications.id = spouse.enrollment_application_id AND
          spouse.applicant_type = 'spouse' AND
          products.id = spouse.product_id

      LEFT OUTER JOIN
      enrollment_application_coverage AS children
        ON
          enrollment_applications.id = children.enrollment_application_id AND
          children.applicant_type = 'children' AND
          products.id = children.product_id
    WHERE
      enrollment_applications.signature_time > to_date('""" + start_date + """','yyyy-mm-dd')
      AND enrollment_applications.signature_time < to_date('""" + end_date   + """','yyyy-mm-dd')
      AND cases.id = """ + str(case_id) + """
    GROUP BY
      enrollment_applications.id
      , products.id
      , products.name
    ORDER BY
      enrollment_applications.id
      , products.id

)
SELECT
  enrollment_applications.signature_time                                                  AS "Timestamp",
  enrollment_applications.application_status                                              AS "Status",
  enrollment_applications.agent_code                                                      AS "Agent Code",
  enrollment_applications.agent_name                                                      AS "Agent Name",
  enrollment_applications.signature_city                                                  AS "Signature City",
  enrollment_applications.signature_state                                                 AS "Signature State",
  enrollment_applications.identity_token                                                  AS "Date of Hire",
  cast(enrollment_applications.payment_mode AS TEXT)                                      AS "Payment Mode",
  enrollment_applications.method                                                          AS "Enrollment Method",
  initcap(cast((enrollment_applications.is_employee_owner) AS
               TEXT))                                                                     AS "Is Employee Owner",
  enrollment_applications.employee_other_owner_name                                       AS "Other Owner Name",
  enrollment_applications.employee_other_owner_ssn                                        AS "Other Owner SSN",
  enrollment_applications.spouse_other_owner_name                                         AS "Spouse Other Owner",
  enrollment_applications.spouse_other_owner_ssn                                          AS "Spouse Other Owner SSN",
  initcap(cast((enrollment_applications.is_employee_beneficiary_spouse) AS
               TEXT))                                                                     AS "Is Employee Beneficiary Spouse",
  coalesce(nullif(enrollment_applications.employee_beneficiary_name, ''),
           'None')                                                                        AS "Employee Beneficiary Name",
  coalesce(nullif(enrollment_applications.employee_beneficiary_relationship, ''),
           'None')                                                                        AS "Employee Beneficiary Relationship",
  coalesce(enrollment_applications.employee_beneficiary_birthdate,
           'None')                                                                        AS "Employee Beneficiary Birthdate",
  enrollment_applications.employee_beneficiary_ssn                                        AS "Employee Beneficiary SSN",
  initcap(cast((enrollment_applications.is_spouse_beneficiary_employee) AS
               TEXT))                                                                     AS "Is Spouse Beneficiary Employee",
  enrollment_applications.spouse_beneficiary_name                                         AS "Spouse Beneficiary Name",
  enrollment_applications.spouse_beneficiary_relationship                                 AS "Spouse Beneficiary Relationship",
  coalesce(enrollment_applications.spouse_beneficiary_birthdate,
           'None')                                                                        AS "Spouse Beneficiary Birthdate",
  enrollment_applications.spouse_beneficiary_ssn                                          AS "Spouse Beneficiary SSN",
  (coalesce(coverage_1.total_modal_premium, 0) +
   coalesce(coverage_2.total_modal_premium, 0) +
   coalesce(coverage_3.total_modal_premium, 0) +
   coalesce(coverage_4.total_modal_premium, 0) +
   coalesce(coverage_5.total_modal_premium, 0) +
   coalesce(coverage_6.total_modal_premium, 0)) *
  enrollment_applications.payment_mode
                                                                                          AS "Total Annual Premium",


  CASE
  WHEN (enrollment_applications.signature_time :: DATE) < ('2016-08-29' :: DATE)
    THEN 'None'
  ELSE
    CASE WHEN
      extract(WEEK FROM coverage_1.coverage_start) - extract(WEEK FROM date_trunc('month', coverage_1.coverage_start)) <
      4
      THEN coverage_1.coverage_start :: TEXT
    ELSE (coverage_1.coverage_start +
          (((extract(WEEK FROM coverage_1.coverage_start) -
             extract(WEEK FROM date_trunc('month', coverage_1.coverage_start))) -
            3) *
           7) :: INTEGER) :: TEXT
    END
  END
                                                                                          AS "Effective Date",
  coverage_1.product_name                                                                 AS "Product 1 Name",
  coalesce(coverage_1.total_modal_premium,
           0)                                                                             AS "Product 1 Total Modal Premium",
  CASE WHEN coverage_1.employee_premium IS NOT NULL
    THEN 'Included'
  ELSE '' END                                                                             AS "Product 1 EMP Coverage",
  coverage_1.employee_premium                                                             AS "Product 1 EMP Premium",
  CASE WHEN coverage_1.spouse_premium IS NOT NULL
    THEN 'Included'
  ELSE '' END                                                                                "Product 1 SP Coverage",
  NULL                                                                                    AS "Product 1 SP Premium",
  CASE WHEN coverage_1.children_premium IS NOT NULL
    THEN 'Included'
  ELSE '' END                                                                             AS "Product 1 CH Coverage",
  NULL                                                                                    AS "Product 1 CH Premium",
  coverage_2.product_name                                                                 AS "Product 2 Name",
  coalesce(coverage_2.total_modal_premium,
           0)                                                                             AS "Product 2 Total Modal Premium",
  coverage_2.employee_face_value                                                          AS "Product 2 EMP Coverage",
  coverage_2.employee_premium                                                             AS "Product 2 EMP Premium",
  coverage_2.spouse_face_value                                                            AS "Product 2 SP Coverage",
  coverage_2.spouse_premium                                                               AS "Product 2 SP Premium",
  coverage_2.children_face_value                                                          AS "Product 2 CH Coverage",
  coverage_2.children_premium                                                             AS "Product 2 CH Premium",
  coverage_3.product_name                                                                 AS "Product 3 Name",
  coalesce(coverage_3.total_modal_premium,
           0)                                                                             AS "Product 3 Total Modal Premium",
  coverage_3.employee_face_value                                                          AS "Product 3 EMP Coverage",
  coverage_3.employee_premium                                                             AS "Product 3 EMP Premium",
  coverage_3.spouse_face_value                                                            AS "Product 3 SP Coverage",
  coverage_3.spouse_premium                                                               AS "Product 3 SP Premium",
  coverage_3.children_face_value                                                          AS "Product 3 CH Coverage",
  coverage_3.children_premium                                                             AS "Product 3 CH Premium",
  coverage_4.product_name                                                                 AS "Product 4 Name",
  coalesce(coverage_4.total_modal_premium,
           0)                                                                             AS "Product 4 Total Modal Premium",
  coverage_4.employee_face_value                                                          AS "Product 4 EMP Coverage",
  coverage_4.employee_premium                                                             AS "Product 4 EMP Premium",
  coverage_4.spouse_face_value                                                            AS "Product 4 SP Coverage",
  coverage_4.spouse_premium                                                               AS "Product 4 SP Premium",
  coverage_4.children_face_value                                                          AS "Product 4 CH Coverage",
  coverage_4.children_premium                                                             AS "Product 4 CH Premium",
  coverage_5.product_name                                                                 AS "Product 5 Name",
  coalesce(coverage_5.total_modal_premium,
           0)                                                                             AS "Product 5 Total Modal Premium",
  coverage_5.employee_face_value                                                          AS "Product 5 EMP Coverage",
  coverage_5.employee_premium                                                             AS "Product 5 EMP Premium",
  coverage_5.spouse_face_value                                                            AS "Product 5 SP Coverage",
  coverage_5.spouse_premium                                                               AS "Product 5 SP Premium",
  coverage_5.children_face_value                                                          AS "Product 5 CH Coverage",
  coverage_5.children_premium                                                             AS "Product 5 CH Premium",
  coverage_6.product_name                                                                 AS "Product 6 Name",
  coalesce(coverage_6.total_modal_premium,
           0)                                                                             AS "Product 6 Total Modal Premium",
  coverage_6.employee_face_value                                                          AS "Product 6 EMP Coverage",
  coverage_6.employee_premium                                                             AS "Product 6 EMP Premium",
  coverage_6.spouse_face_value                                                            AS "Product 6 SP Coverage",
  coverage_6.spouse_premium                                                               AS "Product 6 SP Premium",
  coverage_6.children_face_value                                                          AS "Product 6 CH Coverage",
  coverage_6.children_premium                                                             AS "Product 6 CH Premium",
  case_census.employee_first                                                              AS "EMP_FIRST",
  case_census.employee_last                                                               AS "EMP_LAST",
  case_census.employee_ssn                                                                AS "EMP_SSN",
  case_census.employee_gender                                                             AS "EMP_GENDER",
  case_census.employee_birthdate                                                          AS "EMP_BIRTHDATE",
  coalesce(case_census.employee_email, 'None')                                            AS "EMP_EMAIL",
  coalesce(case_census.employee_phone, 'None')                                            AS "EMP_PHONE",
  coalesce(case_census.employee_street_address, 'None')                                   AS "EMP_ADDRESS1",
  coalesce(case_census.employee_street_address2, 'None')                                  AS "EMP_ADDRESS2",
  coalesce(case_census.employee_city, 'None')                                             AS "EMP_CITY",
  case_census.employee_state                                                              AS "EMP_STATE",
  case_census.employee_zip                                                                AS "EMP_ZIP",
  coalesce(case_census.employee_height_inches,
           'None')                                                                        AS "EMP_HEIGHT_IN",
  coalesce(case_census.employee_weight_lbs,
           'None')                                                                        AS "EMP_WEIGHT_LBS",
  case_census.employee_smoker                                                             AS "EMP_SMOKER_Y_N",
  case_census.occupation_class                                                            AS "CLASSIFICATION",
  coalesce(case_census.spouse_first, 'None')                                              AS "SP_FIRST",
  coalesce(case_census.spouse_last, 'None')                                               AS "SP_LAST",
  case_census.spouse_ssn                                                                  AS "SP_SSN",
  coalesce(cast(case_census.spouse_birthdate AS TEXT), 'None')                            AS "SP_BIRTHDATE",
  coalesce(case_census.spouse_gender, 'None')                                             AS "SP_GENDER",
  coalesce(case_census.spouse_email, 'None')                                              AS "SP_EMAIL",
  coalesce(case_census.spouse_phone, 'None')                                              AS "SP_PHONE",
  coalesce(case_census.spouse_street_address, 'None')                                     AS "SP_ADDRESS1",
  coalesce(case_census.spouse_street_address2, 'None')                                    AS "SP_ADDRESS2",
  coalesce(case_census.spouse_city, 'None')                                               AS "SP_CITY",
  case_census.spouse_state                                                                AS "SP_STATE",
  coalesce(case_census.spouse_zip, 'None')                                                AS "SP_ZIP",
  coalesce(nullif(case_census.spouse_height_inches, ''), 'None')                          AS "SP_HEIGHT_IN",
  coalesce(nullif(case_census.spouse_weight_lbs, ''),
           'None')                                                                        AS "SP_WEIGHT_LBS",
  case_census.spouse_smoker                                                               AS "SP_SMOKER_Y_N",
  coalesce(nullif(case_census.child1_first, ''), 'None')                                  AS "CH1_FIRST",
  coalesce(nullif(case_census.child1_last, ''), 'None')                                   AS "CH1_LAST",
  coalesce(cast(case_census.child1_birthdate AS TEXT),
           'None')                                                                        AS "CH1_BIRTHDATE",
  coalesce(nullif(case_census.child2_first, ''), 'None')                                  AS "CH2_FIRST",
  coalesce(nullif(case_census.child2_last, ''), 'None')                                   AS "CH2_LAST",
  coalesce(cast(case_census.child2_birthdate AS TEXT),
           'None')                                                                        AS "CH2_BIRTHDATE",
  coalesce(nullif(case_census.child3_first, ''), 'None')                                  AS "CH3_FIRST",
  coalesce(nullif(case_census.child3_last, ''), 'None')                                   AS "CH3_LAST",
  coalesce(cast(case_census.child3_birthdate AS TEXT),
           'None')                                                                        AS "CH3_BIRTHDATE",
  coalesce(nullif(case_census.child4_first, ''), 'None')                                  AS "CH4_FIRST",
  coalesce(nullif(case_census.child4_last, ''), 'None')                                   AS "CH4_LAST",
  coalesce(cast(case_census.child4_birthdate AS TEXT),
           'None')                                                                        AS "CH4_BIRTHDATE",
  coalesce(nullif(case_census.child5_first, ''), 'None')                                  AS "CH5_FIRST",
  coalesce(nullif(case_census.child5_last, ''), 'None')                                   AS "CH5_LAST",
  coalesce(cast(case_census.child5_birthdate AS TEXT),
           'None')                                                                        AS "CH5_BIRTHDATE",
  coalesce(nullif(case_census.child6_first, ''), 'None')                                  AS "CH6_FIRST",
  coalesce(nullif(case_census.child6_last, ''), 'None')                                   AS "CH6_LAST",
  coalesce(cast(case_census.child6_birthdate AS TEXT), 'None')                            AS "CH6_BIRTHDATE"
FROM
  cases
  INNER JOIN enrollment_applications ON cases.id = enrollment_applications.case_id
  LEFT JOIN
  case_census
    ON
      cases.id = case_census.case_id AND case_census.id = enrollment_applications.census_record_id
  INNER JOIN coverage AS coverage_1
    ON coverage_1.enrollment_application_id = enrollment_applications.id AND
       --coverage_1.row_number = 1 and
       (coverage_1.product_name = 'HealthDepot Membership' OR
        coverage_1.product_name IS NULL)
  LEFT OUTER JOIN coverage AS coverage_2
    ON coverage_1.enrollment_application_id = coverage_2.enrollment_application_id AND
       (coverage_2.product_name = 'Family Protection Plan - Critical Illness' OR
        coverage_2.product_name IS NULL)
  LEFT OUTER JOIN coverage AS coverage_3
    ON coverage_1.enrollment_application_id = coverage_3.enrollment_application_id AND
       (coverage_3.product_name = 'Group CI - Health Depot' OR
        coverage_3.product_name IS NULL)
  LEFT OUTER JOIN coverage AS coverage_4
    ON coverage_1.enrollment_application_id = coverage_4.enrollment_application_id AND
       (coverage_4.product_name = 'FPP-Terminal Illness (Health Depot)' OR
        coverage_4.product_name IS NULL)
  LEFT OUTER JOIN coverage AS coverage_5
    ON coverage_1.enrollment_application_id = coverage_5.enrollment_application_id AND
       (coverage_5.product_name = 'Family Protection Plan - Terminal Illness' OR
        coverage_5.product_name IS NULL)
  LEFT OUTER JOIN coverage AS coverage_6
    ON coverage_1.enrollment_application_id = coverage_6.enrollment_application_id AND coverage_6.row_number = 6

WHERE
  enrollment_applications.signature_time > to_date('""" + start_date + """','yyyy-mm-dd')
  AND enrollment_applications.signature_time < to_date('""" + end_date   + """','yyyy-mm-dd')
  AND cases.id = """ + str(case_id) + ";")

    [csv_data.writerow([unicode(row[column]).encode('utf-8', 'backslashreplace') for column in headers]) for row in rows]
    return csv_buffer.getvalue()
