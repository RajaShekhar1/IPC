"""Add census report and export_enrollment_report procedures

Revision ID: 75522d63df0d
Revises: 5bbb4335a759
Create Date: 2017-06-08 16:05:49.537840

"""

# revision identifiers, used by Alembic.
revision = '75522d63df0d'
down_revision = 'aa04f95b63d4'

from alembic import op
from taa.alembic_helpers import ReplaceableObject


census_report = ReplaceableObject(
    "census_report(min timestamp without time zone, max timestamp without time zone, caseid integer)",
    """
    returns TABLE("EMP_FIRST" character varying, "EMP_LAST" character varying, "EMP_SSN" character varying, "EMP_GENDER"
    character varying, "EMP_BIRTHDATE" date, "EMP_EMAIL" character varying, "EMP_PHONE" character varying,
    "EMP_ADDRESS1" character varying, "EMP_ADDRESS2" character varying, "EMP_CITY" character varying, "EMP_STATE"
    character varying, "EMP_ZIP" character varying, "EMP_HEIGHT_IN" character varying, "EMP_WEIGHT_LBS" character
    varying, "EMP_SMOKER_Y_N" character varying, "CLASSIFICATION" character varying, "SP_FIRST" character varying,
    "SP_LAST" character varying, "SP_SSN" character varying, "SP_GENDER" character varying, "SP_BIRTHDATE" date,
    "SP_EMAIL" character varying, "SP_PHONE" character varying, "SP_ADDRESS1" character varying, "SP_ADDRESS2"
    character varying, "SP_CITY" character varying, "SP_STATE" character varying, "SP_ZIP" character varying,
    "SP_HEIGHT_IN" character varying, "SP_WEIGHT_LBS" character varying, "SP_SMOKER_Y_N" character varying,
    "CH1_FIRST" character varying, "CH1_LAST" character varying, "CH1_BIRTHDATE" date, "CH2_FIRST" character
    varying, "CH2_LAST" character varying, "CH2_BIRTHDATE" date, "CH3_FIRST" character varying, "CH3_LAST"
    character varying, "CH3_BIRTHDATE" date, "CH4_FIRST" character varying, "CH4_LAST" character varying,
    "CH4_BIRTHDATE" date, "CH5_FIRST" character varying, "CH5_LAST" character varying, "CH5_BIRTHDATE" date,
    "CH6_FIRST" character varying, "CH6_LAST" character varying, "CH6_BIRTHDATE" date)
    LANGUAGE plpgsql
    AS $$
    BEGIN

RETURN QUERY SELECT
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
FROM case_census WHERE case_census.case_id = caseid;

END;
$$;


    """)


export_enrollment_report = ReplaceableObject(
    "export_enrollment_report(min timestamp without time zone, max timestamp without time zone, caseid integer)",
    """
    returns TABLE("Timestamp" timestamp without time zone, "Status" character varying, "Agent Code" character varying,
        "Agent Name" character varying, "Signature City" text, "Signature State" character varying, "Payment Mode" integer,
        "Enrollment Method" character varying, "Is Employee Owner" boolean, "Other Owner Name" text, "Other Owner SSN"
        character varying, "Spouse Other Owner" boolean, "Spouse Other Owner SSN" character varying, "Is Employee Beneficiary
        Spouse" boolean, "Employee Beneficiary Name" text, "Employee Beneficiary Relationship" text, "Employee Beneficiary
        Birthdate" text, "Employee Beneficiary SSN" character varying, "Is Spouse Beneficiary Employee" boolean,
        "Spouse Beneficiary Name" text, "Spouse Beneficiary Relationship" text, "Spouse Beneficiary Birthdate" text,
        "Spouse Beneficiary SSN" character varying, "EMP_FIRST" character varying, "EMP_LAST" character varying, "EMP_SSN"
        character varying, "EMP_GENDER" character varying, "EMP_BIRTHDATE" date, "EMP_EMAIL" character varying,
        "EMP_PHONE" character varying, "EMP_ADDRESS1" character varying, "EMP_ADDRESS2" character varying, "EMP_CITY"
        character varying, "EMP_STATE" character varying, "EMP_ZIP" character varying, "EMP_HEIGHT_IN" character varying,
        "EMP_WEIGHT_LBS" character varying, "EMP_SMOKER_Y_N" character varying, "CLASSIFICATION" character varying,
        "SP_FIRST" character varying, "SP_LAST" character varying, "SP_SSN" character varying, "SP_GENDER" character
        varying, "SP_BIRTHDATE" date, "SP_EMAIL" character varying, "SP_PHONE" character varying, "SP_ADDRESS1"
        character varying, "SP_ADDRESS2" character varying, "SP_CITY" character varying, "SP_STATE" character varying,
        "SP_ZIP" character varying, "SP_HEIGHT_IN" character varying, "SP_WEIGHT_LBS" character varying,
        "SP_SMOKER_Y_N" character varying, "CH1_FIRST" character varying, "CH1_LAST" character varying,
        "CH1_BIRTHDATE" date, "CH2_FIRST" character varying, "CH2_LAST" character varying, "CH2_BIRTHDATE"
        date, "CH3_FIRST" character varying, "CH3_LAST" character varying, "CH3_BIRTHDATE" date, "CH4_FIRST"
        character varying, "CH4_LAST" character varying, "CH4_BIRTHDATE" date, "CH5_FIRST" character varying,
        "CH5_LAST" character varying, "CH5_BIRTHDATE" date, "CH6_FIRST" character varying, "CH6_LAST" character varying,
            "CH6_BIRTHDATE" date)
LANGUAGE plpgsql
AS $$
BEGIN
RETURN QUERY SELECT DISTINCT
--   cases.id,
--   case_census.id AS case_census_id,
  enrollment_applications.signature_time as "Timestamp",
  enrollment_applications.application_status AS "Status",
  enrollment_applications.agent_code AS "Agent Code",
  enrollment_applications.agent_name AS "Agent Name",
  enrollment_applications.signature_city AS "Signature City",
  enrollment_applications.signature_state AS "Signature State",
--   enrollment_applications. AS "Date of Hire",
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
--   enrollment_applications. AS "Total Annual Premium",
--   enrollment_applications. AS "Effective Date",
--   "Product 1 Name"
--   "Product 1 Total Modal Premium"
--   "Product 1 EMP Coverage"
--   "Product 1 EMP Premium"
--   "Product 1 SP Coverage"
--   "Product 1 SP Premium"
--   "Product 1 CH Coverage"
--   "Product 1 CH Premium"
--   "Product 2 Name"
--   "Product 2 Total Modal Premium"
--   "Product 2 EMP Coverage"
--   "Product 2 EMP Premium"
--   "Product 2 SP Coverage"
--   "Product 2 SP Premium"
--   "Product 2 CH Coverage"
--   "Product 2 CH Premium"
--   "Product 3 Name"
--   "Product 3 Total Modal Premium"
--   "Product 3 EMP Coverage"
--   "Product 3 EMP Premium"
--   "Product 3 SP Coverage"
--   "Product 3 SP Premium"
--   "Product 3 CH Coverage"
--   "Product 3 CH Premium"
--   "Product 4 Name"
--   "Product 4 Total Modal Premium"
--   "Product 4 EMP Coverage"
--   "Product 4 EMP Premium"
--   "Product 4 SP Coverage"
--   "Product 4 SP Premium"
--   "Product 4 CH Coverage"
--   "Product 4 CH Premium"
--   "Product 5 Name"
--   "Product 5 Total Modal Premium"
--   "Product 5 EMP Coverage"
--   "Product 5 EMP Premium"
--   "Product 5 SP Coverage"
--   "Product 5 SP Premium"
--   "Product 5 CH Coverage"
--   "Product 5 CH Premium"
--   "Product 6 Name"
--   "Product 6 Total Modal Premium"
--   "Product 6 EMP Coverage"
--   "Product 6 EMP Premium"
--   "Product 6 SP Coverage"
--   "Product 6 SP Premium"
--   "Product 6 CH Coverage"
--   "Product 6 CH Premium"
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
  AND enrollment_applications.signature_time > min
  AND enrollment_applications.signature_time < max
  AND case_census.case_id = caseid;
END;
$$;

    """)


def upgrade():
    op.create_sp(census_report)
    op.create_sp(export_enrollment_report)


def downgrade():
    op.drop_sp(census_report)
    op.drop_sp(export_enrollment_report)
