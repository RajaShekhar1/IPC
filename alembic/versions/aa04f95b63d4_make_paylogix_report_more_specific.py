"""make paylogix report more specific

Revision ID: aa04f95b63d4
Revises: 146bba82e494
Create Date: 2017-06-20 13:27:35.213346

"""

# revision identifiers, used by Alembic.
revision = 'aa04f95b63d4'
down_revision = '146bba82e494'

import sys
from os.path import dirname, abspath
sys.path.append(dirname(dirname(dirname(abspath(__file__)))))
from alembic import op
from taa.alembic_helpers import ReplaceableObject

paylogix_report = ReplaceableObject(
    "paylogix_report(min timestamp without time zone, max timestamp without time zone, pending_only boolean)",
    """
        returns TABLE("Signature Time" text, "Effective Date" text, "EE SSN" text, "EE Last Name" character varying, "EE First Name" character varying, "Account Holder Name" text, "ACH Routing Number" text, "ACH Account Number" text, "ACH Account Type" text, "Bank Name" text, "Address One" text, "Address Two" text, "City, State, Zip" text, "Deduction Week" integer, "Product Code" character varying, "Product Name" character varying, "Insured Last Name" text, "Insured First Name" text, "Insured DOB" text, "Insured Premium" text, "Insured Coverage" text, "Agent Code" character varying, "Group Number" character varying, "Company Name" character varying)
            LANGUAGE plpgsql
        AS $$
            BEGIN

                CREATE TEMPORARY TABLE temp_json (
                    case_id INTEGER,
                    case_census_id INTEGER,
                    enrollment_applicaiton_id INTEGER,
                    json_data JSON
                ) ON COMMIT DROP;

                INSERT INTO temp_json
                SELECT
                    cases.id,
                    case_census.id AS case_census_id,
                    enrollment_applications.id AS enrollment_applicaiton_id,
                    cast(
                        replace(
                            replace(
                                substring(
                                    cast(
                                        enrollment_applications.standardized_data AS TEXT),
                                    2,
                                    length(
                                        cast(
                                            enrollment_applications.standardized_data AS TEXT)
                                    ) -
                                2),
                                '\"',
                            '"'),
                            '\\',
                        '\') AS JSON
                    ) AS json_data
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
                    enrollment_application_submissions              
                        ON              
                            enrollment_applications.id = enrollment_application_submissions.enrollment_application_id
                        INNER JOIN              
                    enrollment_submissions              
                        ON              
                            enrollment_application_submissions.enrollment_submission_id = enrollment_submissions.id              
                WHERE
                    cases.id = 22 AND
                    NOT enrollment_applications.standardized_data IS NULL AND
                    NOT cast(enrollment_applications.standardized_data AS TEXT) = 'null' AND
                    enrollment_applications.is_preview != TRUE AND
                    cases.requires_paylogix_export = TRUE AND
                    enrollment_applications.signature_time > min AND
                    enrollment_applications.signature_time < max AND
                    enrollment_submissions.submission_type = 'Paylogix CSV Generation' and
                        ((pending_only = TRUE AND
                                enrollment_submissions.status = 'pending') OR
                            pending_only = FALSE)
                ;

                RETURN QUERY SELECT DISTINCT
                    to_char(
                        enrollment_applications.signature_time,
                        'YYYY-MM-ddThh24:MI:SS'
                    ) AS "Signature Time",

                    to_char(
                        enrollment_application_coverage.effective_date,
                        'YYYY-MM-DD'
                    ) AS "Effective Date",

                    to_char(
                        to_number(
                            substring(
                                case_census.employee_ssn,
                                '([0-9]+)'
                            ),
                        '999999999'),
                    '000000000') AS "EE SSN",

                    case_census.employee_last AS "EE Last Name",

                    case_census.employee_first AS "EE First Name",

                    substring(
                        regexp_replace(
                            (temp_json.json_data -> 0 -> 'bank_info' -> 'account_holder_name')::TEXT,
                            '\\t',
                            ''),
                        '([^"]+)') AS "Account Holder Name",

                    substring(
                        (temp_json.json_data  -> 0 -> 'bank_info' -> 'routing_number')::TEXT,
                        '([0-9]+)') AS "ACH Routing Number",

                    substring(
                        (temp_json.json_data -> 0 -> 'bank_info' -> 'account_number')::TEXT,
                        '([0-9]+)') AS "ACH Account Number",

                    upper(
                        substring(
                            (temp_json.json_data  -> 0 -> 'bank_info' -> 'account_type')::TEXT,
                            2,
                            1)) AS "ACH Account Type",
                    trim(BOTH '" ' FROM
                        regexp_replace(
                            (temp_json.json_data  -> 0 -> 'bank_info' -> 'bank_name')::TEXT,
                            '\\t',
                            '')
                        )
                        AS "Bank Name",

                    substring(
                        regexp_replace(
                            (temp_json.json_data  -> 0 -> 'bank_info' -> 'address_one')::TEXT,
                            '\\t',
                            ''),
                        '([^"]+)') AS "Address One",

                    coalesce(
                        substring(
                            (temp_json.json_data  -> 0 -> 'bank_info' -> 'address_two')::TEXT,
                            '([^"]+)'),'') AS "Address Two",

                    substring(
                        (temp_json.json_data  -> 0 -> 'bank_info' -> 'city_state_zip')::TEXT,
                        '([^"]+)')as "City, State, Zip",

                    get_draft_week(enrollment_applications.signature_time::DATE)
                    AS "Deduction Week",

                    CASE
                        WHEN
                            base_products.code <> ''
                        THEN
                           base_products.code
                        WHEN
                            products.code <> ''
                        THEN
                            products.code
                        ELSE
                            'Static Benefit'
                    END
                       AS "Product Code",

                    products.name AS "Product Name",

                    substring(
                        CASE
                            WHEN
                                applicant_type = 'children' AND
                                children.child_last IS NOT NULL
                                THEN
                                    children.child_last
                            WHEN
                                applicant_type = 'children'
                                THEN
                                    (temp_json.json_data  -> 0 -> 'children' -> 0 -> 'last')::TEXT
                            WHEN
                                applicant_type = 'spouse'
                                THEN
                                (temp_json.json_data  -> 0 -> 'spouse' -> 'last')::TEXT
                            ELSE
                                (temp_json.json_data  -> 0 -> 'employee' -> 'last')::TEXT
                        END,
                        '([^"]+)')  AS "Insured Last Name",

                    substring(
                        CASE
                            WHEN
                                applicant_type = 'children' AND
                                children.child_first IS NOT NULL
                                THEN
                                    children.child_first
                            WHEN
                                applicant_type = 'children'
                                THEN
                                  (temp_json.json_data  -> 0 -> 'children' -> 0 -> 'first')::TEXT
                            WHEN
                                applicant_type = 'spouse'
                                THEN
                                (temp_json.json_data  -> 0 -> 'spouse' -> 'first')::TEXT
                            ELSE
                                (temp_json.json_data  -> 0 -> 'employee' -> 'first')::TEXT
                        END,
                        '([^"]+)') 
                    AS "Insured First Name",

                    to_char(
                        to_date(
                            substring(
                                CASE 
                            WHEN
                                applicant_type = 'children' AND
                                children.child_birthdate IS NOT NULL
                                THEN
                                    children.child_birthdate
                            WHEN
                                applicant_type = 'children'
                                THEN
                                    (temp_json.json_data  -> 0 -> 'children' -> 0 -> 'birthdate')::TEXT
                            WHEN 
                                applicant_type = 'spouse' 
                                THEN
                                (temp_json.json_data  -> 0 -> 'spouse' -> 'birthdate')::TEXT
                            ELSE
                                (temp_json.json_data  -> 0 -> 'employee' -> 'birthdate')::TEXT
                            END,
                                '[0-9/]+'), 
                            'MM/DD/YYYY'), 
                        'MM/DD/YYYY') 
                    AS "Insured Birth DOB",

                    trim( 
                        BOTH
                            '[" ]' 
                        FROM 
                            to_char(
                                enrollment_application_coverage.monthly_premium::FLOAT,
                                '999,999,999,990.00'
                            )
                    ) AS "Insured Premium",

                    CASE 
                        WHEN 
                            enrollment_application_coverage.coverage_face_value ~ E'^\\d+$' 
                            THEN
                            trim( BOTH '[" ]' FROM to_char(enrollment_application_coverage.coverage_face_value::FLOAT, '999,999,999,990'))
                        ELSE
                            'Selected'
                    END AS "Insured Coverage",

                    enrollment_applications.agent_code,

                    cases.group_number,

                    cases.company_name
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
                    enrollment_application_coverage 
                        ON 
                            enrollment_applications.id = enrollment_application_coverage.enrollment_application_id 
                        INNER JOIN
                    products 
                        ON 
                            enrollment_application_coverage.product_id = products.id 
                        INNER JOIN
                    temp_json 
                        ON
                            enrollment_applications.id = temp_json.enrollment_applicaiton_id
                        LEFT JOIN
                    products_custom_guaranteed_issue
                        ON
                            products.id = products_custom_guaranteed_issue.id
                        LEFT JOIN
                    products AS base_products
                        ON
                            products_custom_guaranteed_issue.base_product_id = base_products.id
                        LEFT JOIN

                        (SELECT
                            DISTINCT
                            d ->> 'first' AS child_first,
                            d ->> 'last' AS child_last,
                            d ->> 'birthdate' AS child_birthdate,
                            t.enrollment_applicaiton_id AS enrollment_applicaiton_id,
                            (t.json_data -> 0 -> 'child_coverages' -> (row_number() OVER(ORDER BY t.enrollment_applicaiton_id)::INTEGER) ->> 'coverage_selection')::BOOL AS coverage_selection,
                            (t.json_data -> 0 -> 'child_coverages' -> (row_number() OVER(ORDER BY t.enrollment_applicaiton_id)::INTEGER) ->> 'premium')::FLOAT AS premium,
                            (t.json_data -> 0 -> 'child_coverages' -> (row_number() OVER(ORDER BY t.enrollment_applicaiton_id)::INTEGER) ->> 'flat_fee')::FLOAT AS flat_fee
                        FROM
                           temp_json t,
                               json_array_elements(t.json_data -> 0 -> 'children') AS d
                        ) AS children
                        ON
                            children.enrollment_applicaiton_id = temp_json.enrollment_applicaiton_id AND
                            applicant_type = 'children'
                        INNER JOIN
                    enrollment_application_submissions
                        ON
                            enrollment_applications.id = enrollment_application_submissions.enrollment_application_id
                        INNER JOIN
                    enrollment_submissions
                        ON
                            enrollment_application_submissions.enrollment_submission_id = enrollment_submissions.id

                WHERE
                    cases.id = 22 AND
                    NOT enrollment_applications.standardized_data IS NULL AND 
                    NOT cast(enrollment_applications.standardized_data AS TEXT) = 'null' AND 
                    NOT (temp_json.json_data->0->'bank_info') IS NULL AND 
                    enrollment_applications.is_preview != TRUE AND 
                    cases.requires_paylogix_export = TRUE AND 
                    enrollment_application_coverage.monthly_premium > 0 AND 
                    NOT effective_date IS NULL AND 
                    enrollment_applications.signature_time > min AND 
                    enrollment_applications.signature_time < max AND 
                    enrollment_submissions.submission_type = 'Paylogix CSV Generation' and
                    ((pending_only = TRUE AND
                            enrollment_submissions.status = 'pending') OR
                        pending_only = FALSE)
                ORDER BY
                    "Signature Time",
                    "EE SSN",
                    "Product Name" DESC;
            END;
        $$;

    """)


def upgrade():
    op.replace_sp(paylogix_report, replaces='146bba82e494.paylogix_report')

def downgrade():
    op.replace_sp(paylogix_report, replace_with='146bba82e494.paylogix_report')



