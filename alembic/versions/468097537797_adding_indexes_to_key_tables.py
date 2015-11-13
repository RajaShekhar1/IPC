"""Adding indexes to key tables

Revision ID: 468097537797
Revises: 236e1a6732de
Create Date: 2015-11-12 20:44:07.791650

"""

# revision identifiers, used by Alembic.
revision = '468097537797'
down_revision = '236e1a6732de'

from alembic import op
import sqlalchemy as sa


def upgrade():

    # Agents by code or URL
    op.create_index('ix_agents_code', 'agents', ['agent_code'])
    op.create_index('ix_agents_url', 'agents', ['stormpath_url'])

    # Agent products - lookup by agent
    op.create_index('ix_product_agents_agent', 'product_agents', ['agent_id'])
    op.create_index('ix_product_restricted_agents_agent',
                    'product_restricted_agents', ['agent_id'])

    # Get an API token by token or stormpath_url
    op.create_index('ix_api_tokens_token', 'api_tokens', ['api_token'])
    op.create_index('ix_api_tokens_url', 'api_tokens', ['stormpath_url'])

    # Create an index for looking up cases by product
    op.create_index('ix_cases_product_product', 'case_products', ['product_id'])

    # Lookup cases by agent
    op.create_index('ix_case_partners_agent', 'case_partner_agents', ['agent_id'])

    # Get all enrollment periods for a case
    op.create_index('ix_case_enrollment_periods_case', 'case_enrollment_periods', ['case_id'])

    # Get census records for a case
    op.create_index('ix_case_census_records_case', 'case_census', ['case_id'])

    # Active cases
    op.create_index('ix_cases_active', 'cases', ['active'])
    # Cases by token
    op.create_index('ix_cases_token', 'cases', ['case_token'])

    # Products
    op.create_index('ix_products_code', 'products', ['code'])

    # Custom product question settings
    op.create_index('ix_products_gi_bypass_questions_product',
                    'products_gi_bypass_questions', ['product_id'])

    # Custom product criteria
    op.create_index('ix_product_criteria_product', 'products_gi_criteria', ['product_id'])

    # enrollments
    op.create_index('ix_enrollment_applications_case',
                    'enrollment_applications',
                    ['case_id'])
    op.create_index('ix_enrollment_applications_census_record',
                    'enrollment_applications',
                    ['census_record_id'])

    # Coverages
    op.create_index('ix_enrollment_app_coverages_enrollment_id',
                    'enrollment_application_coverage', ['enrollment_application_id'])


    # Enrollment batches
    op.create_index('ix_enrollment_batch_items_batch', 'enrollment_import_batch_items',
                    ['enrollment_batch_id'])
    op.create_index('ix_enrollment_batches_hash', 'enrollment_import_batches', ['log_hash'])
    op.create_index('ix_enrollment_batches_case', 'enrollment_import_batches', ['case_token'])

    # Form template tabs
    op.create_index('ix_form_template_tabs_template', 'form_template_tabs', ['form_template_id'])

    # Form templates
    op.create_index('ix_form_templates_template_id', 'form_templates', ['template_id'])

    # case email batches
    op.create_index('ix_self_enroll_email_batches_case', 'self_enrollment_email_batches', ['case_id'])
    op.create_index('ix_email_log_batch', 'self_enrollment_email_log', ['batch_id'])
    op.create_index('ix_email_log_link', 'self_enrollment_email_log', ['link_id'])

    # link by URL (token)
    op.create_index('ix_self_enroll_link_url', 'self_enrollment_links', ['url'])

    # Setup record by case_id
    op.create_index('ix_self_enroll_setup_case', 'self_enrollment_setups', ['case_id'])


def downgrade():

    op.drop_index('ix_agents_code')
    op.drop_index('ix_agents_url')
    op.drop_index('ix_product_agents_agent')
    op.drop_index('ix_product_restricted_agents_agent')
    op.drop_index('ix_api_tokens_token')
    op.drop_index('ix_api_tokens_url')
    op.drop_index('ix_cases_product_product')
    op.drop_index('ix_case_partners_agent')
    op.drop_index('ix_case_enrollment_periods_case')
    op.drop_index('ix_case_census_records_case')
    op.drop_index('ix_cases_active')
    op.drop_index('ix_cases_token')
    op.drop_index('ix_products_code')
    op.drop_index('ix_products_gi_bypass_questions_product')
    op.drop_index('ix_product_criteria_product')
    op.drop_index('ix_enrollment_applications_case')
    op.drop_index('ix_enrollment_applications_census_record')
    op.drop_index('ix_enrollment_app_coverages_enrollment_id')
    op.drop_index('ix_enrollment_batch_items_batch')
    op.drop_index('ix_enrollment_batches_hash')
    op.drop_index('ix_enrollment_batches_case')
    op.drop_index('ix_form_template_tabs_template')
    op.drop_index('ix_form_templates_template_id')
    op.drop_index('ix_self_enroll_email_batches_case')
    op.drop_index('ix_email_log_batch')
    op.drop_index('ix_email_log_link')
    op.drop_index('ix_self_enroll_link_url')
    op.drop_index('ix_self_enroll_setup_case')
