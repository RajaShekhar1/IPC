"""Adding unique index to URL and hash lookups

Revision ID: 39386c5ffac9
Revises: 468097537797
Create Date: 2015-11-13 16:02:11.018082

"""

# revision identifiers, used by Alembic.
revision = '39386c5ffac9'
down_revision = '468097537797'

from alembic import op
import sqlalchemy as sa


def upgrade():

    # Drop the indexes we are modifying, Re-create them with the unique=True flag
    op.drop_index('ix_agents_url')
    op.create_index('ix_agents_url', 'agents', ['stormpath_url'], unique=True)

    op.drop_index('ix_api_tokens_token')
    op.create_index('ix_api_tokens_token', 'api_tokens', ['api_token'], unique=True)

    op.drop_index('ix_cases_token')
    op.create_index('ix_cases_token', 'cases', ['case_token'], unique=True)

    op.drop_index('ix_form_templates_template_id')
    op.create_index('ix_form_templates_template_id', 'form_templates', ['template_id'], unique=True)

def downgrade():

    # Drop the modified indexes, Re-create without the unique=True flag
    op.drop_index('ix_agents_url')
    op.create_index('ix_agents_url', 'agents', ['stormpath_url'])

    op.drop_index('ix_api_tokens_token')
    op.create_index('ix_api_tokens_token', 'api_tokens', ['api_token'])

    op.drop_index('ix_cases_token')
    op.create_index('ix_cases_token', 'cases', ['case_token'])

    op.drop_index('ix_form_templates_template_id')
    op.create_index('ix_form_templates_template_id', 'form_templates', ['template_id'], unique=True)


