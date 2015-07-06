"""Add enrolling agent for self enroll

Revision ID: 2fc712362050
Revises: 4f55ed31b083
Create Date: 2015-07-03 17:55:54.148644

"""

# revision identifiers, used by Alembic.
revision = '2fc712362050'
down_revision = '4e71db67b758'

from alembic import op
import sqlalchemy as sa


def upgrade():
    op.add_column('self_enrollment_setups', sa.Column('enrolling_agent_id', sa.Integer(), nullable=True))
    op.create_foreign_key("self_enrollment_setup_enrolling_agent_fk",
                          'self_enrollment_setups', 'agents', ['enrolling_agent_id'], ['id'])


def downgrade():
    op.drop_constraint("self_enrollment_setup_enrolling_agent_fk",
                       'self_enrollment_setups', type_='foreignkey')
    op.drop_column('self_enrollment_setups', 'enrolling_agent_id')
