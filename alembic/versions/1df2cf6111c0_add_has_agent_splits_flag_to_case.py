"""add "has agent splits" flag to case

Revision ID: 1df2cf6111c0
Revises: 72600a788388
Create Date: 2016-07-25 17:46:14.211520

"""

# revision identifiers, used by Alembic.
revision = '1df2cf6111c0'
down_revision = '72600a788388'

from alembic import op
import sqlalchemy as sa


def upgrade():
    op.add_column('cases', sa.Column('has_agent_splits', sa.Boolean(), server_default='FALSE', nullable=False))
    op.execute("UPDATE cases SET has_agent_splits='1' WHERE id IN (SELECT DISTINCT case_id FROM agent_split_setups);")


def downgrade():
    op.drop_column('cases', 'has_agent_splits')
