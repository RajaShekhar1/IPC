"""add self-enrollment field to cases

Revision ID: 38f2ad8d3b1f
Revises: 536f64a850
Create Date: 2015-05-21 22:31:40.776694

"""

# revision identifiers, used by Alembic.
revision = '38f2ad8d3b1f'
down_revision = '536f64a850'

from alembic import op
import sqlalchemy as sa


def upgrade():
    op.add_column('cases', sa.Column('is_self_enrollment', sa.Boolean(), server_default='FALSE', nullable=False))


def downgrade():
    op.drop_column('cases', 'is_self_enrollment')
