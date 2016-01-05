"""add coverage selection column to enrollment application coverage

Revision ID: 55b328b3d2bf
Revises: 214e18d30f8a
Create Date: 2016-01-05 00:31:47.910450

"""

# revision identifiers, used by Alembic.
revision = '55b328b3d2bf'
down_revision = '214e18d30f8a'

from alembic import op
import sqlalchemy as sa


def upgrade():
    op.add_column('enrollment_application_coverage', sa.Column('coverage_selection', sa.Unicode(length=32), nullable=True))


def downgrade():
    op.drop_column('enrollment_application_coverage', 'coverage_selection')
