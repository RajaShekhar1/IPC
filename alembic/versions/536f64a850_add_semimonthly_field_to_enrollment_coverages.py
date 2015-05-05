"""add semimonthly field to enrollment coverages

Revision ID: 536f64a850
Revises: 39f1429644b8
Create Date: 2015-05-05 18:05:04.508644

"""

# revision identifiers, used by Alembic.
revision = '536f64a850'
down_revision = '39f1429644b8'

from alembic import op
import sqlalchemy as sa


def upgrade():
    op.add_column('enrollment_application_coverage', sa.Column('semimonthly_premium', sa.Numeric(), nullable=True))


def downgrade():
    op.drop_column('enrollment_application_coverage', 'semimonthly_premium')
