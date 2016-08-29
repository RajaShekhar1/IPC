"""Add "is Paylogic?" flag to enrollment applications table

Revision ID: 44acec40b170
Revises: 9f0469010a26
Create Date: 2016-08-08 19:14:13.718506

"""

# revision identifiers, used by Alembic.
revision = '44acec40b170'
down_revision = '9f0469010a26'

from alembic import op
import sqlalchemy as sa


def upgrade():
    op.add_column('enrollment_applications', sa.Column('is_paylogix', sa.Boolean(), server_default='0', nullable=False))


def downgrade():
    op.drop_column('enrollment_applications', 'is_paylogix')
