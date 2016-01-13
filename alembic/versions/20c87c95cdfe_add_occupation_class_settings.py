"""add occupation class

Revision ID: 20c87c95cdfe
Revises: 55b328b3d2bf
Create Date: 2016-01-13 19:35:52.390010

"""

# revision identifiers, used by Alembic.
revision = '20c87c95cdfe'
down_revision = '55b328b3d2bf'

from alembic import op
import sqlalchemy as sa
from sqlalchemy.dialects import postgresql

def upgrade():
    op.add_column('case_census', sa.Column('occupation_class', sa.String(length=256), nullable=True))
    op.add_column('cases', sa.Column('occupation_class_settings', postgresql.JSON(), nullable=True))


def downgrade():
    op.drop_column('cases', 'occupation_class_settings')
    op.drop_column('case_census', 'occupation_class')
