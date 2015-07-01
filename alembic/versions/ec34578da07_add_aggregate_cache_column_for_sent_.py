"""add aggregate cache column for sent_email

Revision ID: ec34578da07
Revises: 23f9a047d797
Create Date: 2015-07-01 20:39:36.340874

"""

# revision identifiers, used by Alembic.
revision = 'ec34578da07'
down_revision = '23f9a047d797'

from alembic import op
import sqlalchemy as sa


def upgrade():
    op.add_column('case_census', sa.Column('sent_email_count', sa.Integer(), nullable=True))


def downgrade():
    op.drop_column('case_census', 'sent_email_count')
