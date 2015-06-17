"""add success column to self-enrollment email log

Revision ID: a0d114586a9
Revises: 51a3684ca671
Create Date: 2015-06-17 20:35:25.813648

"""

# revision identifiers, used by Alembic.
revision = 'a0d114586a9'
down_revision = '51a3684ca671'

from alembic import op
import sqlalchemy as sa


def upgrade():
    op.add_column('self_enrollment_email_log', sa.Column('is_success', sa.Boolean(), nullable=False))


def downgrade():
    op.drop_column('self_enrollment_email_log', 'is_success')
