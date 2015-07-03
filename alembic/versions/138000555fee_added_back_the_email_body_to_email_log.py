"""added back the email body to email log

Revision ID: 138000555fee
Revises: 54c74718997d
Create Date: 2015-07-03 00:00:40.166342

"""

# revision identifiers, used by Alembic.
revision = '138000555fee'
down_revision = '54c74718997d'

from alembic import op
import sqlalchemy as sa


def upgrade():
    op.add_column('self_enrollment_email_log', sa.Column('email_body', sa.Unicode(), nullable=True))


def downgrade():
    op.drop_column('self_enrollment_email_log', 'email_body')
