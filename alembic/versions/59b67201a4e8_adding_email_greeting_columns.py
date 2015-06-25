"""Adding email greeting columns

Revision ID: 59b67201a4e8
Revises: 2c3339065a26
Create Date: 2015-06-24 19:44:07.229304

"""

# revision identifiers, used by Alembic.
revision = '59b67201a4e8'
down_revision = '2c3339065a26'

from alembic import op
import sqlalchemy as sa


def upgrade():
    op.add_column('self_enrollment_setups', sa.Column('email_greeting_salutation', sa.Unicode(), nullable=True))
    op.add_column('self_enrollment_setups', sa.Column('email_greeting_type', sa.Unicode(length=16), nullable=True))


def downgrade():
    op.drop_column('self_enrollment_setups', 'email_greeting_type')
    op.drop_column('self_enrollment_setups', 'email_greeting_salutation')
