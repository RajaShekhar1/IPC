"""add payment mode to case

Revision ID: 3ae1188fe1f2
Revises: 2de309fd0302
Create Date: 2015-04-10 23:07:54.755772

"""

# revision identifiers, used by Alembic.
revision = '3ae1188fe1f2'
down_revision = '2de309fd0302'

from alembic import op
import sqlalchemy as sa


def upgrade():
    op.add_column('cases', sa.Column('payment_mode', sa.INTEGER(), autoincrement=False, nullable=True))


def downgrade():
    op.drop_column('cases', 'payment_mode')
