"""Renamed column

Revision ID: 2a4c2de08d5c
Revises: 5433c0e5d8ff
Create Date: 2016-01-22 15:31:43.079249

"""

# revision identifiers, used by Alembic.
revision = '2a4c2de08d5c'
down_revision = '5433c0e5d8ff'

from alembic import op
import sqlalchemy as sa


def upgrade():
    ### commands auto generated by Alembic - please adjust! ###
    op.add_column('agent_split_setups', sa.Column('commission_subcount_code', sa.String(), server_default='', nullable=True))
    op.drop_column('agent_split_setups', 'commision_subcount_code')
    ### end Alembic commands ###


def downgrade():
    ### commands auto generated by Alembic - please adjust! ###
    op.add_column('agent_split_setups', sa.Column('commision_subcount_code', sa.VARCHAR(), server_default=sa.text(u"''::character varying"), autoincrement=False, nullable=True))
    op.drop_column('agent_split_setups', 'commission_subcount_code')
    ### end Alembic commands ###
