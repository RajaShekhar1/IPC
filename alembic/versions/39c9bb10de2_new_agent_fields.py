"""new agent fields

Revision ID: 39c9bb10de2
Revises: 3cea481a88e1
Create Date: 2015-02-13 01:01:51.448407

"""

# revision identifiers, used by Alembic.
revision = '39c9bb10de2'
down_revision = '3cea481a88e1'

from alembic import op
import sqlalchemy as sa


def upgrade():
    ### commands auto generated by Alembic - please adjust! ###
    op.add_column('enrollment_applications', sa.Column('agent_code', sa.Unicode(length=16), nullable=True))
    op.add_column('enrollment_applications', sa.Column('agent_id', sa.Integer(), nullable=True))
    op.add_column('enrollment_applications', sa.Column('agent_name', sa.Unicode(length=256), nullable=True))
    ### end Alembic commands ###


def downgrade():
    ### commands auto generated by Alembic - please adjust! ###
    op.drop_column('enrollment_applications', 'agent_name')
    op.drop_column('enrollment_applications', 'agent_id')
    op.drop_column('enrollment_applications', 'agent_code')
    ### end Alembic commands ###