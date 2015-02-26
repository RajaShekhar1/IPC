"""added visible boolean on base products

Revision ID: 24d2a377ae6e
Revises: 17f958e1bdcf
Create Date: 2015-01-27 22:27:48.035170

"""

# revision identifiers, used by Alembic.
revision = '24d2a377ae6e'
down_revision = '17f958e1bdcf'

from alembic import op
import sqlalchemy as sa


def upgrade():
    ### commands auto generated by Alembic - please adjust! ###
    op.add_column('products', sa.Column('visible_to_agents', sa.Boolean(), server_default='True', nullable=False))
    ### end Alembic commands ###


def downgrade():
    ### commands auto generated by Alembic - please adjust! ###
    op.drop_column('products', 'visible_to_agents')
    ### end Alembic commands ###
