"""Add is_stp to case model

Revision ID: 59b5226b023f
Revises: 1dd6ec928db9
Create Date: 2016-01-22 17:25:43.583563

"""

# revision identifiers, used by Alembic.
revision = '59b5226b023f'
down_revision = '1dd6ec928db9'

from alembic import op
import sqlalchemy as sa


def upgrade():
    ### commands auto generated by Alembic - please adjust! ###
    op.add_column('cases', sa.Column('is_stp', sa.Boolean(), nullable=True))
    ### end Alembic commands ###


def downgrade():
    ### commands auto generated by Alembic - please adjust! ###
    op.drop_column('cases', 'is_stp')
    ### end Alembic commands ###