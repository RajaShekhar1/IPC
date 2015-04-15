"""Added some census columns

Revision ID: 499a213c2399
Revises: 44cf9c4873ae
Create Date: 2014-10-17 18:31:43.376973

"""

# revision identifiers, used by Alembic.
revision = '499a213c2399'
down_revision = '44cf9c4873ae'

from alembic import op
import sqlalchemy as sa


def upgrade():
    ### commands auto generated by Alembic - please adjust! ###
    op.add_column('case_census', sa.Column('spouse_email', sa.String(length=256), nullable=True))
    ### end Alembic commands ###


def downgrade():
    ### commands auto generated by Alembic - please adjust! ###
    op.drop_column('case_census', 'spouse_email')
    ### end Alembic commands ###