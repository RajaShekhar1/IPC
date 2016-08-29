"""Add binary data column to submission

Revision ID: ced85f2cea4e
Revises: 634fd49cd032
Create Date: 2016-08-29 02:51:36.141985

"""

# revision identifiers, used by Alembic.
revision = 'ced85f2cea4e'
down_revision = '634fd49cd032'

from alembic import op
import sqlalchemy as sa


def upgrade():
    ### commands auto generated by Alembic - please adjust! ###
    op.add_column('enrollment_submissions', sa.Column('binary_data', sa.Binary(), nullable=True))
    ### end Alembic commands ###


def downgrade():
    ### commands auto generated by Alembic - please adjust! ###
    op.drop_column('enrollment_submissions', 'binary_data')
    ### end Alembic commands ###