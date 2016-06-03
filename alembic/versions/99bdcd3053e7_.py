"""empty message

Revision ID: 99bdcd3053e7
Revises: b5586eedeb
Create Date: 2016-05-03 14:04:46.530043

"""

# revision identifiers, used by Alembic.
revision = '99bdcd3053e7'
down_revision = 'b5586eedeb'

from alembic import op
import sqlalchemy as sa


def upgrade():
    ### commands auto generated by Alembic - please adjust! ###
    op.alter_column('case_census', 'employee_zip',
               existing_type=sa.VARCHAR(length=9),
               type_=sa.String(length=5),
               existing_nullable=True)
    op.add_column('products', sa.Column('flat_fee', sa.Numeric(), nullable=True))
    ### end Alembic commands ###


def downgrade():
    ### commands auto generated by Alembic - please adjust! ###
    op.drop_column('products', 'flat_fee')
    op.alter_column('case_census', 'employee_zip',
               existing_type=sa.String(length=5),
               type_=sa.VARCHAR(length=9),
               existing_nullable=True)
    ### end Alembic commands ###
