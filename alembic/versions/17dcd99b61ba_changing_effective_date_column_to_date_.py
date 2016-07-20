"""changing effective_date column to Date type

Revision ID: 17dcd99b61ba
Revises: ac7812ef81a0
Create Date: 2016-07-20 13:30:46.049699

"""

# revision identifiers, used by Alembic.
revision = '17dcd99b61ba'
down_revision = 'ac7812ef81a0'

from alembic import op
import sqlalchemy as sa
from sqlalchemy.dialects import postgresql

def upgrade():
    ### commands auto generated by Alembic - please adjust! ###
    op.alter_column('enrollment_application_coverage', 'effective_date',
               existing_type=postgresql.TIMESTAMP(),
               type_=sa.Date(),
               existing_nullable=True)
    ### end Alembic commands ###


def downgrade():
    ### commands auto generated by Alembic - please adjust! ###
    op.alter_column('enrollment_application_coverage', 'effective_date',
               existing_type=sa.Date(),
               type_=postgresql.TIMESTAMP(),
               existing_nullable=True)
    ### end Alembic commands ###
