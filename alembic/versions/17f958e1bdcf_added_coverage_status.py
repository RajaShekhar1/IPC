"""added coverage status

Revision ID: 17f958e1bdcf
Revises: 1f8da1442eba
Create Date: 2015-01-22 23:26:36.388949

"""

# revision identifiers, used by Alembic.
revision = '17f958e1bdcf'
down_revision = '1f8da1442eba'

from alembic import op
import sqlalchemy as sa


def upgrade():
    ### commands auto generated by Alembic - please adjust! ###
    op.add_column('enrollment_application_coverage', sa.Column('coverage_status', sa.Unicode(length=32), nullable=True))
    ### end Alembic commands ###


def downgrade():
    ### commands auto generated by Alembic - please adjust! ###
    op.drop_column('enrollment_application_coverage', 'coverage_status')
    ### end Alembic commands ###