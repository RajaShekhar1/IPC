"""Add case_riders column to case

Revision ID: 438f1de3b4bf
Revises: 44c62ed2572a
Create Date: 2015-08-05 17:03:10.771963

"""

# revision identifiers, used by Alembic.
revision = '438f1de3b4bf'
down_revision = '44c62ed2572a'

from alembic import op
import sqlalchemy as sa


def upgrade():
    ### commands auto generated by Alembic - please adjust! ###
    op.add_column('cases', sa.Column('case_riders', sa.String(length=64), nullable=True))
    ### end Alembic commands ###


def downgrade():
    ### commands auto generated by Alembic - please adjust! ###
    op.drop_column('cases', 'case_riders')
    ### end Alembic commands ###
