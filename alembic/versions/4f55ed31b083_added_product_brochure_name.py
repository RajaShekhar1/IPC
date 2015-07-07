"""Added product brochure name

Revision ID: 4f55ed31b083
Revises: 23f9a047d797
Create Date: 2015-07-03 13:49:07.394494

"""

# revision identifiers, used by Alembic.
revision = '4f55ed31b083'
down_revision = '23f9a047d797'

from alembic import op
import sqlalchemy as sa


def upgrade():
    ### commands auto generated by Alembic - please adjust! ###
    op.add_column('products', sa.Column('brochure_name', sa.Unicode(length=256), nullable=True))
    ### end Alembic commands ###


def downgrade():
    ### commands auto generated by Alembic - please adjust! ###
    op.drop_column('products', 'brochure_name')
    ### end Alembic commands ###
