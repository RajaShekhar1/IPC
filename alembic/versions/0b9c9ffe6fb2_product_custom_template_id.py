"""Product Custom Template Id

Revision ID: 0b9c9ffe6fb2
Revises: 72a680ebb6ad
Create Date: 2016-05-06 10:11:03.651484

"""

# revision identifiers, used by Alembic.
revision = '0b9c9ffe6fb2'
down_revision = '72a680ebb6ad'

from alembic import op
import sqlalchemy as sa


def upgrade():
    ### commands auto generated by Alembic - please adjust! ###
    op.add_column('products', sa.Column('template_id', sa.String(length=64), nullable=True))
    ### end Alembic commands ###


def downgrade():
    ### commands auto generated by Alembic - please adjust! ###
    op.drop_column('products', 'template_id')
    ### end Alembic commands ###
