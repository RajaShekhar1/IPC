"""add product brochure URL to product table

Revision ID: 262cd0a658b0
Revises: a0d114586a9
Create Date: 2015-06-22 19:09:13.294535

"""

# revision identifiers, used by Alembic.
revision = '262cd0a658b0'
down_revision = 'a0d114586a9'

from alembic import op
import sqlalchemy as sa


def upgrade():
    op.add_column('products', sa.Column('brochure_url', sa.Unicode(length=2000), nullable=True))


def downgrade():
    op.drop_column('products', 'brochure_url')
