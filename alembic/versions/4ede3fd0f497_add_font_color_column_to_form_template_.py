"""add font color column to form template tabs table

Revision ID: 4ede3fd0f497
Revises: 1b19c2ed5182
Create Date: 2015-07-23 16:48:24.116396

"""

# revision identifiers, used by Alembic.
revision = '4ede3fd0f497'
down_revision = '1b19c2ed5182'

from alembic import op
import sqlalchemy as sa


def upgrade():
    op.add_column('form_template_tabs', sa.Column('font_color', sa.Unicode(), nullable=True))


def downgrade():
    op.drop_column('form_template_tabs', 'font_color')
