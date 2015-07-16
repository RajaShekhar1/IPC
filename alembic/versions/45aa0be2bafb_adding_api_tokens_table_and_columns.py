"""Adding API tokens table and columns

Revision ID: 45aa0be2bafb
Revises: 2fc712362050
Create Date: 2015-07-13 14:43:33.829924

"""

# revision identifiers, used by Alembic.
revision = '45aa0be2bafb'
down_revision = '2fc712362050'

from alembic import op
import sqlalchemy as sa


def upgrade():
    op.create_table('api_tokens',
    sa.Column('id', sa.Integer(), nullable=False),
    sa.Column('api_token', sa.String(length=64), nullable=False),
    sa.Column('activated', sa.Boolean(), nullable=False),
    sa.Column('name', sa.String(), nullable=True),
    sa.Column('stormpath_url', sa.String(), nullable=True),
    sa.PrimaryKeyConstraint('id')
    )


def downgrade():
    op.drop_table('api_tokens')
