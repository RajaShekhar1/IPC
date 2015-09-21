"""Adding case token to case model

Revision ID: 58bb52782fb7
Revises: 45aa0be2bafb
Create Date: 2015-07-13 15:06:10.476209

"""

# revision identifiers, used by Alembic.
revision = '58bb52782fb7'
down_revision = '45aa0be2bafb'

from alembic import op
import sqlalchemy as sa

def upgrade():
    op.add_column('cases', sa.Column('case_token', sa.String(length=64), nullable=True))

def downgrade():
    op.drop_column('cases', 'case_token')
