"""Add "is preview?" flag to enrollment applications table

Revision ID: 1bce9ab9956d
Revises: b934f821a42e
Create Date: 2016-08-17 19:08:24.330152

"""

# revision identifiers, used by Alembic.
revision = '1bce9ab9956d'
down_revision = 'b934f821a42e'

from alembic import op
import sqlalchemy as sa


def upgrade():
    op.add_column('enrollment_applications', sa.Column('is_preview', sa.Boolean(), server_default='0', nullable=False))


def downgrade():
    op.drop_column('enrollment_applications', 'is_preview')
