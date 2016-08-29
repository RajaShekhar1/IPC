"""Adding new enrollmet period types

Revision ID: aa6cf43cd024
Revises: b4be94f5565c
Create Date: 2016-05-25 16:22:03.338512

"""

# revision identifiers, used by Alembic.
revision = 'aa6cf43cd024'
down_revision = 'b4be94f5565c'

from alembic import op
import sqlalchemy as sa


def upgrade():
    op.add_column('cases', sa.Column('ongoing_enrollment_type', sa.String(length=16), nullable=True))
    op.add_column('cases', sa.Column('open_enrollment_type', sa.String(length=16), nullable=True))


def downgrade():
    op.drop_column('cases', 'open_enrollment_type')
    op.drop_column('cases', 'ongoing_enrollment_type')
