"""Add docusign envelope id and signing status

Revision ID: 402434168457
Revises: 214e18d30f8a
Create Date: 2016-01-04 14:33:17.169979

"""

# revision identifiers, used by Alembic.
revision = '402434168457'
down_revision = '214e18d30f8a'

from alembic import op
import sqlalchemy as sa


def upgrade():
    ### commands auto generated by Alembic - please adjust! ###
    op.add_column('enrollment_applications', sa.Column('docusign_envelope_id', sa.Unicode(length=128), nullable=True))
    op.add_column('enrollment_applications', sa.Column('signing_status', sa.Unicode(length=32), nullable=True))
    ### end Alembic commands ###


def downgrade():
    ### commands auto generated by Alembic - please adjust! ###
    op.drop_column('enrollment_applications', 'signing_status')
    op.drop_column('enrollment_applications', 'docusign_envelope_id')
    ### end Alembic commands ###