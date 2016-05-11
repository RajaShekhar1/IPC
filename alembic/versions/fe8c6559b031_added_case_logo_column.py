"""added case logo column

Revision ID: fe8c6559b031
Revises: 49198e172f43 
Create Date: 2016-05-06 20:17:23.346475

"""

# revision identifiers, used by Alembic.
revision = 'fe8c6559b031'
down_revision = '49198e172f43'

from alembic import op
import sqlalchemy as sa


def upgrade():
    ### commands auto generated by Alembic - please adjust! ###
    op.add_column('cases', sa.Column('include_cover_sheet', sa.Boolean(), server_default='TRUE', nullable=False))
    op.add_column('cases', sa.Column('logo_image_data', sa.Binary(), nullable=True))
    ### end Alembic commands ###


def downgrade():
    ### commands auto generated by Alembic - please adjust! ###
    op.drop_column('cases', 'logo_image_data')
    op.drop_column('cases', 'include_cover_sheet')
    ### end Alembic commands ###
