"""Add flag for FPP-Gov products

Revision ID: 33e7fc3b68fe
Revises: 23470384a7b8
Create Date: 2015-09-28 18:31:33.301225

"""

# revision identifiers, used by Alembic.
revision = '33e7fc3b68fe'
down_revision = '23470384a7b8'

from alembic import op
from sqlalchemy.orm import Session
from sqlalchemy.sql import column, select, table
import sqlalchemy as sa


FppGov = table('products',
    column('name', sa.String),
    column('product_type', sa.String),
    column('visible_to_agents', sa.String),
    column('brochure_url', sa.String),
    column('code', sa.String),
    column('is_fpp_gov', sa.Boolean()),
    )


def upgrade():
    bind = op.get_bind()
    session = Session(bind=bind)
    op.add_column('products', sa.Column('is_fpp_gov', sa.Boolean(),
                                        server_default='FALSE', nullable=False))
    # Update FPP-Gov (FPP-White) if it exists
    op.execute(
        FppGov.update().
            where(FppGov.c.code==op.inline_literal('FPP-Gov')).\
            values({'name': 'FPP-White', 'is_fpp_gov': op.inline_literal(True)})
        )
    # Add FPP-Gray and FPP-Blue
    op.execute(FppGov.insert().values(name='FPP-Gray', code='FPPTIY',
                                      is_fpp_gov=op.inline_literal(True),
                                      visible_to_agents=op.inline_literal(False),
                                      brochure_url='http://5starlifeinsurance.com/wp-content/uploads/2015/02/5Star-Life-FPP-TI-brochure.pdf'))
    op.execute(FppGov.insert().values(name='FPP-Blue', code='FPPTIB',
                                      is_fpp_gov=op.inline_literal(True),
                                      visible_to_agents=op.inline_literal(False),
                                      brochure_url='http://5starlifeinsurance.com/wp-content/uploads/2015/02/5Star-Life-FPP-TI-brochure.pdf'))
    session.commit()


def downgrade():
    op.execute(
        FppGov.update().
            where(FppGov.c.code==op.inline_literal('FPP-Gov')). \
            values({'name': 'FPP-Gov'})
    )
    op.drop_column('products', 'is_fpp_gov')
