"""Broke out the GI Criteria into separate table

Revision ID: 5a7a885db111
Revises: 4274968f00fe
Create Date: 2014-12-01 16:06:49.063635

"""

# revision identifiers, used by Alembic.
revision = '5a7a885db111'
down_revision = '4274968f00fe'

from alembic import op
import sqlalchemy as sa


def upgrade():
    ### commands auto generated by Alembic - please adjust! ###
    op.create_table('products_gi_criteria',
    sa.Column('id', sa.Integer(), nullable=False),
    sa.Column('product_id', sa.Integer(), nullable=True),
    sa.Column('applicant_type', sa.Unicode(length=16), nullable=True),
    sa.Column('guarantee_issue_amount', sa.Integer(), nullable=True),
    sa.Column('criteria_age_min', sa.Integer(), nullable=True),
    sa.Column('criteria_age_max', sa.Integer(), nullable=True),
    sa.Column('criteria_height_min', sa.Integer(), nullable=True),
    sa.Column('criteria_height_max', sa.Integer(), nullable=True),
    sa.Column('criteria_weight_min', sa.Integer(), nullable=True),
    sa.Column('criteria_weight_max', sa.Integer(), nullable=True),
    sa.ForeignKeyConstraint(['product_id'], ['products.id'], ),
    sa.PrimaryKeyConstraint('id')
    )
    op.create_table('products_gi_bypass_questions',
    sa.Column('id', sa.Integer(), nullable=False),
    sa.Column('product_id', sa.Integer(), nullable=True),
    sa.Column('question_type_label', sa.Unicode(), nullable=True),
    sa.ForeignKeyConstraint(['product_id'], ['products.id'], ),
    sa.PrimaryKeyConstraint('id')
    )
    op.drop_column(u'products_custom_guaranteed_issue', 'criteria_age_min')
    op.drop_column(u'products_custom_guaranteed_issue', 'guarantee_issue_amount')
    op.drop_column(u'products_custom_guaranteed_issue', 'criteria_weight_min')
    op.drop_column(u'products_custom_guaranteed_issue', 'criteria_height_min')
    op.drop_column(u'products_custom_guaranteed_issue', 'criteria_weight_max')
    op.drop_column(u'products_custom_guaranteed_issue', 'criteria_height_max')
    op.drop_column(u'products_custom_guaranteed_issue', 'criteria_age_max')
    ### end Alembic commands ###


def downgrade():
    ### commands auto generated by Alembic - please adjust! ###
    op.add_column(u'products_custom_guaranteed_issue', sa.Column('criteria_age_max', sa.INTEGER(), autoincrement=False, nullable=True))
    op.add_column(u'products_custom_guaranteed_issue', sa.Column('criteria_height_max', sa.INTEGER(), autoincrement=False, nullable=True))
    op.add_column(u'products_custom_guaranteed_issue', sa.Column('criteria_weight_max', sa.INTEGER(), autoincrement=False, nullable=True))
    op.add_column(u'products_custom_guaranteed_issue', sa.Column('criteria_height_min', sa.INTEGER(), autoincrement=False, nullable=True))
    op.add_column(u'products_custom_guaranteed_issue', sa.Column('criteria_weight_min', sa.INTEGER(), autoincrement=False, nullable=True))
    op.add_column(u'products_custom_guaranteed_issue', sa.Column('guarantee_issue_amount', sa.INTEGER(), autoincrement=False, nullable=True))
    op.add_column(u'products_custom_guaranteed_issue', sa.Column('criteria_age_min', sa.INTEGER(), autoincrement=False, nullable=True))
    op.drop_table('products_gi_bypass_questions')
    op.drop_table('products_gi_criteria')
    ### end Alembic commands ###