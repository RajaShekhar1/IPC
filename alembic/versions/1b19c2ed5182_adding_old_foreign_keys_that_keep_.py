"""Adding old foreign keys that keep popping up in autogenerate, also table rename autogenerated

Revision ID: 1b19c2ed5182
Revises: 44e02491e27
Create Date: 2015-07-16 15:53:10.745097

"""

# revision identifiers, used by Alembic.
revision = '1b19c2ed5182'
down_revision = '44e02491e27'

from alembic import op
import sqlalchemy as sa


def upgrade():
    ### commands auto generated by Alembic - please adjust! ###
    op.create_table('form_template_tabs',
    sa.Column('id', sa.Integer(), nullable=False),
    sa.Column('form_template_id', sa.Integer(), nullable=False),
    sa.Column('page', sa.Integer(), nullable=False),
    sa.Column('x', sa.Integer(), nullable=False),
    sa.Column('y', sa.Integer(), nullable=False),
    sa.Column('name', sa.Unicode(), nullable=True),
    sa.Column('type_', sa.Unicode(), nullable=True),
    sa.Column('label', sa.Unicode(), nullable=True),
    sa.Column('is_bold', sa.Boolean(), server_default='FALSE', nullable=True),
    sa.Column('is_italic', sa.Boolean(), server_default='FALSE', nullable=True),
    sa.Column('is_underline', sa.Boolean(), server_default='FALSE', nullable=True),
    sa.Column('custom_type', sa.Unicode(), nullable=True),
    sa.Column('width', sa.Integer(), nullable=True),
    sa.Column('height', sa.Integer(), nullable=True),
    sa.Column('font', sa.Unicode(), nullable=True),
    sa.Column('font_size', sa.Integer(), nullable=True),
    sa.ForeignKeyConstraint(['form_template_id'], ['form_templates.id'], ),
    sa.PrimaryKeyConstraint('id')
    )
    op.drop_table('form_templates_tabs')
    op.create_foreign_key(None, 'enrollment_applications', 'agents', ['agent_id'], ['id'])
    op.create_foreign_key(None, 'self_enrollment_email_log', 'self_enrollment_email_batches', ['batch_id'], ['id'])
    ### end Alembic commands ###


def downgrade():
    ### commands auto generated by Alembic - please adjust! ###
    op.create_table('form_templates_tabs',
    sa.Column('id', sa.INTEGER(), nullable=False),
    sa.Column('form_template_id', sa.INTEGER(), autoincrement=False, nullable=False),
    sa.Column('page', sa.INTEGER(), autoincrement=False, nullable=False),
    sa.Column('x', sa.INTEGER(), autoincrement=False, nullable=False),
    sa.Column('y', sa.INTEGER(), autoincrement=False, nullable=False),
    sa.Column('name', sa.VARCHAR(), autoincrement=False, nullable=True),
    sa.Column('type_', sa.VARCHAR(), autoincrement=False, nullable=True),
    sa.Column('label', sa.VARCHAR(), autoincrement=False, nullable=True),
    sa.Column('is_bold', sa.BOOLEAN(), server_default=sa.text(u'false'), autoincrement=False, nullable=True),
    sa.Column('is_italic', sa.BOOLEAN(), server_default=sa.text(u'false'), autoincrement=False, nullable=True),
    sa.Column('is_underline', sa.BOOLEAN(), server_default=sa.text(u'false'), autoincrement=False, nullable=True),
    sa.Column('custom_type', sa.VARCHAR(), autoincrement=False, nullable=True),
    sa.Column('width', sa.INTEGER(), autoincrement=False, nullable=True),
    sa.Column('height', sa.INTEGER(), autoincrement=False, nullable=True),
    sa.Column('font', sa.VARCHAR(), autoincrement=False, nullable=True),
    sa.Column('font_size', sa.INTEGER(), autoincrement=False, nullable=True),
    sa.ForeignKeyConstraint(['form_template_id'], [u'form_templates.id'], name=u'form_templates_tabs_form_template_id_fkey'),
    sa.PrimaryKeyConstraint('id', name=u'form_templates_tabs_pkey')
    )
    op.drop_table('form_template_tabs')
    ### end Alembic commands ###