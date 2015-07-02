"""Adding Self Enrollment Email batch table and columns.

Revision ID: 4207ba1a44fc
Revises: 23f9a047d797
Create Date: 2015-07-01 18:00:33.528480

"""

# revision identifiers, used by Alembic.
revision = '4207ba1a44fc'
down_revision = '23f9a047d797'

from alembic import op
import sqlalchemy as sa


def upgrade():
    ### commands auto generated by Alembic - please adjust! ###
    op.create_table('self_enrollment_email_batches',
    sa.Column('id', sa.Integer(), nullable=False),
    sa.Column('agent_id', sa.Integer(), nullable=False),
    sa.Column('email_from_address', sa.Unicode(), nullable=True),
    sa.Column('email_from_name', sa.Unicode(), nullable=True),
    sa.Column('email_subject', sa.Unicode(), nullable=True),
    sa.Column('email_body', sa.UnicodeText(), nullable=True),
    sa.Column('sent_date', sa.DateTime(), nullable=False),
    sa.ForeignKeyConstraint(['agent_id'], ['agents.id'], ),
    sa.PrimaryKeyConstraint('id')
    )
    op.add_column(u'self_enrollment_email_log', sa.Column('batch_id', sa.Integer(), nullable=True))
    op.create_foreign_key(None, 'self_enrollment_email_log', 'self_enrollment_email_batches', ['batch_id'], ['id'])
    op.drop_column(u'self_enrollment_email_log', 'email_from_address')
    op.drop_column(u'self_enrollment_email_log', 'email_from_name')
    op.drop_column(u'self_enrollment_email_log', 'email_subject')
    op.drop_column(u'self_enrollment_email_log', 'email_body')
    ### end Alembic commands ###


def downgrade():
    ### commands auto generated by Alembic - please adjust! ###
    op.add_column(u'self_enrollment_email_log', sa.Column('email_body', sa.TEXT(), autoincrement=False, nullable=True))
    op.add_column(u'self_enrollment_email_log', sa.Column('email_subject', sa.VARCHAR(), autoincrement=False, nullable=True))
    op.add_column(u'self_enrollment_email_log', sa.Column('email_from_name', sa.VARCHAR(), autoincrement=False, nullable=True))
    op.add_column(u'self_enrollment_email_log', sa.Column('email_from_address', sa.VARCHAR(), autoincrement=False, nullable=True))
    op.drop_constraint(None, 'self_enrollment_email_log', type_='foreignkey')
    op.drop_column(u'self_enrollment_email_log', 'batch_id')
    op.drop_table('self_enrollment_email_batches')
    ### end Alembic commands ###
