"""Add summary_confirmation_email_log table

Revision ID: bae868f8111f
Revises: 80c3571eb423
Create Date: 2016-07-21 18:08:49.658683

"""

# revision identifiers, used by Alembic.
revision = 'bae868f8111f'
down_revision = '72600a788388'

from alembic import op
import sqlalchemy as sa


def upgrade():
    ### commands auto generated by Alembic - please adjust! ###
    op.create_table('summary_confirmation_email_log',
    sa.Column('id', sa.Integer(), nullable=False),
    sa.Column('enrollment_application_id', sa.Integer(), nullable=False),
    sa.Column('sent_date', sa.DateTime(), nullable=False),
    sa.Column('is_success', sa.Boolean(), nullable=False),
    sa.Column('email_to_address', sa.Unicode(), nullable=True),
    sa.Column('email_to_name', sa.Unicode(), nullable=True),
    sa.Column('email_body', sa.Unicode(), nullable=True),
    sa.Column('status', sa.Unicode(length=16), nullable=True),
    sa.ForeignKeyConstraint(['enrollment_application_id'], ['enrollment_applications.id'], ),
    sa.PrimaryKeyConstraint('id')
    )
    op.create_index(op.f('ix_summary_confirmation_email_log_enrollment_application_id'), 'summary_confirmation_email_log', ['enrollment_application_id'], unique=False)
    op.create_index(op.f('ix_summary_confirmation_email_log_status'), 'summary_confirmation_email_log', ['status'], unique=False)
    ### end Alembic commands ###


def downgrade():
    ### commands auto generated by Alembic - please adjust! ###
    op.drop_index(op.f('ix_summary_confirmation_email_log_status'), table_name='summary_confirmation_email_log')
    op.drop_index(op.f('ix_summary_confirmation_email_log_enrollment_application_id'), table_name='summary_confirmation_email_log')
    op.drop_table('summary_confirmation_email_log')
    ### end Alembic commands ###