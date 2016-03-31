"""Increase length of submission type

Revision ID: 33d857a70c39
Revises: 3918b7c2f45f
Create Date: 2016-03-31 15:38:47.350421

"""

# revision identifiers, used by Alembic.
revision = '33d857a70c39'
down_revision = '3918b7c2f45f'

from alembic import op
import sqlalchemy as sa


def upgrade():
    ### commands auto generated by Alembic - please adjust! ###
    op.alter_column('case_census', 'employee_zip',
               existing_type=sa.VARCHAR(length=9),
               type_=sa.String(length=5),
               existing_nullable=True)
    op.alter_column('enrollment_submissions', 'status',
               existing_type=sa.VARCHAR(length=32),
               type_=sa.Unicode(length=64),
               existing_nullable=True,
               existing_server_default=sa.text(u"'pending'::character varying"))
    op.alter_column('enrollment_submissions', 'submission_type',
               existing_type=sa.VARCHAR(length=32),
               type_=sa.Unicode(length=64),
               existing_nullable=True)
    op.alter_column('submission_logs', 'status',
               existing_type=sa.VARCHAR(length=32),
               type_=sa.Unicode(length=64),
               existing_nullable=True,
               existing_server_default=sa.text(u"'success'::character varying"))
    ### end Alembic commands ###


def downgrade():
    ### commands auto generated by Alembic - please adjust! ###
    op.alter_column('submission_logs', 'status',
               existing_type=sa.Unicode(length=64),
               type_=sa.VARCHAR(length=32),
               existing_nullable=True,
               existing_server_default=sa.text(u"'success'::character varying"))
    op.alter_column('enrollment_submissions', 'submission_type',
               existing_type=sa.Unicode(length=64),
               type_=sa.VARCHAR(length=32),
               existing_nullable=True)
    op.alter_column('enrollment_submissions', 'status',
               existing_type=sa.Unicode(length=64),
               type_=sa.VARCHAR(length=32),
               existing_nullable=True,
               existing_server_default=sa.text(u"'pending'::character varying"))
    op.alter_column('case_census', 'employee_zip',
               existing_type=sa.String(length=5),
               type_=sa.VARCHAR(length=9),
               existing_nullable=True)
    ### end Alembic commands ###
