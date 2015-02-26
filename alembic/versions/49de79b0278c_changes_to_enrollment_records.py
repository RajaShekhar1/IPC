"""Changes to enrollment records

Revision ID: 49de79b0278c
Revises: 31523a310f8f
Create Date: 2015-01-14 21:28:14.847382

"""

# revision identifiers, used by Alembic.
revision = '49de79b0278c'
down_revision = '31523a310f8f'

from alembic import op
import sqlalchemy as sa


def upgrade():
    ### commands auto generated by Alembic - please adjust! ###
    op.create_table('enrollments',
    sa.Column('id', sa.Integer(), nullable=False),
    sa.Column('case_id', sa.Integer(), nullable=True),
    sa.Column('census_record_id', sa.Integer(), nullable=False),
    sa.Column('signature_time', sa.DateTime(), server_default='NOW', nullable=True),
    sa.Column('signature_city', sa.UnicodeText(), nullable=True),
    sa.Column('signature_state', sa.Unicode(length=2), nullable=True),
    sa.Column('identity_token', sa.UnicodeText(), nullable=True),
    sa.Column('identity_token_type', sa.Unicode(length=64), nullable=True),
    sa.Column('did_decline_enrollment', sa.Boolean(), nullable=True),
    sa.Column('mode', sa.Unicode(length=16), nullable=True),
    sa.Column('method', sa.Unicode(length=32), nullable=True),
    sa.Column('is_employee_owner', sa.Boolean(), nullable=True),
    sa.Column('employee_other_owner_name', sa.UnicodeText(), nullable=True),
    sa.Column('employee_other_owner_ssn', sa.Unicode(length=16), nullable=True),
    sa.Column('is_spouse_owner', sa.Boolean(), nullable=True),
    sa.Column('spouse_other_owner_name', sa.UnicodeText(), nullable=True),
    sa.Column('spouse_other_owner_ssn', sa.Unicode(length=16), nullable=True),
    sa.Column('is_employee_beneficiary_spouse', sa.Boolean(), nullable=True),
    sa.Column('employee_beneficiary_name', sa.UnicodeText(), nullable=True),
    sa.Column('employee_beneficiary_relationship', sa.UnicodeText(), nullable=True),
    sa.Column('employee_beneficiary_birthdate', sa.UnicodeText(), nullable=True),
    sa.Column('employee_beneficiary_ssn', sa.Unicode(length=16), nullable=True),
    sa.Column('is_spouse_beneficiary_employee', sa.Boolean(), nullable=True),
    sa.Column('spouse_beneficiary_name', sa.UnicodeText(), nullable=True),
    sa.Column('spouse_beneficiary_relationship', sa.UnicodeText(), nullable=True),
    sa.Column('spouse_beneficiary_birthdate', sa.UnicodeText(), nullable=True),
    sa.Column('spouse_beneficiary_ssn', sa.Unicode(length=16), nullable=True),
    sa.ForeignKeyConstraint(['case_id'], ['cases.id'], ),
    sa.ForeignKeyConstraint(['census_record_id'], ['case_census.id'], ),
    sa.PrimaryKeyConstraint('id')
    )
    op.create_table('enrollment_coverage',
    sa.Column('id', sa.Integer(), nullable=False),
    sa.Column('enrollment_id', sa.Integer(), nullable=False),
    sa.Column('product_id', sa.Integer(), nullable=True),
    sa.Column('applicant_type', sa.Unicode(length=32), nullable=True),
    sa.Column('height_inches', sa.Integer(), nullable=True),
    sa.Column('weight_pounds', sa.Integer(), nullable=True),
    sa.Column('is_smoker', sa.Boolean(), nullable=True),
    sa.Column('coverage_face_value', sa.Unicode(length=256), nullable=True),
    sa.Column('weekly_premium', sa.Numeric(), nullable=True),
    sa.Column('biweekly_premium', sa.Numeric(), nullable=True),
    sa.Column('monthly_premium', sa.Numeric(), nullable=True),
    sa.Column('annual_premium', sa.Numeric(), nullable=True),
    sa.Column('soh_answers', sa.UnicodeText(), nullable=True),
    sa.ForeignKeyConstraint(['enrollment_id'], ['enrollments.id'], ),
    sa.ForeignKeyConstraint(['product_id'], ['products.id'], ),
    sa.PrimaryKeyConstraint('id')
    )
    ### end Alembic commands ###


def downgrade():
    ### commands auto generated by Alembic - please adjust! ###
    op.drop_table('enrollment_coverage')
    op.drop_table('enrollments')
    ### end Alembic commands ###
