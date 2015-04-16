"""add payment mode to enrollments

Revision ID: 39f1429644b8
Revises: 3ae1188fe1f2
Create Date: 2015-04-13 21:35:39.111602

"""

# revision identifiers, used by Alembic.
revision = '39f1429644b8'
down_revision = '3ae1188fe1f2'

from alembic import op
from sqlalchemy.sql import table, column
from sqlalchemy.orm import Session
import sqlalchemy as sa


OLD_TO_NEW_MODES = {
    u'weekly': 52,
    u'monthly': 12,
    u'biweekly': 26,
    u'semimonthly': 24,
    }

NEW_TO_OLD_MODES = dict(zip(OLD_TO_NEW_MODES.values(), OLD_TO_NEW_MODES.keys()))

ModeConvert = table('enrollment_applications',
    column('mode', sa.Unicode(16)),
    column('payment_mode', sa.Integer()),
    )


def convert_mode(old_mode, old_to_new=True):
    if (old_to_new and old_mode in OLD_TO_NEW_MODES) or (not old_to_new and old_mode in NEW_TO_OLD_MODES):
        return OLD_TO_NEW_MODES[old_mode] if old_to_new else NEW_TO_OLD_MODES[old_mode]
    else:
        print("WARNING: Unable to convert '{}' to {} form; using NULL".format(old_mode, 'new' if old_to_new else 'old'))
        return None


def upgrade():
    bind = op.get_bind()
    session = Session(bind=bind)
    op.add_column('enrollment_applications', sa.Column('payment_mode', sa.Integer(), nullable=True))
    # Translate old modes into new modes
    for old_mode, new_mode in OLD_TO_NEW_MODES.iteritems():
        op.execute(
            ModeConvert.update().
                where(ModeConvert.c.mode==op.inline_literal(old_mode)).\
                values({'payment_mode': op.inline_literal(new_mode)})
                )
    op.drop_column('enrollment_applications', 'mode')
    session.commit()


def downgrade():
    bind = op.get_bind()
    session = Session(bind=bind)
    op.add_column('enrollment_applications', sa.Column('mode', sa.Unicode(16), nullable=True))
    # Translate new modes into old modes
    for new_mode, old_mode in NEW_TO_OLD_MODES.iteritems():
        op.execute(
            ModeConvert.update().
                where(ModeConvert.c.payment_mode==op.inline_literal(new_mode)).\
                values({'mode': op.inline_literal(old_mode)})
                )
    op.drop_column('enrollment_applications', 'payment_mode')
    session.commit()
