from sqlalchemy.dialects.postgresql import JSON

from taa import db


class BackgroundExport(db.Model):
    __tablename__ = 'background_exports'

    id = db.Column(db.Integer, primary_key=True)
    params = db.Column(JSON(none_as_null=False))
    user_href = db.Column(db.UnicodeText)
    case_id = db.Column(db.Integer, db.ForeignKey('cases.id'), nullable=True)

    status = db.Column(db.UnicodeText)
    download_type = db.Column(db.UnicodeText)
    unicode_data = db.Column(db.UnicodeText)
    binary_data = db.Column(db.Binary)

    DOWNLOAD_TYPE_BINARY = u'binary'
    DOWNLOAD_TYPE_UNICODE = u'unicode'

    STATUS_PENDING = u'pending'
    STATUS_PROCESSING = u'processing'
    STATUS_COMPLETE = u'complete'

