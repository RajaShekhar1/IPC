from sqlalchemy.dialects.postgresql import JSON

from taa import db

class BackgroundExport(db.Model):
    __tablename__ = 'background_exports'

    id = db.Column(db.Integer, primary_key=True)
    params = db.Column(JSON(none_as_null=False))
    user_href = db.Column(db.UnicodeText)

    status = db.Column(db.UnicodeText)
    download_type = db.Column(db.UnicodeText)
    unicode_data = db.Column(db.UnicodeText)
    binary_data = db.Column(db.Binary)

    DOWNLOAD_TYPE_BINARY = u'binary'
    DOWNLOAD_TYPE_UNICODE = u'unicode'

    STATUS_PENDING = u'pending'
    STATUS_PROCESSING = u'processing'
    STATUS_COMPLETE = u'complete'



class EnrollmentApplication(EnrollmentSerializer, db.Model):
    """Describes an application made for an enrollment"""
    __tablename__ = 'enrollment_applications'

    id = db.Column(db.Integer, primary_key=True)
    case_id = db.Column(db.Integer, db.ForeignKey('cases.id'), nullable=True, index=True)
    case = db.relationship('Case', backref=db.backref('enrollment_records',
                                                      lazy='dynamic'))
    census_record_id = db.Column(db.Integer, db.ForeignKey('case_census.id'),
                                 nullable=False, index=True)
    census_record = db.relationship('CaseCensus',
                                    backref=db.backref(
                                        'enrollment_applications',
                                        lazy='joined'))
    signature_time = db.Column(db.DateTime, index=True)
    signature_city = db.Column(db.UnicodeText)
    signature_state = db.Column(db.Unicode(2))
    identity_token = db.Column(db.UnicodeText)
    identity_token_type = db.Column(db.Unicode(64))

    SIGNATURE_METHOD_DOCUSIGN = u'docusign'