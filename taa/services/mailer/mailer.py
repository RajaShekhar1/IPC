

from sparkpost import SparkPost
from sparkpost.exceptions import SparkPostAPIException

from taa import app


def send_email(to, subject, from_email="noreply@5StarEnroll.com", from_name="5Star Enrollment", html=None, text=None, reply_to=None, track_clicks=True, attachments=None):

    sparkpost = SparkPost(app.config['SPARKPOST_API_KEY'])

    if not attachments:
        # Sparkpost crashes if None is passed in ...
        attachments = []

    try:
        response = sparkpost.transmissions.send(
            recipients=to,
            html=html,
            text=text,
            from_email=from_email,
            from_name=from_name,
            subject=subject,
            reply_to=reply_to,
            transactional=True,
            track_clicks=track_clicks,
            attachments=attachments,
        )
    except SparkPostAPIException as e:
        raise Error(e)


class Error(Exception):
    pass
