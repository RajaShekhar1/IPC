

from sparkpost import SparkPost
from sparkpost.exceptions import SparkPostAPIException

from taa import app


def send_email(to, subject, from_email="noreply@5StarEnroll.com", html=None, text=None, reply_to=None, track_clicks=True):

    sparkpost = SparkPost(app.config['SPARKPOST_API_KEY'])

    try:
        response = sparkpost.transmissions.send(
            recipients=to,
            html=html,
            text=text,
            from_email=from_email,
            subject=subject,
            reply_to=reply_to,
            transactional=True,
            track_clicks=track_clicks,
        )
    except SparkPostAPIException as e:
        raise Error(e)


class Error(Exception):
    pass
