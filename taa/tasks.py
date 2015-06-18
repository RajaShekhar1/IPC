# Celery tasks

import celery
app = Celery('email')

@app.task
def send_email(x, y):
    return x + y

