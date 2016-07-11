# 
# timeout: Allow for plenty of time for large census uploads to be processed 
web: gunicorn taa:app --log-file - --timeout 60 --max-requests 30 --workers 3 --worker-class gevent --sendfile
worker: celery worker --app=taa.tasks.app --without-heartbeat --without-gossip --concurrency=3 --maxtasksperchild=10
clock: python taa/clock.py