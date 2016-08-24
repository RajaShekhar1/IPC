# 
# timeout: Allow for plenty of time for large census uploads to be processed 
web: gunicorn taa:app --log-file - --timeout 60 --max-requests 30 --workers 3 --worker-class gevent --sendfile
worker: bin/proximo celery worker --app=taa.tasks.celery --without-heartbeat --without-gossip --concurrency=1 --maxtasksperchild=10
