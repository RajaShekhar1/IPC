# 
# timeout: Allow for plenty of time for large census uploads to be processed 
web: gunicorn taa:app --log-file - --timeout 75 --max-requests 1000 
