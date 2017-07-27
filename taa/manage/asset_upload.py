
from flask import current_app

from flask_script import Command, Option

import flask_s3


class AssetUploadCommand(Command):
    option_list = (
        Option('-a', '--aws-access-key', dest='aws_access_key_id', required=False,
               help="API key AWS"),
        Option('-s', '--aws-secret', dest='aws_secret', required=False,
               help="API secret for AWS"),
        
        Option('-b', '--aws-bucket-name', dest='bucket_name', required=False,
               help="Bucket Name for AWS"),
    )
    
    def run(self, aws_access_key_id=None, aws_secret=None, bucket_name=None):
        
        if aws_access_key_id:
            current_app.config['AWS_ACCESS_KEY_ID'] = aws_access_key_id
        
        if aws_secret:
            current_app.config['AWS_SECRET_ACCESS_KEY'] = aws_secret
            
        if bucket_name:
            current_app.config['FLASKS3_BUCKET_NAME'] = bucket_name
        
        
        flask_s3.create_all(current_app)