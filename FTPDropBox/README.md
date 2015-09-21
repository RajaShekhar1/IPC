Take-An-App FTP DropBox Documentation
===========

Setup
---------------------
Current Deployment
Digital Ocean - account access in passwords document.
Server IP Address: 104.236.231.255
Staging and Production ftp servers



Administration
------------------------------ 

# Restart FTP service
sudo supervisorctl restart all

# Update latest code and restart
git pull <repo/branch>
sudo supervisorctl restart all


Setup Procedure
---------------------

Mostly follow same instructions as the install for local dev, but no need for database setup or initialization
Ubuntu 14.04 LTS
Additional python library dependencies: PyOpenSSL and pyftpdlib
Also need to run pip install -e . from the directory to install the `taa` app in editable mode

NOTE: PyOpenSSL is required in order for TLS_FTPServer to be importable, so install this dependency before pyftpdserver.

Detailed instructions (the python virtualenv directory can be anywhere): 
    
    sudo apt-get update
    sudo apt-get upgrade -y
    sudo apt-get install -y python-dev python-pip git
    sudo pip install virtualenv
    virtualenv ~/env
    source ~/env/bin/activate
    
Then checkout the code to any directory you want (production serve from /var/www, usually local development just shares the folder with TAA).

Install the taa app using:
    
    pip install -r requirements.txt
    pip install -e .
    
Install additional FTP dropbox requirements using

    pip install -r FTPDropBox/requirements.txt

    
Determine the API token for the dropbox using the following command on the server you want it to communicate with:

    heroku run "SQLALCHEMY_ECHO=False python manage-taa.py get_dropbox_token" --app=<heroku_app_name>
   
Or if running locally, just 

    SQLALCHEMY_ECHO=False python manage-taa.py get_dropbox_token
    
This will print out a token for the dropbox to use when communicating with the server using the API.

If deploying, generate a new PEM certificate file by running 
    
    ./FTPDropBox/generatePEM.sh 
    
and answering the questions. 

Now the server can be invoked with (from the TAA directory in the source):

    python FTPDropBox/server.py <port_number> FTPDropBox/taa.pem <upload_endpoint> <api_token>
     
The upload endpoint is the server address followed by '/enrollments'. This is where the DropBox will post uploaded data.

An important environment variable controls which stormpath directory is used for authentication with the FTP server.
By default, it is set to 'TAA-Sandbox'. To override, set STORMPATH_APPLICATION like this:

    STORMPATH_APPLICATION=TAA python FTPDropBox/server.py <port_number> FTPDropBox/taa.pem <upload_endpoint> <api_token>
    
On the production DropBox server, supervisord is used to manage spawning and managing the python process.
    
The configuration looks like this (usually in a file like /etc/supervisor/conf.d/dropbox.conf):

    [program:dropbox]
    environment=STORMPATH_APPLICATION="TAA", ALLOW_DUPLICATE_SUBMISSION="False"
    command=/var/www/env_taa/bin/python /var/www/taa/FTPDropBox/server.py 21 /var/www/taa/FTPDropBox/taa.pem https://5starenroll.com/enrollments PRODUCTION_TOKEN 
    directory=/var/www/taa

