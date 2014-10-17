Take-An-App 
===========

Developer setup
---------------------

In an ubuntu 14.04 environment (vagrant recommended), do basic install 
and set up a virtual environment for python: 

    sudo apt-get update
    sudo apt-get upgrade
    sudo apt-get install python-dev python-pip postgresql-server-dev-9.3 nginx php5-fpm phppgadmin php5-pgsql git
    sudo pip install virtualenv
    cd <root folder, i.e. /vagrant>
    virtualenv ~/env
    source ~/env/bin/activate
    pip install -r requirements.txt

Then create the database:

    sudo -u postgres createdb -T template0 -E utf-8 taa
    adduser taa
    <type password that matches config file (DATABASE_URI)>
    sudo -u postgres psql template1
    CREATE USER taa WITH PASSWORD '<DB PASSWORD>';
    
Use [alembic](http://alembic.readthedocs.org/en/latest/) to bring the database schema 
up to date structurally:
    
    alembic upgrade head

Now populate the initial data needed to run the site:
    
    python manage.py initialize_db

To run the site, do the following:
    
    python run_server.py
