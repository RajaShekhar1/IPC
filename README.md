Take-An-App 
===========

Running the site locally using a vagrant VM
---------------------

If you have already setup your environment (see Developer Setup below), 


Developer setup
---------------------

In an ubuntu 14.04 environment (vagrant recommended), install system dependencies
and set up a virtual environment for python: 

    sudo apt-get update
    sudo apt-get upgrade
    sudo apt-get install -y python-dev python-pip postgresql-9.3 postgresql-client-9.3 postgresql-server-dev-9.3 nginx php5-fpm phppgadmin php5-pgsql git
    wget -qO- https://toolbelt.heroku.com/install-ubuntu.sh | sh
    sudo pip install virtualenv
    virtualenv ~/env
    source ~/env/bin/activate
    cd /vagrant
    pip install -r requirements.txt
     
To allow deploying to heroku directly from the VM, you need to add the SSH key to heroku:

    heroku keys:add
    
Now create the database:

    sudo -u postgres createdb -T template0 -E utf-8 taa
    sudo adduser taa
    <type password that matches config file (DATABASE_URI)>
    sudo -u postgres psql template1
    CREATE USER taa WITH PASSWORD '<DB PASSWORD>';
   

Use [alembic](http://alembic.readthedocs.org/en/latest/) to bring the database schema 
up to date structurally:
    
    alembic upgrade head

Now populate the initial data needed to run the site:
    
    python manage-taa.py initialize_db
    python manage-taa.py sync_agents

To run the site, do the following:
    
    python run_server.py
    
If you want to override any of the environment config variables, specify them on the command line:

    DEBUG=False ASSETS_DEBUG=False python run_server.py
    
Or, if you want to simulate the heroku environment, create an environment file with any 
configuration overrides you desire and save it as '.env'. Then run the app with

    foreman start
    
This will read the Procfile and run the app similar to how heroku does it. Running flask
directly with python will give you more output on the terminal and also allow you to 
set pdb breakpoints with pdb.set_trace().
    


Deploying to Heroku
-------------------
    
If you have not done so, follow the guide below to set up the git remotes for the deployment Heroku apps.

To deploy the Staging branch to the taa-staging app, run the following from your directory:

    git push staging Staging:master
    heroku run alembic upgrade head --remote=staging
    
To deploy the Production branch to the taa app, execute:

    git push production Production:master
    heroku run alembic upgrade head --remote=production
    
    
 In general, you need to add `--remote=staging` or `--remote=production` to any Heroku CLI command, like `heroku logs`.
    


Heroku Deployment Management and git branches
------------------------------

The apps are currently deployed to the Heroku platform-as-a-service. There are two Heroku apps, taa and taa-staging.

The taa app should be deployed from the Production branch, and the taa-staging app should be deployed from the Staging branch.
All mainstream development should branch off of master, unless applying hotfixes to the live or staging branches.
If a hotfix is deployed, remember to merge it back into master right away.

To deploy a specific branch to a heroku app, you must first configure the app's git remote in your local git config. 
If you already have a "heroku" remote linked to the taa app (that's you, Bill), you should run

    git config -e
    
And change the remote named "heroku" to "production".

Otherwise, add the "production" remote this way:

    git remote add heroku-production git@heroku.com:taa.git
    
And the "staging" remote:

    git remote add heroku-staging git@heroku.com:taa-staging.git
    
After this, your git config should contain:

    [remote "heroku-production"]
        url = https://git.heroku.com/taa.git
        fetch = +refs/heads/*:refs/remotes/heroku-production/*
    [remote "heroku-staging"]
        url = git@heroku.com:taa-staging.git 
        fetch = +refs/heads/*:refs/remotes/heroku-staging/*