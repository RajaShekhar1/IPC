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
    sudo apt-get upgrade -y
    sudo apt-get install -y python-dev python-pip postgresql-9.3 postgresql-client-9.3 postgresql-server-dev-9.3 git rabbitmq-server libyaml-dev libjpeg8-dev libffi-dev libcurl4-openssl-dev
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
    sudo -u postgres createdb -T template0 -E utf-8 taa-test
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
    python manage-taa.py add_brochure_links
    python manage-taa.py import_docusign -f taa/services/enrollments/docusign_xml_templates/demo

To run the site, do the following:
    
    python run_server.py
    
If you want to override any of the environment config variables, specify them on the command line:

    DEBUG=False ASSETS_DEBUG=False python run_server.py
    
Or, if you want to simulate the heroku environment, create an environment file with any 
configuration overrides you desire and save it as '.env'. Then run the app with

    foreman start
    
This will read the Procfile and run the app similar to how heroku does it. Running flask
directly with python will give you more output on the terminal and also allow you to 
set pdb breakpoints with pdb.set\_trace().
    

Running tests
-------------------

There three stages of tests: commit tests (fast, unit-level tests), integration tests (with database or external service),
and end-to-end tests (with browser control). 

The commit and integration tests should be run frequently, and should be passing before merging into master.

To run commit tests, perform the following:

    ./run_commit_tests

This will run some [behave](http://pythonhosted.org/behave/) tests along with plain vanilla unit tests using the nose test runner.

To run the integration tests, make sure you have created a test database named taa-test and run:

    ./run_integration_tests
    
To run the browser tests, make sure firefox is installed using apt-get. For a headless run, also install Xvfb.
 
    ./run_browser_tests

[pytest](https://pytest.org/latest/index.html) is an alternative test runner. To run the tests using pytest, 
ensure you are in the parent TAA directory and install the `taa` app in editable mode:

    pip install -e .

Run the tests with the command:

    py.test


Deploying to Heroku
-------------------
    
If you have not done so, follow the guide below to set up the git remotes for the deployment Heroku apps.

Before pushing to Heroku, you must pre-compile / minify the static assets (javascript and css) by running the following:

    python manage-taa.py assets build

Add the generated files to the repository, commit and push. This is necessary for any heroku environment that has ASSETS_DEBUG as false, because any files generated dynamically by heroku are "transient" and will be deleted by the heroku build system.

Now, To deploy the Staging branch to the taa-staging app, run the following from your directory:

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