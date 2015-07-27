
from taa import db, app
from taa.manage.InitializeDatabase import init_basic_data
from taa.manage.init_brochures import init_brochures


def before_scenario(context, scenario):

    # Create all tables
    db.drop_all()
    db.create_all()

    # Add basic data
    init_basic_data()
    init_brochures()

    # Initialize the testing app
    app.config['TESTING'] = True
    context.app = app.test_client()


def after_scenario(context, scenario):
    # Clear the session
    db.session.close()
    db.session.remove()
