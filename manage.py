from flask_script import Manager
from flask_assets import ManageAssets

from taa import app
from taa.manage import InitializeDatabaseCommand

manager = Manager(app)
manager.add_command('initialize_db', InitializeDatabaseCommand())

from taa.assets import init_app as init_assets
assets_env = init_assets(app)
manager.add_command("assets", ManageAssets(assets_env))

if __name__ == "__main__":
    manager.run()