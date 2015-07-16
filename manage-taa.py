from flask_script import Manager
from flask_assets import ManageAssets

from taa import app
from taa.manage import InitializeDatabaseCommand
from taa.manage.sync_agents import SyncAgentsCommand
from taa.manage.scramble_data import ScrambleDataCommand
from taa.manage.init_brochures import InitProductBrochures
from taa.assets import init_app as init_assets

manager = Manager(app)
manager.add_command('initialize_db', InitializeDatabaseCommand())
assets_env = init_assets(app)
manager.add_command("assets", ManageAssets(assets_env))
manager.add_command("sync_agents", SyncAgentsCommand())
manager.add_command("add_brochure_links", InitProductBrochures())
manager.add_command("scramble_data", ScrambleDataCommand())

if __name__ == "__main__":
    manager.run()
