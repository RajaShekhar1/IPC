from flask_script import Manager
from flask_assets import ManageAssets

from taa import app
from taa.manage import InitializeDatabaseCommand
from taa.manage.check_case_tokens import CheckCaseTokensCommand
from taa.manage.docusign2taa import DocusignImportCommand
from taa.manage.init_brochures import InitProductBrochures
from taa.manage.scramble_data import ScrambleDataCommand
from taa.manage.sync_agents import SyncAgentsCommand
from taa.assets import init_app as init_assets
from taa.manage.database import ResetDataCommand

manager = Manager(app)
manager.add_command('initialize_db', InitializeDatabaseCommand())
assets_env = init_assets(app)
manager.add_command("assets", ManageAssets(assets_env))
manager.add_command("sync_agents", SyncAgentsCommand())
manager.add_command("add_brochure_links", InitProductBrochures())
manager.add_command("check_case_tokens", CheckCaseTokensCommand())
manager.add_command("scramble_data", ScrambleDataCommand())
manager.add_command("import_docusign", DocusignImportCommand())
manager.add_command("clear_and_reset_database", ResetDataCommand())

if __name__ == "__main__":
    manager.run()
