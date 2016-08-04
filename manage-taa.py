from flask_script import Manager
from flask_assets import ManageAssets

from taa import app
from taa.manage import InitializeDatabaseCommand, GetDropBoxTokenCommand
from taa.manage.check_case_tokens import CheckCaseTokensCommand
from taa.manage.docusign2taa import DocusignImportCommand
from taa.manage.init_brochures import InitProductBrochures
from taa.manage.scramble_data import ScrambleDataCommand
from taa.manage.sync_agents import SyncAgentsCommand
from taa.assets import init_app as init_assets
from taa.manage.database import ResetDataCommand
from taa.manage.generate_flatfile import CSVToFlatFileCommand
from taa.manage.generate_flatfile_docs import GenFlatFileDocsCommand
from taa.manage.sync_envelopes import SyncEnvelopesCommand
from taa.manage.CaseConversion import CaseConversionCommand

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
manager.add_command("csv_to_flatfile", CSVToFlatFileCommand())
manager.add_command("gen_flatfile_docs", GenFlatFileDocsCommand())
manager.add_command("get_dropbox_token", GetDropBoxTokenCommand())
manager.add_command("sync_envelopes", SyncEnvelopesCommand())
manager.add_command("convert_cases", CaseConversionCommand())
if __name__ == "__main__":
    manager.run()
