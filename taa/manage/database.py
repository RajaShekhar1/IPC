from flask import current_app
from flask_script import Command, prompt, Option

from sqlalchemy import create_engine, MetaData

class ResetDataCommand(Command):
    """Resets the current database """
    option_list = (
        Option(dest='db_url',
               help="SQLALCHEMY_URL of database to clear and reset"),
    )

    def run(self, db_url):
        ans = prompt("Are you sure you want to clear and reset the {} database? [y/n]".format(db_url))

        if ans in 'Yy':
            clear_database(db_url)


def clear_database(db_url):
    engine = create_engine(db_url)
    meta = MetaData(bind=engine, reflect=True)
    with engine.begin() as connection:
        for table in reversed(meta.sorted_tables):
            connection.execute(table.delete())
