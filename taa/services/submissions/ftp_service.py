from io import StringIO

import gnupg
from ftplib import FTP
from taa.config_defaults import DELL_FTP_HOSTNAME, DELL_FTP_USERNAME, DELL_FTP_PASSWORD, GNUPG_DIR, DELL_PGP_KEY, \
    DELL_FTP_WORKING_DIRECTORY, DELL_FTP_PGP_KEY_ID, PAYLOGIX_FTP_HOSTNAME, PAYLOGIX_FTP_USERNAME, \
    PAYLOGIX_FTP_PASSWORD, PAYLOGIX_PGP_KEY


def pgp_add_key(pgp, key):
    import_result = pgp.import_keys(key)
    # noinspection PyProtectedMember
    if len(import_result.results) == 0 or not all(
            (r.get('status').strip() in import_result._ok_reason.values() for r in import_result.results)):
        raise Exception


class FtpService(object):
    def __init__(self):
        """
        Initialize the object with a new instance of gnupg and add the Dell and Paylogix public keys to it
        """
        self.__gpg = gnupg.GPG(binary=GNUPG_DIR)
        pgp_add_key(self.__gpg, DELL_PGP_KEY)
        pgp_add_key(self.__gpg, PAYLOGIX_PGP_KEY)

    def send_file(self, hostname, username, password, filename, data, directory=None, key_id=None):
        """
        Send a file via FTP to the specified hostname, and optionally directory.
        If key_id is present the file stored on the server will be encrypted the key that key_id references.

        :param hostname: Hostname of the FTP server
        :param username: Username to login with
        :param password: Password to login with
        :param filename: Filename to store the data as
        :param data: The data to store
        :param directory: Optional directory to store the data in
        :param key_id: Optional public key id to use for encryption.
        :return:
        """
        ftp = FTP(host=hostname, user=username, passwd=password)
        ftp.set_pasv(False)
        if directory:
            ftp.cwd(directory)
        if key_id:
            data = StringIO(self.__gpg.encrypt(data, key_id))
        ftp.storbinary('STOR {0}' % filename, data)
        ftp.close()
