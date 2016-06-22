from io import StringIO

import gnupg
from ftplib import FTP
from taa.config_defaults import GNUPG_DIR, DELL_PGP_KEY, PAYLOGIX_PGP_KEY


def pgp_add_key(pgp, key):
    import_result = pgp.import_keys(key)
    # noinspection PyProtectedMember
    if len(import_result.results) == 0 or not all(
            (r.get('status').strip() in import_result._ok_reason.values() for r in import_result.results)):
        raise Exception


class FtpService(object):

    def __initialize_gpg(self):
        if not hasattr(self, '__gpg'):
            self.__gpg = gnupg.GPG(binary=GNUPG_DIR)
            pgp_add_key(self.__gpg, DELL_PGP_KEY)
            pgp_add_key(self.__gpg, PAYLOGIX_PGP_KEY)

    def encrypt(self, data, key_id):
        self.__initialize_gpg()
        return self.__gpg.encrypt(data, key_id)

    def send_file(self, hostname, username, password, filename, data, directory=None, key_id=None):
        """
        Send a file via FTP to the specified hostname, and optionally directory.
        If key_id is present the file stored on the server will be encrypted the key that key_id references.
        """
        #ftp = FTP(host=hostname, user=username, passwd=password)
        ftp = FTP()
        ftp.set_pasv(False)
        ftp.set_debuglevel(5)
        ftp.connect(host=hostname)
        ftp.login(username, password)

        if directory:
            ftp.cwd(directory)
        if key_id:
            data = StringIO(unicode(self.encrypt(data, key_id)))
        
        ftp.storlines('STOR {0}'.format(filename), data)
        ftp.close()
