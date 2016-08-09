from ftplib import FTP
from io import StringIO

from taa.config_defaults import PAYLOGIX_FTP_HOSTNAME, PAYLOGIX_FTP_USERNAME, \
    PAYLOGIX_FTP_PASSWORD, PAYLOGIX_FTP_DIRECTORY
from taa.services import RequiredFeature


class FtpServer(object):
    def __init__(self, host, username, password, directory=None, pgp_encryption_key=None):
        self.host = host
        self.username = username
        self.password = password
        self.directory = directory
        # If provided, we want to encrypt any data we send to this server
        self.pgp_encryption_key = pgp_encryption_key


class FtpService(object):
    "Transfer enrollment data to a remote delivery location"

    encryption_service = RequiredFeature("PGPEncryptionService")

    def get_paylogix_server(self):
        return FtpServer(PAYLOGIX_FTP_HOSTNAME, PAYLOGIX_FTP_USERNAME, PAYLOGIX_FTP_PASSWORD,
                                directory=PAYLOGIX_FTP_DIRECTORY,
                                pgp_encryption_key=self.encryption_service.get_paylogix_key(),
                                )

    def send_file(self, ftp_server, filename, data):
        """
        Send a file via FTP to the given server.
        If the server requires encryption, first encrypt the data.
        """

        ftp = FTP()

        # Heroku does not allow active connections, use passive transfers.
        ftp.set_pasv(True)

        # This prints helpful debug output to the logs.
        ftp.set_debuglevel(5)

        ftp.connect(host=ftp_server.host)
        ftp.login(ftp_server.username, ftp_server.password)

        if ftp_server.directory:
            ftp.cwd(ftp_server.directory)

        # Encrypt the data if necessary.
        if ftp_server.pgp_encryption_key:
            data = StringIO(self.encryption_service.encrypt(data, ftp_server.pgp_encryption_key))

        # Transfer the data using the given filename.
        ftp.storlines('STOR {0}'.format(filename), data)
        ftp.close()
