import pysftp
import paramiko
from StringIO import StringIO

from taa import config_defaults
from taa.services import RequiredFeature


class SFTPServer(object):
    "Container for SFTP Server info."
    def __init__(self, hostname, hostkeyline, username, password, directory, pgp_encryption_key=None):
        self.hostname = hostname
        self.hostkeyline = hostkeyline
        self.username = username
        self.password = password
        self.directory = directory
        self.pgp_encryption_key = pgp_encryption_key


class SFTPService(object):

    encryption_service = RequiredFeature('PGPEncryptionService')
    
    def get_dell_server(self):
        return SFTPServer(config_defaults.DELL_FTP_HOSTNAME,
                                  config_defaults.DELL_HOSTKEY,
                                  username=config_defaults.DELL_FTP_USERNAME,
                                  password=config_defaults.DELL_FTP_PASSWORD,
                                  directory=config_defaults.DELL_FTP_WORKING_DIRECTORY,
                                  # Don't encrypt the files over SFTP.
                                  pgp_encryption_key=None,
                                  #pgp_encryption_key=self.encryption_service.get_dell_key()
        )

    def send_file(self, sftp_server, filename, data):

        opts = pysftp.CnOpts()
        opts.hostkeys = paramiko.HostKeys()
        entry = paramiko.hostkeys.HostKeyEntry.from_line(sftp_server.hostkeyline)
        key_type = sftp_server.hostkeyline.split()[1] if len(sftp_server.hostkeyline.split()) > 1 else 'ssh-dss'
        opts.hostkeys.add(entry.hostnames[0], key_type, entry.key)

        # Encrypt the data if required.
        if sftp_server.pgp_encryption_key:
            data = self.encryption_service.encrypt(data, sftp_server.pgp_encryption_key)

        with pysftp.Connection(sftp_server.hostname, username=sftp_server.username, password=sftp_server.password, cnopts=opts) as sftp:
            
            if sftp_server.directory:
                with sftp.cd(sftp_server.directory):
                    # Upload the data as a stream and name it using the given filename.
                    sftp.putfo(StringIO(data), filename)
            else:
                sftp.putfo(StringIO(data), filename)