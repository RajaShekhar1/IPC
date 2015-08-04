#!/usr/bin/python
import pyftpdlib
from pyftpdlib.authorizers import DummyAuthorizer
from pyftpdlib.handlers import TLS_FTPHandler
from pyftpdlib.servers import FTPServer
from pyftpdlib.filesystems import AbstractedFS, FilesystemError
import os
import requests
import sys
from io import BytesIO
from StringIO import StringIO
from taa.services.data_import.file_import import FlatFileSpec


class TAAFileHandler(AbstractedFS):
    def listdir(self, path):
        return []

    def chdir(self, path):
        pass

    def mkdir(self, path):
        pass

    def rmdir(self, path):
        pass

    def remove(self, path):
        pass

    def rename(self, src, dst):
        pass

    def chmod(self, path, mode):
        pass

    def mkstemp(self, suffix='', prefix='', dir=None, mode='wb'):
        """Override the filesystem behavior and use an in-memory stream."""
        if 'b' in mode:
            return BytesIO()
        else:
            return StringIO()

    def open(self, filename, mode):
        """Override the filesystem behavior and use an in-memory stream."""
        class MemoryFile(StringIO):
            def __init__(self, file_obj, name, *args, **kwargs):
                self.wrapped_file = file_obj
                self.name = name

                StringIO.__init__(self, *args, **kwargs)

            def write(self, data):
                self.wrapped_file.write(data)

            def seek(self, pos):
                self.wrapped_file.seek(pos)

            def close(self):
                """
                Override close implementation so the FTP code doesn't close it.
                We have to remember to close the underlying file in our own handler.
                """
                pass

        f = StringIO()
        f_wrap = MemoryFile(f, filename)
        return f_wrap


class TAAHandler(TLS_FTPHandler):
    upload_url = None
    auth_token = None

    def on_file_received(self, filename):
        # Access the in-memory file we create in our FS class.
        data = self.get_file_data()
        data_format = self.get_data_format(data)
        self.post_api_request(data, data_format)

    def post_api_request(self, data, data_format):
        url = "{}?auth_token={}&format={}&email_errors=true".format(self.upload_url, self.auth_token, data_format)
        print
        print requests.post(url, data=data).text

    def get_file_data(self):
        file_obj = self.data_channel.file_obj.wrapped_file
        data = file_obj.getvalue()
        file_obj.close()
        return data

    def get_data_format(self, data):
        if len(data) < 14:
            raise Exception("Bad file format")

        data_header = data[:14]
        if data_header == FlatFileSpec.FLAT_FILE_TYPE:
            data_format = "flat"
        else:
            data_format = "csv"
        return data_format


access_credentials = [
    ('taa-enroller', 'F19K7z49459G'),
]


def main(port, certfile, upload_url, auth_token):

    # Users have "list" and "put" permissions only.
    authorizer = DummyAuthorizer()
    for username, passwd in access_credentials:
        authorizer.add_user(username, passwd, ".", perm="lw")
    authorizer.add_anonymous(os.getcwd())

    handler = TAAHandler

    handler.auth_token = auth_token
    handler.upload_url = upload_url
    handler.certfile = certfile
    handler.authorizer = authorizer
    handler.abstracted_fs = TAAFileHandler
    handler.banner = "Take An App - Secure FTP drop box"

    address = ("0.0.0.0", port)
    server = FTPServer(address, handler)
    server.max_cons = 256
    server.max_cons_per_ip = 5

    server.serve_forever()


if __name__ == '__main__':
    if not len(sys.argv) == 5:
        print("Usage: {} port certfile upload_endpoint user_token".format(sys.argv[0]))
        sys.exit(1)

    port = sys.argv[1]
    certfile = sys.argv[2]
    upload_url = sys.argv[3]
    auth_token = sys.argv[4]

    main(port, certfile, upload_url, auth_token)
