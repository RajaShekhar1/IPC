#!/usr/bin/python
import pyftpdlib
from pyftpdlib.authorizers import DummyAuthorizer
from pyftpdlib.handlers import TLS_FTPHandler
from pyftpdlib.servers import FTPServer
from pyftpdlib.filesystems import AbstractedFS, FilesystemError
import os
import requests
import sys


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


class TAAHandler(TLS_FTPHandler):
    upload_url = None
    auth_token = None

    def on_file_received(self, filename):
        _, ext = os.path.splitext(filename)
        if not ext or ext not in [".csv", ".flat", ".json"]:
            data_format = "flat"
        else:
            data_format = ext[1:]

        with open(filename, "rb") as f:
            data = f.read()

        url = "{}?auth_token={}&format={}&email_errors=true".format(self.upload_url, self.auth_token, data_format)
        print
        print requests.post(url, data=data).text
        os.remove(filename)


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
