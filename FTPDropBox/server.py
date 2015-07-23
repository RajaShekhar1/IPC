from pyftpdlib.authorizers import DummyAuthorizer
from pyftpdlib.handlers import FTPHandler, TLS_FTPHandler
from pyftpdlib.servers import FTPServer
from pyftpdlib.filesystems import AbstractedFS
import os
import requests
import urllib
import sys

API_URL = ""

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
    def on_login(self, username):
        print(username)

    def on_file_received(self, f):
        with open(f, "rb") as f:
            pass
            # csv_response = urllib.quote_plus(f.read())
            # response = requests.post(API_URL)
            # import pdb; pdb.set_trace()


def main():
    authorizer = DummyAuthorizer()
    authorizer.add_user("test", "test", ".", perm="elradfmwM")
    authorizer.add_anonymous(os.getcwd())

    handler = TAAHandler
    handler.certfile = sys.argv[2] or 'keyfile.pem'
    handler.authorizer = authorizer
    handler.abstracted_fs = TAAFileHandler
    handler.banner = "Take An App - FTP drop box"

    port = sys.argv[1] or 2100
    address = ("", port)
    server = FTPServer(address, handler)

    server.max_cons = 256
    server.max_cons_per_ip = 5

    server.serve_forever()

if __name__ == '__main__':
    main()
