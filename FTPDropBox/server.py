from pyftpdlib.authorizers import DummyAuthorizer
from pyftpdlib.handlers import FTPHandler, TLS_FTPHandler
from pyftpdlib.servers import FTPServer
from pyftpdlib.filesystems import AbstractedFS, FilesystemError
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

    def on_file_received(self, filename):
        url = sys.argv[3]
        user_token = sys.argv[4]
        data_format = filename.split(".")[1]
        if data_format not in ["csv", "flat", "json"]:
            raise FilesystemError("Incorrect data format. Please submit a .csv, .flat, or .json file")
        with open(filename, "rb") as f:
            data = f.read()
        print requests.post("{}?auth_token={}&format={}".format(url, user_token, data_format), data=data).text
        os.remove(filename)

def main():
    authorizer = DummyAuthorizer()
    authorizer.add_user("test", "test", ".", perm="elradfmwM")
    authorizer.add_anonymous(os.getcwd())

    handler = TAAHandler
    handler.certfile = sys.argv[2]
    handler.authorizer = authorizer
    handler.abstracted_fs = TAAFileHandler
    handler.banner = "Take An App - FTP drop box"

    port = sys.argv[1]
    address = ("", port)
    server = FTPServer(address, handler)

    server.max_cons = 256
    server.max_cons_per_ip = 5

    server.serve_forever()

if __name__ == '__main__':
    main()
