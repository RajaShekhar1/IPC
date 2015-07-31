#!/usr/bin/python
from pyftpdlib.authorizers import DummyAuthorizer
from pyftpdlib.handlers import FTPHandler, TLS_FTPHandler
from pyftpdlib.servers import FTPServer
from pyftpdlib.filesystems import AbstractedFS, FilesystemError
import os
import requests
import sys

port = ""
certfile = ""
upload_url = ""
user_token = ""

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
    def on_file_received(self, filename):
        _, ext = os.path.splitext(filename)
        if not ext or ext not in [".csv", ".flat", ".json"]:
            data_format = "flat"
        else:
            data_format = ext[1:]
        with open(filename, "rb") as f:
            data = f.read()
        print requests.post("{}?auth_token={}&format={}&email_errors=true".format(upload_url, user_token, data_format), data=data).text
        os.remove(filename)

def main():
    if not len(sys.argv)==5:
        print("Usage: {} port certfile upload_endpoint user_token".format(sys.argv[0]))
        return False
    global port
    port = sys.argv[1]
    global certfile
    certfile = sys.argv[2]
    global upload_url
    upload_url = sys.argv[3]
    global user_token
    user_token = sys.argv[4]sh

    authorizer = DummyAuthorizer()
    authorizer.add_user("test", "test", ".", perm="elradfmwM")
    authorizer.add_anonymous(os.getcwd())

    handler = TAAHandler
    handler.certfile = certfile
    handler.authorizer = authorizer
    handler.abstracted_fs = TAAFileHandler
    handler.banner = "Take An App - FTP drop box"

    address = ("", port)
    server = FTPServer(address, handler)

    server.max_cons = 256
    server.max_cons_per_ip = 5

    server.serve_forever()

if __name__ == '__main__':
    main()
