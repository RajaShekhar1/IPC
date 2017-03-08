#!/usr/bin/python
import requests
import sys
from io import BytesIO
from StringIO import StringIO

from pyftpdlib.authorizers import DummyAuthorizer, AuthenticationFailed
from pyftpdlib.handlers import TLS_FTPHandler
from pyftpdlib.servers import FTPServer
from pyftpdlib.filesystems import AbstractedFS, FilesystemError
from stormpath.error import Error as StormpathError

from taa.services.users.UserService import UserService
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
        self.post_api_request(data, data_format, filename)

    def post_api_request(self, data, data_format, filename):
        
        # Put some sane limit on the length of the filename
        filename = filename[:128]
        print("received file with name '{}', {} bytes".format(filename, len(data)))
        
        user_href = self.authorizer.get_user_href(self.username)
        url = "{}?auth_token={}&format={}&user_href={}&upload_source=dropbox&filename={}".format(
            self.upload_url, self.auth_token, data_format, user_href, filename)
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



class StormPathAuthorizer(object):
    """
    Use the stormpath account service to validate that the user
    is authorized to upload enrollment import files, and access the user's email address.
    """

    def __init__(self, stormpath_application):
        self.stormpath_application = stormpath_application
        self.user_table = {}

    def validate_authentication(self, username, password, handler):
        """Raises AuthenticationFailed if supplied username and
        password don't match the stored credentials, else return
        None.
        """
        if username == 'anonymous':
            msg = "Anonymous access not allowed."
            raise AuthenticationFailed(msg)
        try:
            account = self.stormpath_application.authenticate_account(username, password).account
        except StormpathError as err:
            msg = err.message['message'] if hasattr(err, 'message') and err.message and err.message.get('message') else ''
            raise AuthenticationFailed(u"Authentication failed: {}".format(msg))

        # TODO: Validate user group
        if not UserService().can_user_submit_enrollments(account):
            raise AuthenticationFailed("Authorization failed: you are not authorized to upload enrollment files.")

        self.user_table[username] = account

    def get_user_href(self, username):
        return self.user_table[username].href

    def get_home_dir(self, username):
        """Return the user's home directory.
        Since this is called during authentication (PASS),
        AuthenticationFailed can be freely raised by subclasses in case
        the provided username no longer exists.
        """
        return ''

    def impersonate_user(self, username, password):
        """Impersonate another user (noop).

        It is always called before accessing the filesystem.
        By default it does nothing.  The subclass overriding this
        method is expected to provide a mechanism to change the
        current user.
        """

    def terminate_impersonation(self, username):
        """Terminate impersonation (noop).

        It is always called after having accessed the filesystem.
        By default it does nothing.  The subclass overriding this
        method is expected to provide a mechanism to switch back
        to the original user.
        """

    all_user_perms = "elrw"
    def has_perm(self, username, perm, path=None):
        """Whether the user has permission over path (an absolute
        pathname of a file or a directory).

        Expected perm argument is one of the following letters:
        "elradfmwM".
        """
        if username in self.user_table:
            return perm in self.all_user_perms
        else:
            return False

    def get_perms(self, username):
        """Return current user permissions."""
        if username in self.user_table:
            return self.all_user_perms
        else:
            return ""

    def get_msg_login(self, username):
        """Return the user's login message."""
        return "Welcome to the 5Star Take-An-App Enrollment Import DropBox"

    def get_msg_quit(self, username):
        """Return the user's quitting message."""
        return "Goodbye"





def main(port, certfile, upload_url, auth_token):

    # Users have "list" and "put" permissions only.
    from stormpath.client import Client as SPClient

    stormpath_app = UserService().get_stormpath_application()
    authorizer = StormPathAuthorizer(stormpath_app)

    handler = TAAHandler
    
    handler.tls_control_required = True
    handler.
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
        print(u"Usage: {} port certfile upload_endpoint user_token".format(sys.argv[0]))
        sys.exit(1)

    port = sys.argv[1]
    certfile = sys.argv[2]
    upload_url = sys.argv[3]
    auth_token = sys.argv[4]

    main(port, certfile, upload_url, auth_token)
