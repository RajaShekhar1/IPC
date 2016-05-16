
import os
from StringIO import StringIO
from subprocess import Popen, PIPE
import sys
import time
from unittest2 import TestCase

from ftplib import FTP_TLS
from hamcrest import assert_that, starts_with, equal_to


class TestFTPDropBox(TestCase):
    def setUp(self):
        self.test_host = 'localhost'
        self.test_port = 5021
        # These are stormpath now
        self.test_user = 'agent@zachmason.com'
        self.test_passwd = '12121212'
        self.test_user_token = 'USER-ABC123'
        self.test_http_port = 8088
        self.test_endpoint = 'http://localhost:%s/enrollments'%self.test_http_port
        self.valid_test_file = open(os.path.abspath('tests/data/minimal_data.flat'), 'r').read()

        # Assumes the test is run from the base directory, which is standard.
        server_file_path = os.path.abspath("FTPDropBox/server.py")

        # Run the FTP DropBox server for each test.
        cmd = "{} {} {} {} {} {}".format(sys.executable,
                                         server_file_path,
                                         self.test_port,
                                         "FTPDropBox/taa.pem",
                                         self.test_endpoint,
                                         self.test_user_token)
        #print(cmd)
        self.pipe = Popen(cmd.split())

        # Run an HTTP server to capture the data.
        http_server_path = os.path.abspath("tests/integration_tests/fake_http.py")
        http_cmd = "{} {} {}".format(sys.executable,
                               http_server_path,
                               self.test_http_port,
                               )
        #print(http_cmd)
        self.http_pipe = Popen(http_cmd.split(), stdout=PIPE)

        # Give the server a second to start up
        time.sleep(5)

    def tearDown(self):
        # Kill both subprocesses after each test.
        try:
            self.pipe.kill()
        except OSError: pass
        try:
            self.http_pipe.kill()
        except OSError: pass

    def test_it_should_post_to_URL_when_file_submitted(self):

        # Connect to the FTP server and transfer a valid file.
        FTP_TLS.port = self.test_port
        ftp = FTP_TLS(host=self.test_host)
        ftp.login(user=self.test_user, passwd=self.test_passwd, secure=True)
        ftp.storlines('STOR test.flat', StringIO(self.valid_test_file))
        ftp.quit()

        requested_path = self.http_pipe.stdout.readline(1024)
        assert_that(requested_path, starts_with("/enrollments"))

        data_sent = line_read = self.http_pipe.stdout.readline(65535)
        #while line_read:
        #    line_read = self.http_pipe.stdout.readline(65535)
        #    data_sent += line_read

        assert_that(data_sent, starts_with(self.valid_test_file.split('\n')[0]))

