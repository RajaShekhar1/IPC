"""
Simple fake HTTP server that prints anything posted to it on the given port.
"""

import BaseHTTPServer
import sys


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print "usage: {} port_number"
        sys.exit(1)

    port = int(sys.argv[1])

    class PostDummyHandler(BaseHTTPServer.BaseHTTPRequestHandler):
        def do_POST(self):
            print(self.path)
            data = line = self.rfile.readline(65537)
            print line
            #while line:
            line = self.rfile.readline(65537)
            print line
            #data += line
            #print(data)

            self.wfile.write('HTTP/1.1 200 OK')

    httpd = BaseHTTPServer.HTTPServer(('localhost', port), PostDummyHandler)
    #while True:
    httpd.handle_request()
