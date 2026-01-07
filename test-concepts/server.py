import http.server
import socketserver
import os

os.chdir('/home/asdf/scientific-mapping/test-concepts')

PORT = 8080

Handler = http.server.SimpleHTTPRequestHandler
with socketserver.TCPServer(("", PORT), Handler) as httpd:
    print(f"Server running at http://localhost:{PORT}/")
    httpd.serve_forever()
