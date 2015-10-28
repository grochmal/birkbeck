#!/usr/bin/env python
# Shall be used as a CGI script with a POST method, example:
#
# POST proxy.py HTTP/1.1
# Content-Length: 44
# Content-Type: Content-Type: application/x-www-form-urlencoded
#
# url=http%3A%2F%2Fpython.org&mime=text%2Fhtml

from urllib import urlopen, unquote
from sys import stdin
mime = "text/html"
post = stdin.read()
params = dict(map(lambda x: x.split('='), post.split('&')))
if ('mime' in params): mime = unquote(params['mime'])
print "Content-Type: " + mime
print
if ('url' in params): print urlopen(unquote(params['url'])).read()
else: print "<html><body>You must specify a url</body></html>"

