#!/usr/bin/python
import socket
import re
import json
from tornado import websocket, web, ioloop, gen, iostream
from tornado.tcpserver import TCPServer

# vars
online_socks = []
online_hosts = []


# tcp server
class smallTcpHandler(object):
    def __init__(self, stream):
        self.name = False
        self.stream = stream
        self.stream.socket.setsockopt(
            socket.IPPROTO_TCP, socket.TCP_NODELAY, 1)
        self.stream.socket.setsockopt(
            socket.IPPROTO_TCP, socket.SO_KEEPALIVE, 1)
        self.stream.set_close_callback(self.on_disconnect)

    @gen.coroutine
    def on_disconnect(self):
        yield []

    @gen.coroutine
    def dispatch_client(self):
        try:
            while True:
                line = yield self.stream.read_until(b'\r\n')
                line = line[:-2]
                if self.name is False and re.match('^Auth:: [^$]+', line):
                    self.name = line[7:]
                    online_hosts.append(self.name)
                    toAllSocks(json.dumps(['online_hosts', online_hosts]))
                else:
                    if not self.name is False:
                        if line == 'End':
                            online_hosts.remove(self.name)
                            self.name = False
                            toAllSocks(json.dumps(
                                ['online_hosts', online_hosts]
                            ))
                        if re.match('[^\^]+:: [^$]+', line):
                            toAllSocks(json.dumps(
                                ['message', self.name, line]
                            ))
        except iostream.StreamClosedError:
            pass

    @gen.coroutine
    def on_connect(self):
        raddr = 'closed'
        try:
            raddr = '%s:%d' % self.stream.socket.getpeername()
        except Exception:
            pass
        yield self.dispatch_client()


class smallTcpServer(TCPServer):
    @gen.coroutine
    def handle_stream(self, stream, address):
        conn = smallTcpHandler(stream)
        yield conn.on_connect()

tcp_server = smallTcpServer()


# web app page + socket server
def toAllSocks(msg):
    for sock in online_socks:
        sock.write_message(msg)


class IndexHandler(web.RequestHandler):
    def get(self):
        self.render("index.html")


class SocketHandler(websocket.WebSocketHandler):
    def check_origin(self, origin):
        return True

    def open(self):
        if self not in online_socks:
            online_socks.append(self)
            self.write_message(json.dumps(['online_hosts', online_hosts]))

    def on_close(self):
        if self in online_socks:
            online_socks.remove(self)


web_app = web.Application([
    (r'/', IndexHandler),
    (r'/ws', SocketHandler)
])


# init servers
if __name__ == '__main__':
    tcp_server_port=8081
    tcp_server_host='0.0.0.0'
    web_app_port=8080
    tcp_server.listen(tcp_server_port, tcp_server_host)
    web_app.listen(web_app_port)
    print "web_app works on %s port" % (web_app_port)
    print "tcp_server works on %s port" % (tcp_server_port)
    ioloop.IOLoop.instance().start()
