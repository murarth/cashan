#!/usr/bin/python3
# -*- coding: utf-8

import json

__all__ = [
    'ClientError', 'Connection', 'Message',
]

class ClientError(Exception): pass

# 20 KiB should be plenty.
MESSAGE_LIMIT = 20 << 10

class Connection:

    def __init__(self, conn):
        self.conn = conn
        self.closed = False
        self.recv_buf = bytearray(b'')

    def close(self):
        self.conn.close()

    def fileno(self):
        return self.conn.fileno()

    def recv_message(self):
        while not self.closed:
            msg = self.read_message()

            if msg is None:
                self.recv_data()
            else:
                return msg

    def read_message(self):
        n = self.read_u32()

        if n is not None:
            if n > MESSAGE_LIMIT:
                raise ClientError('message size exceeded limit: {}'.format(n))
            if len(self.recv_buf) >= n + 4:
                s = self.recv_buf[4:n + 4].decode('utf-8')
                msg = json.loads(s)
                del self.recv_buf[:n + 4]
                return Message(msg)

    def read_u32(self):
        if len(self.recv_buf) >= 4:
            return int.from_bytes(self.recv_buf[:4], 'big')

    def recv_data(self):
        if self.closed:
            return

        buf = self.conn.recv(8192)

        if not buf:
            self.closed = True
            return

        self.recv_buf += buf

    def write_message(self, msg):
        if not self.closed:
            msg = json.dumps(msg).encode('utf-8')
            self.write_bytes(len(msg).to_bytes(4, 'big') + msg)

    def write_bytes(self, data):
        self.conn.sendall(data)

class Message:

    def __init__(self, msg):
        if not isinstance(msg, dict):
            raise ClientError('invalid message type')
        validate_message(msg)
        self.msg = msg

    def __contains__(self, key):
        return key in self.msg

    def __repr__(self):
        return repr(self.msg)

    def get(self, key, ty = str):
        v = self.msg[key]
        if not instance_check(v, ty):
            raise ClientError('expected {!r} as {}; got {}'
                .format(key, ty.__name__, type(v).__name__))
        return instance_prepare(v, ty)

def instance_check(value, ty):
    if hasattr(ty, 'instance_check'):
        return ty.instance_check(value)
    if isinstance(ty, type):
        return isinstance(value, ty)
    return ty.instance_check(value)

def instance_prepare(value, ty):
    if hasattr(ty, 'instance_prepare'):
        return ty.instance_prepare(value)
    return value

def validate_message(msg):
    if isinstance(msg, (int, float)):
        pass
    elif isinstance(msg, dict):
        for k, v in msg.items():
            validate_message(k)
            validate_message(v)
    elif isinstance(msg, list):
        for v in msg:
            validate_message(v)
    elif isinstance(msg, str):
        if not msg.isprintable():
            raise ClientError('invalid message')
    else:
        raise ClientError('unexpected value of type {!r}'.format(type(msg).__name__))
