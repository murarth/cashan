#!/usr/bin/python3
# -*- coding: utf-8

import json
import os
import select
import socket
import time

from network import *

VERSION = '0.0.2'

class Client(Connection):

    def __init__(self, conn):
        super().__init__(conn)
        self.name = None
        self.version = None
        self.current_game = None
        self.last_message = time.monotonic()
        self.last_ping = 0

class Game:

    def __init__(self, name, host, num_players):
        self.name = name
        self.players = [host]
        self.num_players = num_players
        self.started = False

    def empty(self):
        if self.started:
            return all(p is None for p in self.players)
        else:
            return len(self.players) == 0

    def ready(self):
        return len(self.players) == self.num_players and \
            all(p is not None for p in self.players)

    def join(self, conn):
        if self.started:
            raise ClientError('game has already started')
        else:
            self.send_all({ 'action': 'join', 'name': conn.name })
            self.players.append(conn)

    def leave(self, conn):
        self.players.remove(conn)
        self.send_all({ 'action': 'leave', 'name': conn.name })

    def rejoin(self, conn):
        raise NotImplementedError

    def start(self):
        self.started = True
        names = [p.name for p in self.players]
        # Send 'start' to the host
        self.players[0].write_message({ 'action': 'start', 'players': names })

    def send(self, conn, msg):
        if not self.started:
            raise ClientError("the game hasn't started")

        for p in self.players:
            if p is not conn:
                p.write_message(msg)

    def send_all(self, msg):
        for p in self.players:
            p.write_message(msg)

PING_INTERVAL = 30

class CashanServer:

    def __init__(self, config):
        self.config = config
        self.epoll = None
        self.connections = {}
        self.names = {}
        self.games = {}
        self.listener = None

    def run(self):
        self.setup()

        while True:
            for fd, events in self.epoll.poll(timeout = 1):
                if fd == self.listener.fileno():
                    self.accept_connection()
                else:
                    self.recv_connection(fd, events)

            now = time.monotonic()
            ping_time = now - PING_INTERVAL
            kill_time = now - PING_INTERVAL * 2

            for fd in list(self.connections.keys()):
                conn = self.connections[fd]

                if conn.last_message < kill_time:
                    self.close_connection(conn, 'ping timeout')
                elif conn.last_message < ping_time and conn.last_ping < conn.last_message:
                    conn.write_message({ 'action': 'ping' })
                    conn.last_ping = now

    def close(self):
        if self.epoll is not None:
            self.epoll.close()
        if self.listener is not None:
            self.listener.close()
        [c.close() for c in self.connections.values()]

    def setup(self):
        self.epoll = epoll = select.epoll()

        self.listener = lis = self.open_listener()
        epoll.register(lis.fileno(), select.EPOLLIN)

    def accept_connection(self):
        conn, addr = self.listener.accept()

        self.connections[conn.fileno()] = Client(conn)
        self.epoll.register(conn.fileno(), select.EPOLLIN)

    def recv_connection(self, fd, events):
        conn = self.connections[fd]

        try:
            conn.recv_data()
        except Exception as e:
            self.close_connection(conn, str(e))
            return

        if conn.closed:
            self.close_connection(conn, 'connection closed')

        while True:
            try:
                msg = conn.read_message()
            except ClientError as e:
                self.close_connection(conn, str(e))
                return
            except Exception as e:
                self.close_connection(conn, 'invalid message')
                return

            if msg is None:
                break

            try:
                conn.last_message = time.monotonic()
                self.handle_message(conn, msg)
            except ClientError as e:
                self.close_connection(conn, str(e))

    def handle_message(self, conn, msg):
        action = msg.get('action')

        if action == 'hello':
            if conn.name is not None:
                raise ClientError("duplicate 'hello'")
            name = msg.get('name')

            validate_name(name)

            if name in self.names:
                raise ClientError('name {!r} is already taken'.format(name))

            version = msg.get('version')

            if version != VERSION:
                raise ClientError('unsupported version; use {!r}'.format(VERSION))

            conn.name = name
            self.names[name] = conn
            conn.version = version

            conn.write_message({ 'action': 'hello', 'version': VERSION })
            return

        if conn.name is None:
            raise ClientError("expected 'hello'")

        if action == 'pong':
            pass
        elif action == 'host':
            name = msg.get('name')
            validate_name(name)
            players = msg.get('players', int)
            self.host_game(conn, name, players)
        elif action == 'join':
            name = msg.get('name')
            self.join_game(conn, name)
        elif action == 'send':
            body = msg.get('body', dict)
            self.send_to_game(conn, body)
        else:
            raise ClientError('invalid action: {!r}'.format(action))

    def host_game(self, conn, name, players):
        if conn.current_game is not None:
            raise ClientError('already in a game')

        if name in self.games:
            raise ClientError('game {!r} already exists'.format(name))

        self.games[name] = Game(name, conn, players)
        conn.current_game = name

        conn.write_message({ 'action': 'host', 'name': name })

    def join_game(self, conn, name):
        if conn.current_game is not None:
            raise ClientError('already in a game')

        if name not in self.games:
            raise ClientError('no such game {!r}'.format(name))

        game = self.games[name]
        game.join(conn)

        conn.current_game = name
        conn.write_message({ 'action': 'join_game', 'name': name })

        if game.ready():
            game.start()

    def send_to_game(self, conn, msg):
        game = self.games[conn.current_game]
        game.send(conn, msg)

    def leave_game(self, conn, name):
        game = self.games[name]

        game.leave(conn)
        if game.empty():
            del self.games[name]

    def close_connection(self, conn, reason):
        conn.write_message({ 'action': 'error', 'error': reason })
        fd = conn.fileno()
        self.epoll.unregister(fd)
        del self.connections[fd]
        conn.close()

        if conn.name is not None:
            del self.names[conn.name]
        if conn.current_game is not None:
            self.leave_game(conn, conn.current_game)

    def open_listener(self):
        addr, port = self.config['address'], self.config['port']
        sock = socket.socket()
        sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        sock.bind((addr, port))
        sock.listen(5)
        print('Listening on', addr, port)
        return sock

def get_config():
    addr = input('Enter an address to bind: ')
    port = int(input('Enter a port to bind: '))

    cfg = {
        'address': addr,
        'port': port,
    }

    os.makedirs(os.path.dirname(config_path()), exist_ok = True)

    with open(config_path(), 'w') as f:
        json.dump(cfg, f)
        f.write('\n')

    print('Configuration saved to', config_path())
    return cfg

def config_path():
    return os.path.expanduser('~/.config/cashan/server.cfg')

def load_config():
    with open(config_path(), 'r') as f:
        return json.load(f)

def validate_name(name):
    if not name or not name.isalnum():
        raise ClientError('invalid name')
    if len(name) > 16:
        raise ClientError('name is too long; limit is 16 characters')

if __name__ == '__main__':
    try:
        config = load_config()
    except FileNotFoundError:
        config = get_config()

    server = CashanServer(config)
    try:
        server.run()
    finally:
        server.close()
