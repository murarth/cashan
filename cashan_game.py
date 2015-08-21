#!/usr/bin/python3
# -*- coding: utf-8

import argparse
from collections import Counter, defaultdict, deque
import curses
import itertools
import json
import os
import random
import socket
import sys
import threading
import time

from cashan import *
from game import *
from network import *

VERSION = '0.0.1'

# High frequency numbers
(COLOR_HIGH,
    # Items in 'Buy' that cannot be bought or cards in 'Play' that cannot be played
    COLOR_INVALID,
    # Terrain colors
    COLOR_HILLS, COLOR_FOREST, COLOR_MOUNTAINS,
    COLOR_FIELDS, COLOR_PASTURE, COLOR_DESERT,
    # Development card colors
    COLOR_KNIGHT, COLOR_PROGRESS, COLOR_VICTORY,
    # Color for special cards, LargestArmy and LongestRoad
    COLOR_SPECIAL,
    # Colors for player units
    COLOR_PLAYER0, COLOR_PLAYER1, COLOR_PLAYER2, COLOR_PLAYER3,
    # 'any' (for harbors) uses no color
    COLOR_ANY,
    ) = range(1, 18)

# Resource colors match their terrain
(COLOR_BRICK, COLOR_LUMBER, COLOR_ORE, COLOR_GRAIN, COLOR_WOOL) = (
    COLOR_HILLS, COLOR_FOREST, COLOR_MOUNTAINS,
    COLOR_FIELDS, COLOR_PASTURE)

# State.priority values
PRI_NORMAL, PRI_HIGH = range(2)

class CashanGame(Game):

    GAME_TITLE = 'Cashan'

    def __init__(self, stdscr, args, config, state, play_state, connection = None):
        super().__init__(stdscr)
        self.args = args
        self.config = config
        # Connection to multiplayer server; None when offline
        self.connection = connection
        # Starting game state
        self.cashan = state
        # Starting state of gameplay
        self.play_state = play_state
        # Messages received from server
        self.messages = deque()
        self.key_callbacks = {
            ord('q'): self.quit_game,
            ord('\n'): self.advance_actions,
            ord(' '): self.skip_actions,
        }

    def init_colors(self):
        super().init_colors()
        curses.init_pair(COLOR_ANY,         -1, -1)
        curses.init_pair(COLOR_HIGH,        curses.COLOR_RED, -1)
        curses.init_pair(COLOR_INVALID,     curses.COLOR_RED, -1)
        curses.init_pair(COLOR_HILLS,       curses.COLOR_RED, -1)
        curses.init_pair(COLOR_FOREST,      curses.COLOR_GREEN, -1)
        curses.init_pair(COLOR_MOUNTAINS,   curses.COLOR_BLUE, -1)
        curses.init_pair(COLOR_FIELDS,      curses.COLOR_MAGENTA, -1)
        curses.init_pair(COLOR_PASTURE,     curses.COLOR_CYAN, -1)
        curses.init_pair(COLOR_DESERT,      curses.COLOR_YELLOW, -1)
        curses.init_pair(COLOR_KNIGHT,      curses.COLOR_MAGENTA, -1)
        curses.init_pair(COLOR_PROGRESS,    curses.COLOR_GREEN, -1)
        curses.init_pair(COLOR_VICTORY,     curses.COLOR_YELLOW, -1)
        curses.init_pair(COLOR_SPECIAL,     curses.COLOR_CYAN, -1)
        curses.init_pair(COLOR_PLAYER0,     curses.COLOR_RED, -1)
        curses.init_pair(COLOR_PLAYER1,     curses.COLOR_BLUE, -1)
        curses.init_pair(COLOR_PLAYER2,     curses.COLOR_GREEN, -1)
        curses.init_pair(COLOR_PLAYER3,     curses.COLOR_MAGENTA, -1)

    def start_game(self):
        # Player who is currently taking their turn
        self.player_turn = self.play_state.player_turn
        # Game phase; 'setup' or 'play'
        self.phase = 'setup'
        # Sequence number of any active trade offer
        self.active_trade = None
        # Index into cashan.players that refers to the human player
        self.self_player = self.play_state.self_player
        # Locally simulated players
        self.ai_players = self.play_state.ai_players
        # Messages to indicate actions of other players
        self.action_messages = deque()
        # Whether to automatically skip action messages
        self.skip_actions_flag = False
        self.states = []

        if self.connection is not None:
            self.start_connection_thread()

        # And off we go!
        self.push_state(self.next_turn())

    def quit_game(self):
        if self.connection is None:
            super().quit_game()
        else:
            pass # TODO: Do something here

    def start_connection_thread(self):
        threading.Thread(target = self.connection_loop, daemon = True).start()

    def connection_loop(self):
        while not self.quit:
            msg = self.connection.recv_message()

            if msg is None:
                self.messages.append({ 'action': 'error', 'error': 'connection closed' })
                break

            action = msg.get('action')

            if action == 'ping':
                self.connection.write_message({ 'action': 'pong' })
                continue

            self.messages.append(msg)

    def send_message(self, msg):
        if self.connection is not None:
            self.connection.write_message({ 'action': 'send', 'body': msg })

    def player_is_local(self, player):
        return player == self.self_player or player in self.ai_players

    @property
    def current_player(self):
        return self.cashan.players[self.player_turn]

    def next_turn(self):
        '''Advances player turn and calls start_turn'''
        self.skip_actions_flag = False
        old = self.player_turn
        if self.player_turn is None:
            self.player_turn = 0
        else:
            self.player_turn = (self.player_turn + 1) % len(self.cashan.players)

        if self.player_turn == 0 and self.args.save_name:
            self.save_game_state()

        return self.start_turn()

    def save_game_state(self):
        try:
            save_game(self.args.save_name, self.cashan.dump_state())
        except Exception as e:
            log('failed to save game:', e)

    def start_turn(self):
        '''Returns the starting State for the current player's turn'''
        if self.phase == 'setup' and self.setup_ended():
            self.phase = 'play'

        if self.player_is_local(self.player_turn):
            if self.phase == 'setup':
                state = BuildSettlement(self,
                    [(pos, intr) for pos, intr in self.cashan.starting_positions
                        if not self.cashan.building_exists(pos, intr)])
            elif self.phase == 'play':
                state = StartTurn(self)
            else:
                raise Exception('start turn on phase {!r}'.format(self.phase))
        else:
            state = WaitRemote(self)

        return state

    def setup_ended(self):
        settlements = Counter(o.owner
            for p, o in self.cashan.buildings.items())

        return all(settlements[p] >= 2 for p in range(len(self.cashan.players)))

    def get_state(self, ty):
        if self.states:
            state = self.states[-1]
            if isinstance(state, ty):
                return state
            raise Exception('get_state ty expected {}; found {!r}'
                .format(ty.__name__, state))
        raise Exception('get_state ty expected {}; found no state'.format(ty.__name__))

    def pop_state(self, ty):
        state = self.states.pop()
        if not isinstance(state, ty):
            raise Exception('expected state of type {}; found {!r}'
                .format(ty.__name__, state))

    def push_state(self, state):
        '''
        Registers a state object to preferentially receive user input.
        The state is a callable accepting a single argument,
        the character input value.
        It returns one of the following:
            CONSUME - Consume the event and maintain state.
            PASS    - Pass the event to the next and maintain state.
            DIE     - Consume the event and remove state.
        '''
        if state is None:
            raise RuntimeError('None state')
        self.states.append(state)

    def remove_state(self, ty):
        for i in reversed(range(len(self.states))):
            if isinstance(self.states[i], ty):
                del self.states[i]
                break
        else:
            raise Exception('no state of type {} found; states are {!r}'
                .format(ty.__name__, self.states))

    def handle_input(self, ch):
        self.queue_redraw = True
        consume = False

        # TODO: Skipping messages on your own turn will cause too much skipping.
        # A better concept of "skipping" is required to resolve this hack.
        self.skip_actions_flag = False

        if not self.action_messages:
            if self.states:
                i = len(self.states) - 1
                state = self.states[i]

                if state.accepts_input(self.self_player):
                    ret = state.player_input(ch)
                    if ret == DIE:
                        consume = True
                        del self.states[i]
                    elif ret == CONSUME:
                        consume = True

        if not consume:
            cb = self.key_callbacks.get(ch)
            if cb:
                cb()

        self.handle_events()

        if not self.states and not self.action_messages:
            self.end_turn()

    def handle_events(self):
        while not self.action_messages and self.messages:
            msg = self.messages.popleft()
            self.handle_remote(msg)
            self.queue_redraw = True

        if not self.action_messages and self.states:
            i = len(self.states) - 1
            state = self.states[i]
            for p in self.ai_players:
                if state.accepts_input(p):
                    ret = ai_driver(self, p, state)

                    if ret == CONSUME:
                        del self.states[i]
                        break
                    elif ret == PASS:
                        pass
                    else:
                        raise RuntimeError('ai_driver returned {!r}'.format(ret))

            if not self.states and not self.action_messages:
                self.end_turn()

    def after_tick(self):
        super().after_tick()
        self.handle_events()

    def handle_remote(self, msg):
        action = msg.get('action')

        if action == 'error':
            raise ClientError(msg.get('error'))
        elif action == 'leave':
            name = msg.get('name')
            self.set_message('{} has left the game'.format(name), None)
        elif action == 'acquire_resources':
            self.acquire_resources({ Resource.get(r): n
                for r, n in msg.get('resources', dict).items() })
        elif action == 'declare_victory':
            self.declare_victory()
        elif action == 'dev_card':
            self.buy_development_card()
        elif action == 'end_turn':
            self.end_turn()
        elif action == 'move_robber':
            self.cashan.robber = msg.get('pos', Position)
            self.add_action_message(self.player_turn, 'move_robber', ())
        elif action == 'place_building':
            item = Item.get(msg.get('type'))
            self.place_building_by(msg.get('player', int),
                item, msg.get('pos', Position), msg.get('intr', Intr))
        elif action == 'place_road':
            self.place_road_by(msg.get('player', int),
                msg.get('pos', Position), msg.get('edge', Edge))
        elif action == 'play_card':
            card = Development.get(msg.get('card'))
            self.play_card(card)
        elif action == 'propose_trade':
            mode = msg.get('mode')
            trade_id = msg.get('trade_id', int)
            n = msg.get('n', int)
            resource = Resource.get(msg.get('resource'))
            self.propose_trade(mode, n, resource, trade_id = trade_id)
        elif action == 'purchase':
            item = Item.get(msg.get('item'))
            self.player_purchase(item)
        elif action == 'roll':
            self.handle_roll(msg.get('n', int))
        elif action == 'set_discard':
            player = msg.get('player', int)
            resources = { Resource.get(r): n
                for r, n in msg.get('resources', dict).items() }
            state = self.get_state(HalveResources)
            if state.set_discard(player, resources):
                self.remove_state(HalveResources)
        elif action == 'steal':
            self.resource_stolen(self.player_turn,
                msg.get('target', int), Resource.get(msg.get('resource')))
        elif action == 'steal_fail':
            self.add_action_message(self.player_turn, 'steal_fail',
                msg.get('target', int))
        elif action == 'take_resource':
            self.take_all_resource(Resource.get(msg.get('resource')))
        elif action == 'trade_bank':
            n_give, r_give = msg.get('give', list)
            n_recv, r_recv = msg.get('recv', list)
            self.trade_with_bank(
                (n_give, Resource.get(r_give)),
                (n_recv, Resource.get(r_recv)))
        elif action == 'trade_offer':
            player = msg.get('player', int)
            n = msg.get('n', int)
            resource = Resource.get(msg.get('resource'))
            trade_id = msg.get('trade_id', int)

            self.trade_offer(player, n, resource, trade_id = trade_id)
        elif action == 'trade_player':
            other = msg.get('other', int)
            n_give, r_give = msg.get('give', list)
            n_recv, r_recv = msg.get('recv', list)

            self.trade_with_player(other,
                n_give, Resource.get(r_give),
                n_recv, Resource.get(r_recv))
        elif action == 'trade_reject':
            player = msg.get('player', int)
            trade_id = msg.get('trade_id', int)

            self.reject_trade_offer(player, trade_id = trade_id)
        elif action == 'withdraw_trade':
            mode = msg.get('mode')
            self.withdraw_trade(mode)
        else:
            raise Exception('unrecognized remote action: {!r}'.format(action))

    def prompt_confirmation(self, msg, cb):
        '''
        Prompts for a yes or no response. If input 'y' is received,
        the given callback is called with no arguments. If any other
        input is received, input grab is released and nothing is called.
        '''
        # TODO: Do this without altering game state.
        raise NotImplementedError

    def declare_victory(self):
        self.phase = 'end'
        self.push_state(Victory(self))
        if self.player_is_local(self.player_turn):
            self.send_message({ 'action': 'declare_victory' })

    def get_placing(self):
        winner = self.player_turn

        places = [{ 'player': i, 'points': self.cashan.count_victory_points(p) }
            for i, p in enumerate(self.cashan.players)]

        places.sort(key = lambda p: (p['player'] == winner, p['points']),
            reverse = True)

        prev = None
        prev_place = None

        for i, p in enumerate(places, 1):
            if prev_place != 1 and p['points'] == prev:
                p['place'] = prev_place
            else:
                p['place'] = i
                prev = p['points']
                prev_place = i

        return places

    def end_turn(self):
        '''Called when a player's turn has ended'''
        if self.player_is_local(self.player_turn):
            self.send_message({ 'action': 'end_turn' })
        else:
            self.pop_state(WaitRemote)

        state = self.next_turn()
        if state is not None:
            self.states.append(state)

    def add_action_message(self, player, action, params):
        if not self.skip_actions_flag:
            self.action_messages.append((player, action, params))
            self.queue_redraw = True

    def advance_actions(self):
        self.skip_actions_flag = False
        if self.action_messages:
            self.action_messages.popleft()
            self.redraw()

    def skip_actions(self):
        if self.action_messages:
            self.skip_actions_flag = True
            self.action_messages.clear()

    def push_action(self, player, msg):
        if not self.skip_actions_flag:
            self.action_messages.append((player, msg))
            self.redraw()

    def buy_development_card(self):
        if self.player_is_local(self.player_turn):
            self.player_purchase(DevelopmentCard)
        devc = self.cashan.development_cards.pop()
        self.current_player.development_cards[devc] += 1
        self.add_action_message(self.player_turn, 'dev_card', devc)
        if self.player_is_local(self.player_turn):
            self.send_message({ 'action': 'dev_card' })
        return devc

    def place_settlement(self, pos, intr):
        '''Places a settlement as the current player'''
        if self.phase != 'setup':
            self.player_purchase(Settlement)
        self.place_building_by(self.player_turn, Settlement, pos, intr)

    def place_city(self, pos, intr):
        self.player_purchase(City)
        self.place_building_by(self.player_turn, City, pos, intr)

    def place_building_by(self, player, item: Item, pos, intr):
        p = self.cashan.players[player]
        if item is Settlement:
            dec(p, 'settlements')
        elif item is City:
            dec(p, 'cities')
            p.settlements += 1
        else:
            raise Exception('unexpected building type: {!r}'.format(item))

        self.cashan.buildings[(pos, intr)] = Object(item, player)
        if player != self.self_player:
            self.add_action_message(player, 'place_building', (item, pos, intr))
        if self.player_is_local(player):
            self.send_message({ 'action': 'place_building',
                'player': player, 'type': item.name, 'pos': pos, 'intr': intr })

        self.check_longest_road()

    def place_road(self, pos, edge, *, no_cost = False):
        if not no_cost:
            self.player_purchase(Road)
        self.place_road_by(self.player_turn, pos, edge)

    def place_road_by(self, player, pos, edge):
        p = self.cashan.players[player]
        dec(p, 'roads')
        self.cashan.roads[(pos, edge)] = Object(Road, player)
        if player != self.self_player:
            self.add_action_message(player, 'place_road', (pos, edge))
        if self.player_is_local(player):
            self.send_message({ 'action': 'place_road',
                'player': player, 'pos': pos, 'edge': edge })

        self.check_longest_road()

    def check_largest_army(self):
        players = self.cashan.players

        if LargestArmy in self.cashan.special_cards:
            pl = [(i, p.played_development_cards[Knight])
                for i, p in enumerate(players)]
            pl.sort(key = lambda p: p[1], reverse = True)

            p_max, n_max = pl[0]

            if n_max >= MIN_LARGEST_ARMY:
                self.cashan.special_cards.remove(LargestArmy)
                players[p_max].special_cards.add(LargestArmy)
                self.add_action_message(p_max, 'largest_army', (n_max, None))
        else:
            pl = [(i, p.played_development_cards[Knight],
                LargestArmy in p.special_cards)
                    for i, p in enumerate(players)]
            # In case of a tie, the player already holding LargestArmy wins.
            pl.sort(key = lambda i: i[1:], reverse = True)

            p_max, n_max, has_largest = pl[0]

            if not has_largest:
                idx = index_of(pl, lambda p: p[2])
                taken = pl[idx][0]
                players[taken].special_cards.remove(LargestArmy)
                players[p_max].special_cards.add(LargestArmy)
                self.add_action_message(p_max, 'largest_army', (n_max, taken))

    def check_longest_road(self):
        '''
        Checks whether a player should be awarded the Longest Road
        card and, if so, awards it.
        '''
        players = self.cashan.players

        if LongestRoad in self.cashan.special_cards:
            pl = [(i, self.cashan.longest_road(i))
                for i in range(len(players))]
            pl.sort(key = lambda i: i[1], reverse = True)

            p_max, n_max = pl[0]

            if n_max >= MIN_LONGEST_ROAD:
                self.cashan.special_cards.remove(LongestRoad)
                players[p_max].special_cards.add(LongestRoad)
                self.add_action_message(p_max, 'longest_road', (n_max, None))
        else:
            pl = [(i, self.cashan.longest_road(i),
                LongestRoad in players[i].special_cards)
                    for i in range(len(players))]
            # In case of a tie, the player already holding LongestRoad wins.
            pl.sort(key = lambda i: i[1:], reverse = True)

            p_max, n_max, has_longest = pl[0]

            if not has_longest:
                idx = index_of(pl, lambda p: p[2])
                taken = pl[idx][0]
                players[taken].special_cards.remove(LongestRoad)
                players[p_max].special_cards.add(LongestRoad)
                self.add_action_message(p_max, 'longest_road', (n_max, taken))

    def play_card(self, card):
        self.current_player.development_cards[card] -= 1

        if self.player_turn != self.self_player:
            self.add_action_message(self.player_turn, 'play_card', card)
        if self.player_is_local(self.player_turn):
            self.send_message({ 'action': 'play_card', 'card': card.name })

        if card is Knight:
            self.current_player.played_development_cards[Knight] += 1
            self.activate_robber(False)

            self.check_largest_army()
        elif card is Monopoly:
            if self.player_is_local(self.player_turn):
                self.push_state(SelectResource(self))
        elif card is RoadBuilding:
            if self.player_is_local(self.player_turn):
                self.push_state(BuildTwoRoads(self))
        elif card is YearOfPlenty:
            if self.player_is_local(self.player_turn):
                self.push_state(SelectResourceCards(self))

    def take_all_resource(self, resource):
        '''
        Gives to the current player all of the given resource
        from every other player
        '''
        player = self.current_player
        total = 0

        for i, other in enumerate(self.cashan.players):
            if other is not player:
                n = other.resources[resource]
                if n:
                    total += n
                    other.resources[resource] = 0
                    player.resources[resource] += n
                    self.add_action_message(self.player_turn,
                        'take_resource', (i, resource, n))

        if not total:
            self.add_action_message(self.player_turn, 'take_none', resource)

        if self.player_is_local(self.player_turn):
            self.send_message({ 'action': 'take_resource',
                'resource': resource.name })

    def acquire_resources(self, resources):
        '''
        Gives the current player the given set of resources from the "bank."
        '''
        player = self.current_player

        for r, n in resources.items():
            player.resources[r] += n
            self.cashan.resources[r] -= n

        self.add_action_message(self.player_turn, 'acquire_resources', resources)

        if self.player_is_local(self.player_turn):
            self.send_message({ 'action': 'acquire_resources',
                'resources': { r.name: n for r, n in resources.items() if n } })

    def player_discard(self, player, resources):
        '''Discards resources from a player's hand'''
        p = self.cashan.players[player]
        for r, n in resources.items():
            p.resources[r] -= n
            self.cashan.resources[r] += n

        self.add_action_message(player, 'discard_resource', resources)

    def player_set_discard(self, player, resources):
        if self.player_is_local(player):
            self.send_message({ 'action': 'set_discard',
                'player': player,
                'resources': { r.name: n for r, n in resources.items() } })

    def propose_trade(self, mode, n, resource, *, trade_id = None):
        self.push_state(TradeOffer(self, mode, n, resource))

        if self.player_is_local(self.player_turn):
            self.active_trade = trade_id = gen_state_id()
            self.send_message({ 'action': 'propose_trade', 'mode': mode,
                'trade_id': trade_id, 'n': n, 'resource': resource.name })
        else:
            if trade_id is None:
                raise Exception('remote propose_trade missing trade_id')
            self.active_trade = trade_id

    def reject_trade_offer(self, player, *, trade_id = None):
        if self.player_is_local(self.player_turn):
            self.send_message({ 'action': 'trade_reject',
                'trade_id': self.active_trade,
                'player': player })
        else:
            if trade_id == self.active_trade:
                state = self.get_state(TradeOffer)
                state.reject_offer(player = player)

    def trade_offer(self, player, n, resource, *, trade_id = None):
        if self.player_is_local(player):
            self.send_message({ 'action': 'trade_offer',
                'trade_id': self.active_trade,
                'player': player, 'n': n, 'resource': resource.name })
        else:
            if trade_id == self.active_trade:
                state = self.get_state(TradeOffer)
                state.submit_offer(player, n, resource)

    def withdraw_trade(self, mode):
        self.active_trade = None
        if self.player_is_local(self.player_turn):
            self.send_message({ 'action': 'withdraw_trade', 'mode': mode })
        else:
            if self.player_turn != self.self_player:
                self.add_action_message(self.player_turn, 'withdraw_trade', mode)
                self.pop_state(TradeOffer)

    def trade_with_bank(self, give, recv):
        player = self.current_player
        n_give, r_give = give
        n_recv, r_recv = recv

        player.resources[r_give] -= n_give
        player.resources[r_recv] += n_recv

        self.cashan.resources[r_give] += n_give
        self.cashan.resources[r_recv] -= n_recv

        if self.player_turn != self.self_player:
            self.add_action_message(self.player_turn, 'trade_bank', (give, recv))
        if self.player_is_local(self.player_turn):
            self.send_message({ 'action': 'trade_bank',
                'give': [n_give, r_give.name],
                'recv': [n_recv, r_recv.name] })

    def trade_with_player(self, other, n_give, r_give, n_recv, r_recv):
        player = self.current_player
        other_p = self.cashan.players[other]

        player.resources[r_give] -= n_give
        player.resources[r_recv] += n_recv

        other_p.resources[r_recv] -= n_recv
        other_p.resources[r_give] += n_give

        self.add_action_message(self.player_turn, 'trade_player',
            (other, n_give, r_give, n_recv, r_recv))

        if self.player_is_local(self.player_turn):
            self.send_message({ 'action': 'trade_player', 'other': other,
                'give': (n_give, r_give.name), 'recv': (n_recv, r_recv.name) })
        else:
            self.pop_state(TradeOffer)

    def player_roll(self):
        n = roll()
        self.send_message({ 'action': 'roll', 'n': n })
        self.handle_roll(n)

    def handle_roll(self, n):
        self.add_action_message(self.player_turn, 'roll', n)
        if n == 7:
            self.activate_robber(halve = True)
        else:
            self.produce_resources(n)

    def produce_resources(self, n):
        robber = self.cashan.robber
        n_players = len(self.cashan.players)
        # { resource: { player: n } }
        resources = resource_cards(defaultdict(int))
        # [ player: { resource: n } ]
        player_r = [resource_cards(0) for i in range(n_players)]
        # Resources to skip because there aren't enough for everyone
        skip = []

        for pos, cell in self.cashan.grid:
            if cell.number == n and pos != robber:
                for pos, intr in intersections(pos):
                    bldg = self.cashan.buildings.get((pos, intr))
                    if bldg is not None:
                        n_res = 1 if bldg.ty is Settlement else 2
                        resources[cell.terrain.resource][bldg.owner] += n_res

        for r, counts in resources.items():
            if sum(counts.values()) > self.cashan.resources[r]:
                # If only one player is receiving, give them what's left
                if len(counts) == 1:
                    pl = next(iter(counts.keys()))
                    player_r[pl][r] = self.cashan.resources[r]
                else:
                    skip.append(r)
            else:
                for pl, n in counts.items():
                    player_r[pl][r] = n

        for i, res in enumerate(player_r):
            if any(res.values()):
                for r, n in res.items():
                    self.cashan.players[i].resources[r] += n
                    self.cashan.resources[r] -= n
                self.add_action_message(i, 'resource_produced', res)

        for r in skip:
            self.add_action_message(None, 'resource_exhausted', r)

    def activate_robber(self, halve):
        '''
        Activates the robber, either by rolling a '7' or the playing
        of a Knight card.
        In the former case only, player resources may be halved.
        '''
        if halve:
            if self.halve_resources():
                # HalveResources will trigger further action upon completion
                return

        if self.player_is_local(self.player_turn):
            self.push_state(SelectCell(self,
                'Move robber', self.move_robber, deny = self.cashan.robber))

    def move_robber(self, pos):
        self.cashan.robber = pos
        self.send_message({ 'action': 'move_robber', 'pos': pos })

        if self.player_turn != self.self_player:
            self.add_action_message(self.player_turn, 'move_robber', ())

        targets = set()
        for pos, intr in intersections(pos):
            obj = self.cashan.buildings.get((pos, intr))
            if obj is not None and obj.owner != self.player_turn:
                targets.add(obj.owner)

        targets = list(targets)

        if self.player_is_local(self.player_turn):
            if len(targets) == 1:
                self.steal_resource(targets[0])
            elif len(targets) > 1:
                self.push_state(StealFrom(self, targets))

    def steal_resource(self, target):
        '''Steals one random resource card from the target player'''
        target_p = self.cashan.players[target]
        choices = sum([[r] * n for r, n in target_p.resources.items()], [])

        if not choices:
            self.add_action_message(self.player_turn, 'steal_fail', target)
            self.send_message({ 'action': 'steal_fail', 'target': target })
        else:
            r = random.choice(choices)
            target_p.resources[r] -= 1
            self.current_player.resources[r] += 1
            self.add_action_message(self.player_turn, 'steal', (target, r))
            self.send_message({ 'action': 'steal',
                'target': target, 'resource': r.name })

    def resource_stolen(self, player, target, resource):
        p = self.cashan.players[player]
        t = self.cashan.players[target]

        p.resources[resource] += 1
        t.resources[resource] -= 1
        self.add_action_message(player, 'steal', (target, resource))

    def halve_resources(self):
        players = {}

        for i, p in enumerate(self.cashan.players):
            n = sum(p.resources.values())
            if n > 7:
                players[i] = n // 2

        if players:
            self.push_state(HalveResources(self, players))
            return True
        return False

    def road_places(self):
        '''Returns a list of (pos, edge) where the player may build a Road'''
        res = set()

        for (rpos, edge), obj in self.cashan.roads:
            if obj.owner == self.player_turn:
                for pos, e in edge_adjacent(rpos, edge):
                    if self.cashan.can_build_road(self.player_turn, pos, e):
                        res.add((pos, e))

        return list(res)

    def settlement_places(self):
        '''Returns a list of (pos, intr) where the player may build a Settlement'''
        res = set()

        for (rpos, edge), obj in self.cashan.roads:
            if obj.owner == self.player_turn:
                for pos, intr in edge_intersections(rpos, edge):
                    if self.cashan.can_build_settlement(self.player_turn, pos, intr):
                        res.add((pos, intr))

        return list(res)

    def city_places(self):
        '''Returns a list of (pos, intr) where the player may build a City'''
        res = []

        for pos, obj in self.cashan.buildings:
            if obj.ty is Settlement and obj.owner == self.player_turn:
                res.append(pos)

        return res

    def player_purchase(self, item: Item):
        '''
        Purchases an item for the local player. 
        Resources are transferred from the player to the "bank".
        Raises an exception if the player lacks the resources.
        '''
        player = self.cashan.players[self.player_turn]
        for n, res in item.cost:
            if player.resources[res] < n:
                raise Exception('insufficient {} to purchase {}'
                    .format(res.name, item.name))
            player.resources[res] -= n
            self.cashan.resources[res] += n

        if self.player_is_local(self.player_turn):
            self.send_message({ 'action': 'purchase', 'item': item.name })

    def draw_display(self, y, x):
        brick = self.cashan.resources[Brick]
        lumber = self.cashan.resources[Lumber]
        ore = self.cashan.resources[Ore]
        grain = self.cashan.resources[Grain]
        wool = self.cashan.resources[Wool]
        dev = len(self.cashan.development_cards)

        # Draw card counts owned by "banker"
        win = self.stdscr
        win.addstr(1, 2, 'Available(Dev[{:>2}]'.format(dev))
        x_off = 19
        if LargestArmy in self.cashan.special_cards:
            win.addstr(1, x_off + 1, 'LA', curses.color_pair(COLOR_SPECIAL))
            x_off += 3
        if LongestRoad in self.cashan.special_cards:
            win.addstr(1, x_off + 1, 'LR', curses.color_pair(COLOR_SPECIAL))
            x_off += 3
        win.addstr(1, x_off, ')')

        win.addstr(2, 2, 'B', curses.color_pair(COLOR_BRICK))
        win.addstr(2, 3, '[{:>2}]'.format(brick))
        win.addstr(2, 8, 'L', curses.color_pair(COLOR_LUMBER))
        win.addstr(2, 9, '[{:>2}]'.format(lumber))
        win.addstr(2, 14, 'O', curses.color_pair(COLOR_ORE))
        win.addstr(2, 15, '[{:>2}]'.format(ore))
        win.addstr(2, 20, 'G', curses.color_pair(COLOR_GRAIN))
        win.addstr(2, 21, '[{:>2}]'.format(grain))
        win.addstr(2, 26, 'W', curses.color_pair(COLOR_WOOL))
        win.addstr(2, 27, '[{:>2}]'.format(wool))

        player = self.cashan.players[self.self_player]

        knight = player.development_cards[Knight]
        progress = sum(n for c, n in player.development_cards.items()
            if isinstance(c, Progress))
        victory = player.development_cards[VictoryPoint]
        total_victory = self.cashan.count_victory_points(player)

        # Draw cards owned by player
        win.addstr(4, 2, 'Hand(')
        win.addstr(4, 7, 'K', curses.color_pair(COLOR_KNIGHT))
        win.addstr(4, 8, '[{:>2}]'.format(knight))
        win.addstr(4, 13, 'P', curses.color_pair(COLOR_PROGRESS))
        win.addstr(4, 14, '[{:>2}]'.format(progress))
        win.addstr(4, 19, 'V', curses.color_pair(COLOR_VICTORY))
        win.addstr(4, 20, '[{:>2}])'.format(victory))

        brick = player.resources[Brick]
        lumber = player.resources[Lumber]
        ore = player.resources[Ore]
        grain = player.resources[Grain]
        wool = player.resources[Wool]

        win.addstr(5, 2, 'B', curses.color_pair(COLOR_BRICK))
        win.addstr(5, 3, '[{:>2}]'.format(brick))
        win.addstr(5, 8, 'L', curses.color_pair(COLOR_LUMBER))
        win.addstr(5, 9, '[{:>2}]'.format(lumber))
        win.addstr(5, 14, 'O', curses.color_pair(COLOR_ORE))
        win.addstr(5, 15, '[{:>2}]'.format(ore))
        win.addstr(5, 20, 'G', curses.color_pair(COLOR_GRAIN))
        win.addstr(5, 21, '[{:>2}]'.format(grain))
        win.addstr(5, 26, 'W', curses.color_pair(COLOR_WOOL))
        win.addstr(5, 27, '[{:>2}]'.format(wool))

        attr = curses.A_BOLD | curses.color_pair(COLOR_PLAYER0 + self.self_player)

        # Items in player's reserve
        win.addstr(6, 2, 'x', attr)
        win.addstr(6, 3, '[{}]'.format(player.settlements))
        win.addstr(6, 8, 'X', attr)
        win.addstr(6, 9, '[{}]'.format(player.cities))
        win.addstr(6, 14, '/', attr)
        win.addstr(6, 15, '[{:>2}]'.format(player.roads))
        # Total victory points
        win.addstr(6, 21, 'V', curses.color_pair(COLOR_VICTORY))
        win.addstr(6, 22, '({:>2})'.format(total_victory))

        # Draw player listing, starting with header
        win.addstr(1, x - 17, 'R', curses.color_pair(COLOR_LUMBER))
        win.addstr(1, x - 12, 'K', curses.color_pair(COLOR_KNIGHT))

        longest_name = max(len(p.name) for p in self.cashan.players)
        startx = x - longest_name - 5 - 6 - 3 - 3 - 2
        #                           |   |   |   |   `- Space at the end
        #                           |   |   |   `- 'LR '
        #                           |   |   `- 'LA '
        #                           |   `- ' [nn] ' - Knight count
        #                           `- ' [nn]' - Resource count

        for i, p in enumerate(self.cashan.players):
            win.addstr(2 + i, startx, p.name, curses.color_pair(COLOR_PLAYER0 + i))
            n_knights = p.played_development_cards[Knight]
            n_res = sum(p.resources.values())

            win.addstr(2 + i, x - 18, '[{:>2}]'.format(n_res))
            win.addstr(2 + i, x - 13, '[{:>2}]'.format(n_knights))
            if LargestArmy in p.special_cards:
                win.addstr(2 + i, x - 8, 'LA', curses.color_pair(COLOR_SPECIAL))
            if LongestRoad in p.special_cards:
                win.addstr(2 + i, x - 5, 'LR', curses.color_pair(COLOR_SPECIAL))

            if self.player_turn == i:
                win.addstr(2 + i, startx - 2, '*', curses.A_BOLD)

    def draw_field(self, y, x):
        self.draw_display(y, x)

        # Draw cells
        for pos, cell in self.cashan.grid.items():
            self.draw_cell(y, x, pos, cell)

        # Draw roads
        for (pos, edge), road in self.cashan.roads.items():
            self.draw_road(y, x, pos, edge, road)

        # Draw normal-priority state
        self.draw_state(y, x, PRI_NORMAL)

        # Draw settlements and cities
        for (pos, intr), obj in self.cashan.buildings.items():
            self.draw_building(y, x, pos, intr, obj)

        # Draw high-priority state (e.g. overlay text box)
        self.draw_state(y, x, PRI_HIGH)

    def draw_state(self, y, x, priority = PRI_NORMAL):
        if not self.action_messages and self.states:
            state = self.states[-1]
            if state.priority == priority and state.accepts_input(self.self_player):
                state.draw_state(y, x)

    def draw_message(self, y, x):
        win = self.stdscr
        if self.action_messages:
            self.draw_action_message(y, x, self.action_messages[0])
        elif self.message:
            self.draw_message_lines(y, x, self.message)
        else:
            if self.states:
                state = self.states[-1]
                if state.accepts_input(self.self_player):
                    msg = self.states[-1].display_message
                    if msg:
                        self.draw_message_lines(y, x, msg)

    def draw_message_lines(self, y, x, lines):
        if isinstance(lines, str):
            lines = [lines]

        for i, msg in enumerate(reversed(lines), 1):
            self.stdscr.addstr(y - i, 0, msg[:x], curses.A_BOLD)

    def draw_action_message(self, y, x, msg):
        player, action, params = msg
        w = ScreenWriter(self.stdscr, y, x, y - 2, 0)
        p = self.cashan.players

        w.write('* ')
        if player is not None:
            player_color = curses.color_pair(COLOR_PLAYER0 + player)
            w.write(p[player].name, curses.A_BOLD | player_color)
            w.move_col(1)

        if action == 'acquire_resources':
            w.write('acquired')
            w.write_resources(params)
        elif action == 'dev_card':
            if player == self.self_player:
                card = params
                color = dev_color(card)
                w.write('received a ')
                w.write(card.name, curses.color_pair(color))
                w.write(' card')
            else:
                w.write('bought a Development Card')
        elif action == 'discard_resource':
            w.write('discarded')
            w.write_resources(params)
        elif action == 'largest_army':
            size, taken = params
            if taken is None:
                w.write('has achieved ')
                w.write('Largest Army', curses.color_pair(COLOR_SPECIAL))
                w.write(' with {} knights'.format(size))
            else:
                w.write('took ')
                w.write('Largest Army', curses.color_pair(COLOR_SPECIAL))
                w.write(' from ')
                w.write(p[taken].name, curses.color_pair(COLOR_PLAYER0 + taken))
                w.write(' with {} knights'.format(size))
        elif action == 'longest_road':
            length, taken = params
            if taken is None:
                w.write('has achieved ')
                w.write('Longest Road', curses.color_pair(COLOR_SPECIAL))
                w.write(' with {} continuous roads'.format(length))
            else:
                w.write('took ')
                w.write('Longest Road', curses.color_pair(COLOR_SPECIAL))
                w.write(' from ')
                w.write(p[taken].name, curses.color_pair(COLOR_PLAYER0 + taken))
                w.write(' with {} continuous roads'.format(length))
        elif action == 'move_robber':
            w.write('moved the ')
            w.write('Robber', curses.A_BOLD)
        elif action == 'place_building':
            item, pos, intr = params
            w.write('placed a {}'.format(item))
            self.draw_building_at(y, x, pos, intr,
                'x' if item is Settlement else 'X',
                curses.A_BOLD | curses.A_REVERSE | player_color)
        elif action == 'place_road':
            pos, edge = params
            w.write('placed a road')
            self.draw_road_at(y, x, pos, edge,
                curses.A_BOLD | curses.A_REVERSE | player_color)
        elif action == 'play_card':
            card = params
            w.write('played ')
            w.write(card.name, curses.color_pair(dev_color(card)))
        elif action == 'resource_exhausted':
            w.write('Nobody receives ')
            w.write_resource_name(params)
        elif action == 'resource_produced':
            w.write('received')
            w.write_resources(params)
        elif action == 'roll':
            n = params
            w.write('rolled: ')
            w.write(str(n), curses.A_BOLD)
        elif action == 'steal':
            target, res = params

            if self.self_player in (player, target):
                w.write('stole 1 ')
                _, r_color = resource_name(res)
                w.write(res.name, curses.color_pair(r_color))
                w.write(' from ')
                w.write(p[target].name, curses.color_pair(COLOR_PLAYER0 + target))
            else:
                w.write('stole from ')
                w.write(p[target].name, curses.color_pair(COLOR_PLAYER0 + target))
        elif action == 'steal_fail':
            target = params

            attr = curses.A_BOLD | curses.color_pair(COLOR_PLAYER0 + target)
            w.write('could not steal from ')
            w.write(p[target].name, attr)
            w.write(' because ')
            w.write(p[target].name, attr)
            w.write(' has no resources')
        elif action == 'take_none':
            w.write('took no ')
            w.write_resource_name(params)
            w.write(' because nobody had any')
        elif action == 'take_resource':
            target, resource, n = params
            w.write('took {} '.format(n))
            w.write_resource_name(resource)
            w.write(' from ')
            w.write(p[target].name, curses.color_pair(COLOR_PLAYER0 + target))
        elif action == 'trade_bank':
            (n_give, r_give), (n_recv, r_recv) = params

            w.write('traded {} '.format(n_give))
            w.write_resource_name(r_give)
            w.write(' for {} '.format(n_recv))
            w.write_resource_name(r_recv)
        elif action == 'trade_player':
            other, n_give, r_give, n_recv, r_recv = params

            w.write('traded {} '.format(n_give))
            w.write_resource_name(r_give)
            w.write(' to ')
            attr = curses.A_BOLD | curses.color_pair(COLOR_PLAYER0 + other)
            w.write(p[other].name, attr)
            w.write(' for {} '.format(n_recv))
            w.write_resource_name(r_recv)
        elif action == 'withdraw_trade':
            mode = params
            w.write('withdrew their trade {}'.format(mode))
        else:
            raise Exception('invalid action: {!r}'.format(action))

        w.next_line()
        w.write('[Enter]: Next  [Space]: Skip', curses.A_BOLD)

    def draw_stopped(self, y, x):
        raise NotImplementedError

    def cell_pos(self, y, x, pos):
        '''Returns the origin (y, x) of a cell in screen coordinates'''
        # Position coordinates
        px, py = pos

        # Center coords
        cx = x // 2
        cy = y // 2

        # Grid coords
        gx = px
        gy = px + py * 2

        # Origin coords
        ox = cx + gx * 6
        oy = cy + gy * 2 # Half height because these are half steps

        return oy, ox

    def draw_cell(self, y, x, pos, cell):
        oy, ox = self.cell_pos(y, x, pos)

        win = self.stdscr

        if cell.terrain is Sea:
            name, color = resource_name(cell.harbor.resource)
            ratio = '{}:{}'.format(*cell.harbor.ratio)
            win.addstr(oy - 1, ox - 2, name, curses.color_pair(color))
            win.addstr(oy + 0, ox - 1, ratio)
        else:
            win.addstr(oy - 3, ox - 1,    '____')
            win.addstr(oy - 2, ox - 2,   '/    \\')
            win.addstr(oy - 1, ox - 3,  '/      \\')
            win.addstr(oy + 0, ox - 3, '\\      /')
            win.addstr(oy + 1, ox - 2,  '\\____/')

            self.draw_name_at(y, x, pos, cell)

            if cell.number is not None:
                attr = 0
                if cell.number in [6, 8]:
                    attr |= curses.color_pair(COLOR_HIGH)
                win.addstr(oy, ox - 1, '{:>2}'.format(cell.number), attr)
            if self.cashan.robber == pos:
                win.addstr(oy, ox + 2, 'R', curses.A_BOLD)

    def draw_name_at(self, y, x, pos, cell, attr = 0):
        oy, ox = self.cell_pos(y, x, pos)
        name, color = terrain_name(cell.terrain)
        self.stdscr.addstr(oy - 1, ox - 1, name, curses.color_pair(color) | attr)

    def draw_road(self, y, x, pos, edge, road):
        attr = curses.A_BOLD | curses.color_pair(COLOR_PLAYER0 + road.owner)
        self.draw_road_at(y, x, pos, edge, attr)

    def draw_road_at(self, y, x, pos, edge, attr = 0):
        oy, ox = self.cell_pos(y, x, pos)
        win = self.stdscr

        if edge == EDGE_N:
            win.addstr(oy - 3, ox - 1, '____', attr)
        elif edge == EDGE_NE:
            win.addstr(oy - 2, ox + 3, '\\', attr)
            win.addstr(oy - 1, ox + 4, '\\', attr)
        elif edge == EDGE_NW:
            win.addstr(oy - 2, ox - 2, '/', attr)
            win.addstr(oy - 1, ox - 3, '/', attr)
        elif edge == EDGE_S:
            win.addstr(oy + 1, ox - 1, '____', attr)
        elif edge == EDGE_SE:
            win.addstr(oy + 0, ox + 4, '/', attr)
            win.addstr(oy + 1, ox + 3, '/', attr)
        elif edge == EDGE_SW:
            win.addstr(oy + 0, ox - 3, '\\', attr)
            win.addstr(oy + 1, ox - 2, '\\', attr)
        else:
            raise Exception('invalid edge: {!r}'.format(edge))

    def draw_building(self, y, x, pos, intr, obj):
        attr = curses.A_BOLD | curses.color_pair(COLOR_PLAYER0 + obj.owner)
        ch = 'X' if obj.ty is City else 'x'
        self.draw_building_at(y, x, pos, intr, ch, attr)

    def draw_building_at(self, y, x, pos, intr, ch, attr = 0):
        oy, ox = self.cell_pos(y, x, pos)

        win = self.stdscr

        if intr == INTR_NE:
            win.addstr(oy - 2, ox + 2, ch, attr)
        elif intr == INTR_NW:
            win.addstr(oy - 2, ox - 1, ch, attr)
        elif intr == INTR_E:
            win.addstr(oy - 1, ox + 5, ch, attr)
        elif intr == INTR_SE:
            win.addstr(oy + 1, ox + 3, ch, attr)
        elif intr == INTR_SW:
            win.addstr(oy + 1, ox - 1, ch, attr)
        elif intr == INTR_W:
            win.addstr(oy - 1, ox - 4, ch, attr)
        else:
            raise Exception('invalid intersection: {!r}'.format(intr))

class PlayState:

    def __init__(self, *, ai_players, self_player, player_turn = None):
        self.ai_players = ai_players 
        self.player_turn = player_turn
        self.self_player = self_player 

class ScreenWriter:

    def __init__(self, win, y, x, start_y, start_x):
        self.win = win
        self.y = y
        self.x = x
        self.start_x = start_x
        self.cur_y = start_y
        self.cur_x = start_x

    def write(self, s, attr = 0):
        n = len(s)
        rem = self.x - self.cur_x
        self.win.addstr(self.cur_y, self.cur_x, s, attr)

        self.cur_x += min(rem, n)

    def write_resource_name(self, r, attr = 0):
        _, color = resource_name(r)
        self.write(r.name, attr | curses.color_pair(color))

    def write_resources(self, res):
        first = True

        for r, n in res.items():
            if n:
                if not first:
                    self.write(',')
                first = False
                self.write(' {} '.format(n))
                self.write_resource_name(r)

    def prev_line(self):
        self.move_line(-1)

    def next_line(self):
        self.move_line(1)

    def move(self, y, x):
        self.cur_y = y
        self.cur_x = x

    def move_col(self, n):
        self.cur_x += n

    def move_line(self, n):
        self.cur_y += n
        self.cur_x = self.start_x

LOG_FILE = None

def log(*args):
    if LOG_FILE is not None:
        print('{:.2f}'.format(time.monotonic()), *args, file = LOG_FILE)

def ai_driver(game, player, state):
    while True:
        ret = ai_step(game, player, state)

        if ret == CONSUME:
            return ret
        elif ret == PASS:
            return ret
        else:
            raise Exception('ai_step returned {!r}'.format(ret))

def ai_step(game, player, state):
    '''
    Operates an AI step on the given state.
    Returns CONSUME or PASS
    '''
    if isinstance(state, BuildSettlement):
        pos = random.choice(state.positions)
        state.select_position(*pos)
        return CONSUME
    elif isinstance(state, BuildRoad):
        pos = random.choice(state.positions)
        state.select_position(*pos)
        return CONSUME
    elif isinstance(state, HalveResources):
        req = state.required[player]
        res = game.cashan.players[player].resources.copy()
        discard = {}

        for i in range(req):
            max_r = max(res.items(), key = lambda k: k[1])[0]
            res[max_r] -= 1
            if max_r in discard:
                discard[max_r] += 1
            else:
                discard[max_r] = 1

        if state.set_discard(player, discard):
            return CONSUME
        return PASS
    elif isinstance(state, StartTurn):
        if not state.rolled:
            state.roll()
            return PASS
        else:
            return CONSUME
    elif isinstance(state, SelectCell):
        if state.action == 'Move robber':
            positions = [pos
                for pos, cell in game.cashan.grid
                    if pos != game.cashan.robber and cell.terrain is not Sea]
            random.shuffle(positions)
            state.select_position(positions[0])
            return CONSUME
        else:
            raise RuntimeError('unrecognized SelectCell.action: {!r}'.format(state.action))
    elif isinstance(state, StealFrom):
        state.choose_player(random.choice(state.targets))
        return CONSUME
    elif isinstance(state, TradeOffer):
        return PASS
    else:
        raise RuntimeError('unrecognized state {!r}'.format(state))

def dec(obj, attr):
    '''Decrements obj.attr, raising an Exception if the value is <= 0'''
    n = getattr(obj, attr)
    if n <= 0:
        raise Exception('cannot dec {!r}.{}; value is {}'.format(obj, attr, n))
    setattr(obj, attr, n - 1)

def index_of(itr, predicate):
    for i, obj in enumerate(itr):
        if predicate(obj):
            return i
    raise ValueError('index not found in list')

# Values returned from State.player_input
CONSUME, PASS, DIE = range(3)

class State:

    display_message = None
    priority = PRI_NORMAL

    def player_input(self, ch):
        raise NotImplementedError

    def accepts_input(self, player: int) -> bool:
        '''Returns whether the given player may interact with the state'''
        return player == self.game.player_turn

    def back_state(self):
        raise NotImplementedError

    def draw_state(self, y, x):
        pass

class ConfirmState(State):
    '''State requiring confirmation to call a function'''

    def __init__(self, game, cb, msg):
        self.cb = cb
        self.display_message = msg

    def accepts_input(self, player):
        return player == self.game.self_player

    def player_input(self, ch):
        if ch == ord('y'):
            self.cb()
        return DIE

class BuildSettlement(State):
    '''Prompts player to place a settlement in one of a set of positions'''

    def __init__(self, game, positions):
        self.game = game
        self.selected = 0
        self.positions = positions

        if self.can_cancel():
            self.display_message = \
                '[Arrows]: Select position  [Enter]: Build settlement  [Space]: Cancel'
        else:
            self.display_message = \
                '[Arrows]: Select position  [Enter]: Build settlement'

    def player_input(self, ch):
        n_positions = len(self.positions)
        if ch == curses.KEY_LEFT or ch == curses.KEY_UP:
            self.selected = (self.selected - 1) % n_positions
        elif ch == curses.KEY_RIGHT or ch == curses.KEY_DOWN:
            self.selected = (self.selected + 1) % n_positions
        elif ch == ord('\n'):
            self.select_position(*self.positions[self.selected])
            return DIE
        elif ch == ord(' ') and self.can_cancel():
            return DIE
        else:
            return PASS
        return CONSUME

    def select_position(self, pos, intr):
        self.game.place_settlement(pos, intr)
        if self.game.phase == 'setup':
            self.game.push_state(BuildRoad(self.game,
                [(p, i) for p, i in intr_edges(pos, intr)
                    if self.game.cashan.edge_exists(p, i)],
                    no_cost = True))

    def can_cancel(self):
        return self.game.phase != 'setup'

    def draw_state(self, y, x):
        pos, intr = self.positions[self.selected]
        for i, (pos, intr) in enumerate(self.positions):
            attr = curses.A_BOLD | curses.color_pair(COLOR_PLAYER0 + self.game.self_player)
            attr |= curses.A_REVERSE if i == self.selected else 0
            self.game.draw_building_at(y, x, pos, intr, '?', attr)

class BuildCity(State):
    '''Prompts player to upgrade an existing settlement into a city'''

    display_message = '[Arrows]: Select position  [Enter]: Build city  [Space]: Cancel'
    priority = PRI_HIGH

    def __init__(self, game, positions):
        self.game = game
        self.selected = 0
        self.positions = positions

    def player_input(self, ch):
        n_positions = len(self.positions)
        if ch == curses.KEY_LEFT or ch == curses.KEY_UP:
            self.selected = (self.selected - 1) % n_positions
        elif ch == curses.KEY_RIGHT or ch == curses.KEY_DOWN:
            self.selected = (self.selected + 1) % n_positions
        elif ch == ord('\n'):
            self.select_position(*self.positions[self.selected])
            return DIE
        elif ch == ord(' '):
            return DIE
        else:
            return PASS
        return CONSUME

    def select_position(self, pos, intr):
        self.game.place_city(pos, intr)

    def draw_state(self, y, x):
        pos, intr = self.positions[self.selected]
        self.game.draw_building_at(y, x, pos, intr, 'x',
            curses.A_BOLD | curses.A_REVERSE |
                curses.color_pair(COLOR_PLAYER0 + self.game.self_player))

class BuildRoad(State):
    '''Prompts player to place a road in one of a set of positions'''

    def __init__(self, game, positions, *, no_cost = False):
        self.game = game
        self.selected = 0
        self.positions = positions
        self.no_cost = no_cost

        if self.can_cancel():
            self.display_message = \
                '[Arrows]: Select position  [Enter]: Build road  [Space]: Cancel'
        else:
            self.display_message = \
                '[Arrows]: Select position  [Enter]: Build road'

    def player_input(self, ch):
        n_positions = len(self.positions)
        if ch == curses.KEY_LEFT or ch == curses.KEY_UP:
            self.selected = (self.selected - 1) % n_positions
        elif ch == curses.KEY_RIGHT or ch == curses.KEY_DOWN:
            self.selected = (self.selected + 1) % n_positions
        elif ch == ord('\n'):
            self.select_position(*self.positions[self.selected])
            return DIE
        elif ch == ord(' ') and self.can_cancel():
            return DIE
        else:
            return PASS
        return CONSUME

    def select_position(self, pos, edge):
        self.game.place_road(pos, edge, no_cost = self.no_cost)

    def can_cancel(self):
        return self.game.phase != 'setup'

    def draw_state(self, y, x):
        pos, edge = self.positions[self.selected]
        self.game.draw_road_at(y, x, pos, edge,
            curses.A_BOLD | curses.A_REVERSE |
                curses.color_pair(COLOR_PLAYER0 + self.game.self_player))

class BuildTwoRoads(State):

    display_message = '[Arrows]: Select position  [Enter]: Build road'

    def __init__(self, game):
        self.game = game
        self.roads_left = 2
        self.state = self.next_state()

    def player_input(self, ch):
        if self.state is None:
            return DIE
        if ch == ord(' '):
            return PASS
        res = self.state.player_input(ch)
        if res == DIE:
            self.roads_left -= 1
            self.state = self.next_state()
            return DIE if self.roads_left == 0 else CONSUME
        return res

    def next_state(self):
        if self.roads_left:
            places = self.game.road_places()
            if not places:
                return
            return BuildRoad(self.game, self.game.road_places(), no_cost = True)

    def draw_state(self, y, x):
        return self.state.draw_state(y, x)

class HalveResources(State):
    '''Forces targeted users to discard some resources'''

    display_message = '[Arrows]: Select resources  [Enter]: Discard'
    priority = PRI_HIGH

    def __init__(self, game, players):
        '''
        players is a dict: { player index: discards required }
        '''
        self.game = game
        self.required = players
        self.discards = {}

        if game.self_player in players:
            self.ui = SelectResourcesUI('You must discard {n} resources',
                players[game.self_player],
                game.cashan.players[game.self_player].resources)

    def accepts_input(self, player):
        selfp = self.game.self_player
        # If the human player is part of this, let them go first so that
        # AI-generated action messages do not interrupt.
        if selfp in self.required and selfp not in self.discards:
            return player == selfp
        return player in self.required and player not in self.discards

    def player_input(self, ch):
        if self.ui.player_input(ch) == CONSUME:
            return CONSUME
        elif ch == ord('\n'):
            req = self.required[self.game.self_player]
            if sum(self.ui.resources.values()) == req:
                if self.set_discard(self.game.self_player, self.ui.resources):
                    return DIE
            return CONSUME
        else:
            return PASS
        return CONSUME

    def set_discard(self, player, resources):
        '''
        Sets the amounts of resources to be discarded for the given player.
        If this is the last player to set a discard set, it returns True
        and triggers discard and further action in the game state.
        '''
        req = self.required[player]
        dis = sum(resources.values())
        if req != dis:
            raise Exception('set_discard got wrong resource count: '
                'expected {}; got {}'.format(req, dis))
        self.discards[player] = resources
        self.game.player_set_discard(player, resources)
        return self.finished()

    def finished(self):
        if len(self.required) == len(self.discards):
            for p, r in self.discards.items():
                self.game.player_discard(p, r)
            self.game.activate_robber(False)
            return True
        return False

    def draw_state(self, y, x):
        self_p = self.game.self_player
        if self_p in self.required and self_p not in self.discards:
            self.ui.draw(self.game.stdscr, y, x)

class SelectResourcesUI:

    def __init__(self, message, max, bounds):
        self.message = message
        self.max = max
        self.bounds = bounds
        self.resources = resource_cards(0)
        self.selected = 0

    def draw(self, win, y, x):
        w = 50
        h = 10

        sub = win.subwin(h, w, (y - h) // 2, (x - w) // 2)
        sub.clear()
        sub.border()

        sub.addstr(2, 3, self.message.format(n = self.max))

        for i, r in enumerate(RESOURCES):
            name, color = resource_name(r)
            sub.addstr(4, 5 + i * 9, name.strip(), curses.color_pair(color))
            sub.addstr(5, 6 + i * 9, '{:>2}'.format(self.resources[r]),
                curses.A_REVERSE if self.selected == i else 0)

    def get_resource(self, index):
        return RESOURCES[index]

    def player_input(self, ch):
        if ch == curses.KEY_LEFT:
            self.selected = (self.selected - 1) % 5
        elif ch == curses.KEY_RIGHT:
            self.selected = (self.selected + 1) % 5
        elif ch == curses.KEY_UP:
            r = self.get_resource(self.selected)
            if self.bounds[r] != self.resources[r] and \
                    sum(self.resources.values()) < self.max:
                self.resources[r] += 1
        elif ch == curses.KEY_DOWN:
            r = self.get_resource(self.selected)
            if self.resources[r] > 0:
                self.resources[r] -= 1
        else:
            return PASS
        return CONSUME

class SelectCell(State):
    '''Selects a cell on the map and calls a callback with the result'''

    def __init__(self, game, action, callback, deny = None):
        self.game = game
        self.action = action
        self.callback = callback
        self.selected = (0, 0)
        self.deny = deny
        if deny == (0, 0):
            self.selected = (0, -1)

        self.display_message = '[Arrows]: Select cell  [Enter]: {}'.format(action)

    def player_input(self, ch):
        x, y = self.selected
        if ch == curses.KEY_LEFT:
            x -= 1
        elif ch == curses.KEY_RIGHT:
            x += 1
        elif ch == curses.KEY_UP:
            y -= 1
        elif ch == curses.KEY_DOWN:
            y += 1
        elif ch == ord('\n'):
            self.select_position(self.selected)
            return DIE
        else:
            return PASS

        if self.deny != (x, y) and self.game.cashan.cell_exists((x, y)):
            self.selected = (x, y)
        return CONSUME

    def select_position(self, pos):
        self.callback(pos)

    def draw_state(self, y, x):
        cell = self.game.cashan.grid[self.selected]
        self.game.draw_name_at(y, x, self.selected, cell, curses.A_REVERSE)

class SelectResource(State):

    display_message = '[Arrows]: Select resource  [Enter]: Take resources'
    priority = PRI_HIGH

    def __init__(self, game):
        self.game = game
        self.selected = 0

    def player_input(self, ch):
        selected = self.selected

        if ch == curses.KEY_UP:
            self.selected = (selected - 1) % len(RESOURCES)
        elif ch == curses.KEY_DOWN:
            self.selected = (selected + 1) % len(RESOURCES)
        elif ch == ord('\n'):
            self.select_resource(RESOURCES[self.selected])
            return DIE
        else:
            return PASS
        return CONSUME

    def select_resource(self, resource):
        self.game.take_all_resource(resource)

    def draw_state(self, y, x):
        w = 50
        h = 15

        sub = self.game.stdscr.subwin(h, w, (y - h) // 2, (x - w) // 2)
        sub.clear()
        sub.border()

        sub.addstr(2, 5, 'Select a resource')
        sub.addstr(4 + self.selected, 3, '*')

        for i, r in enumerate(RESOURCES):
            ScreenWriter(sub, h, w, 4 + i, 5).write_resource_name(r)

class SelectResourceCards(State):

    display_message = '[Arrows]: Select resources  [Enter]: Acquire resources'
    priority = PRI_HIGH

    def __init__(self, game):
        self.game = game
        self.n_resources = n = min(2, sum(game.cashan.resources.values()))
        self.ui = SelectResourcesUI('Select {n} resources',
            n, game.cashan.resources)

    def player_input(self, ch):
        if self.ui.player_input(ch) == CONSUME:
            return CONSUME
        elif ch == ord('\n'):
            if sum(self.ui.resources.values()) == self.n_resources:
                self.select_resources(self.ui.resources)
                return DIE
        else:
            return PASS
        return CONSUME

    def select_resources(self, resources):
        self.game.acquire_resources(resources)

    def draw_state(self, y, x):
        self.ui.draw(self.game.stdscr, y, x)

class StartTurn(State):
    '''Represents the beginning of a normal turn'''

    def __init__(self, game):
        self.game = game
        self.rolled = False
        self.played = False
        self.bought = defaultdict(int)

    def player_input(self, ch):
        if ch == ord('r') and not self.rolled:
            self.roll()
        elif ch == ord('b'):
            self.game.push_state(Buy(self.game, self))
        elif ch == ord('t') and self.can_trade():
            self.game.push_state(Trade(self.game, self))
        elif ch == ord('p') and self.can_play():
            self.game.push_state(Play(self.game, self))
        elif ch == ord('v') and self.can_declare():
            self.game.declare_victory()
        elif ch == ord('e') and self.can_end():
            return DIE
        else:
            return PASS
        return CONSUME

    def roll(self):
        self.rolled = True
        self.game.player_roll()

    def can_buy(self):
        player = self.game.current_player
        return any(player.can_buy(i) for i in purchasable())

    def can_declare(self):
        return self.game.cashan.count_victory_points(
            self.game.current_player) >= 10

    def can_end(self):
        return self.rolled

    def can_play(self):
        return not self.played and any(self.is_playable(card)
            for card in self.game.current_player.development_cards.keys())

    def is_playable(self, card):
        hand = self.game.current_player.development_cards
        return card is not VictoryPoint and hand[card] - self.bought[card] > 0

    def can_roll(self):
        return not self.rolled

    def can_trade(self):
        return any(self.game.current_player.resources.values())

    def buy_item(self, item):
        if item is Road:
            self.game.push_state(BuildRoad(self.game,
                self.game.road_places()))
        elif item is Settlement:
            self.game.push_state(BuildSettlement(self.game,
                self.game.settlement_places()))
        elif item is City:
            self.game.push_state(BuildCity(self.game,
                self.game.city_places()))
        elif item is DevelopmentCard:
            self.bought[self.game.buy_development_card()] += 1
        else:
            raise Exception('invalid item: {!r}'.format(item))

    def play_card(self, card):
        self.played = True
        self.game.play_card(card)

    def propose_trade(self, mode, n, resource):
        '''
        Proposes a trade of given mode ('offer' or 'request').
        '''
        return self.game.propose_trade(mode, n, resource)

    def perform_maritime_trade(self, give, recv):
        '''Trades give: (n, resource) for recv: (n, resource) with the "bank"'''
        self.game.trade_with_bank(give, recv)

    @property
    def display_message(self):
        msg = []
        if self.can_roll():
            msg.append('[R]oll dice')
        if self.can_buy():
            msg.append('[B]uy')
        else:
            # Always show 'Buy' so that players can check costs
            msg.append('[B]: Check costs')
        if self.can_trade():
            msg.append('[T]rade')
        if self.can_play():
            msg.append('[P]lay')
        if self.can_declare():
            msg.append('[V]: Declare victory')
        if self.can_end():
            msg.append('[E]nd turn')
        return '  '.join(msg)

class TradeOffer(State):

    priority = PRI_HIGH

    def __init__(self, game, mode, n, resource):
        self.game = game
        self.mode = mode
        self.n = n
        self.resource = resource
        # [ (player, n, resource), ... ]
        self.offers = []
        # { player: ( 'offered' | 'rejected' ), ... }
        self.states = {}

        owner = game.player_turn == game.self_player
        self.ui = TradeOwnerUI(self) if owner else TradeOtherUI(self)

    @property
    def display_message(self):
        return self.ui.display_message

    def accepts_input(self, player):
        return True

    def player_input(self, ch):
        return self.ui.player_input(ch)

    def accept_offer(self, offer):
        player, n, resource = offer
        if self.mode == 'offer':
            self.game.trade_with_player(player, self.n, self.resource, n, resource)
        else:
            self.game.trade_with_player(player, n, resource, self.n, self.resource)

    def reject_offer(self, index = None, *, player = None):
        if index is None:
            index = index_of(self.offers, lambda off: off[0] == player)

        player, _, _ = self.offers.pop(index)
        self.states[player] = 'rejected'
        self.game.reject_trade_offer(player)

    def submit_offer(self, player, n, resource):
        self.offers.append((player, n, resource))
        self.states[player] = 'offered'
        self.game.trade_offer(player, n, resource)

    def withdraw_trade(self):
        self.game.withdraw_trade(self.mode)

    def draw_state(self, y, x):
        self.ui.draw(y, x)

class TradeOwnerUI:

    def __init__(self, trade):
        self.game = trade.game
        self.trade = trade
        self.selected = None

    @property
    def display_message(self):
        if self.trade.offers:
            if self.selected is None:
                return ['[Arrows]: Select offer',
                    '[Space]: Withdraw {}'.format(self.trade.mode)]
            else:
                return ['[Arrows]: Select offer  [Enter]: Accept offer  [R]eject offer',
                    '[Space]: Withdraw {}'.format(self.trade.mode)]
        else:
            return '[Space]: Withdraw {}'.format(self.trade.mode)

    def player_input(self, ch):
        selected = self.selected
        n_offers = len(self.trade.offers)

        if ch == curses.KEY_UP:
            if n_offers:
                self.selected = (n_offers - 1 if selected is None else
                    (selected - 1) % n_offers)
        elif ch == curses.KEY_DOWN:
            if n_offers:
                self.selected = (0 if selected is None else
                    (selected + 1) % n_offers)
        elif ch == ord('r'):
            if self.selected is not None:
                self.trade.reject_offer(self.selected)
                self.selected = None
            return CONSUME
        elif ch == ord(' '):
            self.trade.withdraw_trade()
            return DIE
        elif ch == ord('\n'):
            if self.selected is not None:
                self.trade.accept_offer(self.trade.offers[self.selected])
                return DIE
        else:
            return PASS
        return CONSUME

    def draw(self, y, x):
        w = 50
        h = 15

        sub = self.game.stdscr.subwin(h, w, (y - h) // 2, (x - w) // 2)
        sub.clear()
        sub.border()

        mode = self.trade.mode

        wr = ScreenWriter(sub, h, w, 2, 10)
        wr.write('You {}ed {} '.format(mode, self.trade.n))
        wr.write_resource_name(self.trade.resource)

        if self.trade.offers:
            players = self.game.cashan.players
            longest_name = max(len(p.name) for p in players)
            sub.addstr(4, 14, 'Select an offer')

            if self.selected is not None:
                sub.addstr(6 + self.selected, 3, '*')

            give = 'give' in self.trade.offers[0]

            for i, (p, n, resource) in enumerate(self.trade.offers):
                player = players[p]

                sub.addstr(6 + i, 5, player.name, curses.color_pair(COLOR_PLAYER0 + p))
                wr = ScreenWriter(sub, y, x, 6 + i, 12 + longest_name)

                wr.write(' {}s {} '.format(mode, n))
                wr.write_resource_name(resource)
        else:
            sub.addstr(4, 14, 'Waiting for offers...')

class TradeOtherUI:

    def __init__(self, trade):
        self.game = trade.game
        self.trade = trade
        self.offer = TradeInput()

    @property
    def display_message(self):
        if self.can_offer():
            return '[0-9]: Set number  [B|L|O|G|W]: Set resource  [Enter]: Offer trade'

    def player_input(self, ch):
        if ch == ord('\n'):
            self.make_offer()
        elif ch == curses.KEY_BACKSPACE or ch == curses.KEY_DC:
            self.clear_field()
        elif ch in RESOURCE_KEYS:
            self.set_field(RESOURCE_KEYS[ch])
        elif ch in NUMBER_KEYS:
            self.set_field(NUMBER_KEYS[ch])
        else:
            return PASS
        return CONSUME

    def can_offer(self):
        return self.trade.states.get(self.game.self_player) != 'offered'

    def make_offer(self):
        n = self.offer.number
        r = self.offer.resource
        player = self.game.cashan.players[self.game.self_player]

        if r is None:
            self.game.set_message('Missing trade resource')
        elif r is self.trade.resource:
            self.game.set_message('Cannot trade same resource')
        elif n == 0:
            self.game.set_message('Cannot trade zero')
        elif player.resources[r] < n:
            self.game.set_message('Not enough {} to trade'.format(r.name))
        else:
            self.trade.submit_offer(self.game.self_player, n, r)

    def set_field(self, value):
        if isinstance(value, int):
            n = self.offer.number
            if n == 0:
                self.offer.number = value
            elif n < 10:
                self.offer.number = n * 10 + value
        else: # isinstance(value, Resource)
            self.offer.resource = value

    def clear_field(self):
        if self.offer.number == 0:
            self.offer.resource = None
        else:
            self.offer.number = 0

    def draw(self, y, x):
        w = 50
        h = 15

        sub = self.game.stdscr.subwin(h, w, (y - h) // 2, (x - w) // 2)
        sub.clear()
        sub.border()

        owner = self.game.player_turn
        player = self.game.cashan.players[owner]
        mode = self.trade.mode

        wr = ScreenWriter(sub, h, w, 2, 5)

        wr.write(player.name, curses.color_pair(COLOR_PLAYER0 + owner))
        wr.write(' {}s {} '.format(mode, self.trade.n))
        wr.write_resource_name(self.trade.resource)

        state = self.trade.states.get(self.game.self_player)

        attr = 0
        if self.can_offer():
            attr |= curses.A_REVERSE
        wr.move(4, 5)
        wr.write(mode.title() + ' ')
        wr.write('{:>2} '.format(self.offer.number), attr)
        if self.offer.resource is None:
            wr.write('  ?  ', attr)
        else:
            wr.write_resource_name(self.offer.resource, attr)

        if state == 'offered':
            wr.move(6, 10)
            wr.write(mode.title() + ' submitted')
        elif state == 'rejected':
            wr.move(6, 10)
            wr.write(mode.title() + ' rejected')

class Buy(State):

    priority = PRI_HIGH

    def __init__(self, game, turn):
        self.game = game
        self.turn = turn
        self.selected = 0
        self.items = purchasable()

    def player_input(self, ch):
        n_items = len(self.items)
        if ch == curses.KEY_UP:
            self.selected = (self.selected - 1) % n_items
        elif ch == curses.KEY_DOWN:
            self.selected = (self.selected + 1) % n_items
        elif ch == ord('\n'):
            if self.can_buy(self.items[self.selected]):
                self.turn.buy_item(self.items[self.selected])
            else:
                self.game.set_message('Cannot buy that item')
        elif ch == ord(' '):
            return DIE
        else:
            return PASS
        return CONSUME

    @property
    def display_message(self):
        if self.can_buy(self.items[self.selected]):
            return '[Arrows]: Select item  [Enter]: Buy item  [Space]: Cancel'
        else:
            return '[Arrows]: Select item  [Space]: Cancel'

    def can_buy(self, item):
        game = self.game
        player = game.current_player

        if not player.can_buy(item):
            return False

        if item is Road:
            return bool(player.roads and game.road_places())
        elif item is Settlement:
            return bool(player.settlements and game.settlement_places())
        elif item is City:
            return bool(player.cities and game.city_places())
        elif item is DevelopmentCard:
            return bool(game.cashan.development_cards)
        else:
            raise Exception('invalid item: {!r}'.format(item))

    def draw_state(self, y, x):
        w = 50
        h = 15

        sub = self.game.stdscr.subwin(h, w, (y - h) // 2, (x - w) // 2)
        sub.clear()
        sub.border()

        sub.addstr(2, 14, 'Choose an item to buy')

        for i, item in enumerate(self.items):
            can_buy = self.can_buy(item)

            if i == self.selected:
                if can_buy:
                    sub.addstr(4 + i * 2, 4, '*')
                else:
                    sub.addstr(4 + i * 2, 4, 'x',
                        curses.A_BOLD | curses.color_pair(COLOR_INVALID))

            attr = curses.A_BOLD if can_buy else 0

            sub.addstr(4 + i * 2, 6, item.name, attr)
            wr = ScreenWriter(sub, h, w, 5 + i * 2, 6)

            for n, r in item.cost:
                _, color = resource_name(r)
                wr.write('  {} '.format(n))
                wr.write(r.name, curses.color_pair(color))

class TradeInput:

    def __init__(self):
        self.number = 0
        self.resource = None

def gen_state_id(NEXT_ID = itertools.count()):
    # itertools is a builtin module, which means that the GIL cannot be released
    # during the call of next(NEXT_ID). Therefore, this is effectively atomic.
    return next(NEXT_ID)

RESOURCE_KEYS = {
    ord('b'): Brick,
    ord('l'): Lumber,
    ord('o'): Ore,
    ord('g'): Grain,
    ord('w'): Wool,
}

NUMBER_KEYS = {
    ord('0'): 0,
    ord('1'): 1,
    ord('2'): 2,
    ord('3'): 3,
    ord('4'): 4,
    ord('5'): 5,
    ord('6'): 6,
    ord('7'): 7,
    ord('8'): 8,
    ord('9'): 9,
}

class Trade(State):

    priority = PRI_HIGH

    def __init__(self, game, turn):
        self.game = game
        self.turn = turn
        self.selected = 0
        self.inputs = [TradeInput() for i in range(4)]

    @property
    def display_message(self):
        msg = ['[Arrows]: Select', '[0-9]: Set number',
            '[B|L|O|G|W]: Set resource', '[Backspace]: Clear']
        msg2 = []

        if self.selected >= 2:
            msg2.append('[Enter]: Trade')
        else:
            msg2.append('[Enter]: Propose trade')

        msg2.append('[Space]: Cancel')

        return ['  '.join(msg), '  '.join(msg2)]

    def player_input(self, ch):
        selected = self.selected

        if ch == curses.KEY_LEFT or ch == curses.KEY_RIGHT:
            self.selected = { 2: 3, 3: 2 }.get(selected, selected)
        elif ch == curses.KEY_UP:
            self.selected = { 0: 2, 1: 0, 2: 1, 3: 1 }[selected]
        elif ch == curses.KEY_DOWN:
            self.selected = { 0: 1, 1: 2, 2: 0, 3: 0 }[selected]
        elif ch in RESOURCE_KEYS:
            self.set_input(selected, RESOURCE_KEYS[ch])
        elif ch in NUMBER_KEYS:
            self.set_input(selected, NUMBER_KEYS[ch])
        elif ch == ord(' '):
            return DIE
        elif ch == ord('\n'):
            return self.make_trade()
        elif ch == curses.KEY_BACKSPACE or ch == curses.KEY_DC:
            self.clear_input(selected)
        else:
            return PASS
        return CONSUME

    def clear_input(self, selected):
        field = self.inputs[selected]
        if field.number == 0:
            field.resource = None
        else:
            field.number = 0

    def set_input(self, selected, value):
        i = self.inputs[selected]
        if isinstance(value, int):
            n = i.number
            if n == 0:
                i.number = value
                self.adjust_linked_inputs(selected)
            elif n < 10:
                i.number = n * 10 + value
                self.adjust_linked_inputs(selected)
        else: # isinstance(value, Resource)
            i.resource = value
            self.adjust_linked_inputs(selected)

    # Linked inputs for maritime trading
    LINKED_INPUTS = { 2: 3, 3: 2 }

    def adjust_linked_inputs(self, selected):
        linked = self.LINKED_INPUTS.get(selected)
        if linked is not None:
            m, n = self.get_ratio()

            if selected == 2:
                amount = self.inputs[2].number
                if amount % m == 0:
                    new_amount = (amount // m) * n
                    if new_amount < 100:
                        self.inputs[3].number = new_amount
            else:
                amount = self.inputs[3].number
                if amount % n == 0:
                    new_amount = (amount // n) * m
                    if new_amount < 100:
                        self.inputs[2].number = new_amount

    def make_trade(self):
        selected = self.selected

        if selected == 0:   # Offer
            n = self.inputs[0].number
            r = self.inputs[0].resource

            player = self.game.current_player

            if r is None:
                self.game.set_message('Missing trade resource')
            elif n == 0:
                self.game.set_message('Cannot trade zero')
            elif player.resources[r] < n:
                self.game.set_message('Not enough {} to trade'.format(r.name))
            else:
                self.turn.propose_trade('offer', n, r)
        elif selected == 1: # Request
            n = self.inputs[1].number
            r = self.inputs[1].resource

            if r is None:
                self.game.set_message('Missing trade resource')
            elif n == 0:
                self.game.set_message('Cannot trade zero')
            else:
                self.turn.propose_trade('request', n, r)
        else:               # Maritime trade
            n_give = self.inputs[2].number
            r_give = self.inputs[2].resource
            n_recv = self.inputs[3].number
            r_recv = self.inputs[3].resource

            if n_give == 0 or n_recv == 0:
                self.game.set_message('Cannot trade zero')
            elif r_give is None or r_recv is None:
                self.game.set_message('Missing trade resource')
            elif r_give is r_recv:
                self.game.set_message('Cannot trade for same resource')
            else:
                player = self.game.current_player
                ratio = self.get_ratio()
                m, n = ratio

                if n_give % m != 0 or n_recv % n != 0:
                    self.game.set_message('Impossible trade amount')
                elif player.resources[r_give] < n_give:
                    self.game.set_message('Not enough {} to trade'
                        .format(r_give.name))
                elif self.game.cashan.resources[r_recv] < n_recv:
                    self.game.set_message('Not enough {} to trade'
                        .format(r_recv.name))
                else:
                    self.turn.perform_maritime_trade(
                        (n_give, r_give), (n_recv, r_recv))
                    return DIE

        return CONSUME

    def get_ratio(self):
        r = self.inputs[2].resource
        return self.game.cashan.get_trade_ratio(self.game.player_turn, r)

    def draw_state(self, y, x):
        w = 40
        h = 15

        sub = self.game.stdscr.subwin(h, w, (y - h) // 2, (x - w) // 2)
        sub.clear()
        sub.border()

        def attr(n):
            return curses.A_REVERSE if n == self.selected else 0

        inputs = self.inputs

        sub.addstr(2, 12, 'Propose a trade', curses.A_BOLD)

        sub.addstr(4, 5, 'Offer')
        sub.addstr(4, 13, '{:>2} '.format(inputs[0].number), attr(0))
        r = inputs[0].resource
        if r is not None:
            ScreenWriter(sub, y, x, 4, 16).write_resource_name(r, attr(0))
        else:
            sub.addstr(4, 16, '  ?  ', attr(0))

        sub.addstr(6, w // 2 - 1, 'or', curses.A_BOLD)

        sub.addstr(8, 5, 'Request')
        sub.addstr(8, 13, '{:>2} '.format(inputs[1].number), attr(1))
        r = inputs[1].resource
        if r is not None:
            ScreenWriter(sub, y, x, 8, 16).write_resource_name(r, attr(1))
        else:
            sub.addstr(8, 16, '  ?  ', attr(1))

        sub.addstr(10, 12, 'Maritime trade', curses.A_BOLD)

        sub.addstr(12, 5, '{:>2} '.format(inputs[2].number), attr(2))
        r = inputs[2].resource
        if r is not None:
            ScreenWriter(sub, y, x, 12, 8).write_resource_name(r, attr(2))
        else:
            sub.addstr(12, 8, '  ?  ', attr(2))

        sub.addstr(12, 16, 'for')

        sub.addstr(12, 21, '{:>2} '.format(inputs[3].number), attr(3))
        r = inputs[3].resource
        if r is not None:
            ScreenWriter(sub, y, x, 12, 24).write_resource_name(r, attr(3))
        else:
            sub.addstr(12, 24, '  ?  ', attr(3))

        sub.addstr(12, 32, '{}:{}'.format(*self.get_ratio()))

class Play(State):

    display_message = '[Arrows]: Select  [Enter]: Play card  [Space]: Cancel'
    priority = PRI_HIGH

    def __init__(self, game, turn):
        self.game = game
        self.turn = turn
        self.selected = 0

        hand = game.current_player.development_cards
        self.cards = [card for card in DEVELOPMENT if hand[card]]

    def player_input(self, ch):
        selected = self.selected
        n_cards = len(self.cards)

        if ch == curses.KEY_UP:
            self.selected = (selected - 1) % n_cards
        elif ch == curses.KEY_DOWN:
            self.selected = (selected + 1) % n_cards
        elif ch == ord(' '):
            return DIE
        elif ch == ord('\n'):
            if self.turn.is_playable(self.cards[self.selected]):
                self.turn.play_card(self.cards[self.selected])
                return DIE
        else:
            return PASS
        return CONSUME

    def draw_state(self, y, x):
        w = 60
        h = 15

        sub = self.game.stdscr.subwin(h, w, (y - h) // 2, (x - w) // 2)
        sub.clear()
        sub.border()

        sub.addstr(2, 19, 'Choose a card to play')

        hand = self.game.current_player.development_cards

        if self.turn.is_playable(self.cards[self.selected]):
            sub.addstr(4 + self.selected, 3, '*')
        else:
            sub.addstr(4 + self.selected, 3, 'x',
                curses.A_BOLD | curses.color_pair(COLOR_INVALID))

        for i, card in enumerate(self.cards):
            sub.addstr(4 + i, 5, card.name, curses.color_pair(dev_color(card)))
            sub.addstr(4 + i, 25, str(hand[card]))

        for i, line in enumerate(self.cards[self.selected].effect.splitlines()):
            sub.addstr(10 + i, 5, line)

class StealFrom(State):
    '''Chooses which opponent from whom to steal'''

    display_name = '[Arrows]: Select player  [Enter]: Steal resource'
    priority = PRI_HIGH

    def __init__(self, game, targets):
        self.game = game
        self.targets = targets
        self.selected = 0

    def player_input(self, ch):
        n_targets = len(self.targets)
        if ch == curses.KEY_UP:
            self.selected = (self.selected - 1) % n_targets
        elif ch == curses.KEY_DOWN:
            self.selected = (self.selected + 1) % n_targets
        elif ch == ord('\n'):
            self.choose_player(self.targets[self.selected])
            return DIE
        else:
            return PASS
        return CONSUME

    def choose_player(self, player):
        '''
        Steals the resource from the player.
        NOTE: player argument indicates index into game.players
        rather than self.targets
        '''
        self.game.steal_resource(player)

    def draw_state(self, y, x):
        w = 50
        h = 10

        sub = self.game.stdscr.subwin(h, w, (y - h) // 2, (x - w) // 2)
        sub.clear()
        sub.border()

        sub.addstr(2, 9, 'Choose the target of the robber')

        p = self.game.cashan.players

        sub.addstr(4 + self.selected, 18, '*')

        for i, t in enumerate(self.targets):
            sub.addstr(4 + i, 20, p[t].name, curses.color_pair(COLOR_PLAYER0 + t))

class Victory(State):

    priority = PRI_HIGH

    def __init__(self, game):
        self.game = game

    def accepts_input(self, player):
        return player == self.game.self_player

    def player_input(self, ch):
        return PASS

    def draw_state(self, y, x):
        w = 50
        h = 10

        sub = self.game.stdscr.subwin(h, w, (y - h) // 2, (x - w) // 2)
        sub.clear()
        sub.border()

        players = self.game.cashan.players
        placing = self.game.get_placing()
        longest_name = max(len(p.name) for p in self.game.cashan.players)

        winner = next(iter(placing))
        winner_p = players[winner['player']]

        textw = len(winner_p.name) + 17
        startx = (w - textw) // 2
        sub.addstr(2, startx, winner_p.name,
            curses.color_pair(COLOR_PLAYER0 + winner['player']))
        sub.addstr(2, startx + len(winner_p.name) + 1,
            'has won the game!')

        for i, p in enumerate(placing):
            player_n = p['player']
            player = players[player_n]

            sub.addstr(4 + i, 5, '{}.'.format(p['place']))
            sub.addstr(4 + i, 8, player.name, curses.color_pair(COLOR_PLAYER0 + player_n))
            sub.addstr(4 + i, 10 + longest_name, '({:>2})'.format(p['points']))

class WaitRemote(State):
    '''Waits for a remote player to finish their turn'''

    def __init__(self, game):
        self.game = game
        self.display_message = 'Waiting for {}\'s turn...'.format(
            game.current_player.name)

    def accepts_input(self, player):
        # All inputs are ignored, but this is required to draw the state.
        return player == self.game.self_player

    def player_input(self, ch):
        return PASS

def dev_color(d):
    return {
        Knight: COLOR_KNIGHT,
        VictoryPoint: COLOR_VICTORY,
    }.get(d, COLOR_PROGRESS)

def resource_name(r):
    return {
        Brick: ('Brick', COLOR_BRICK),
        Lumber: ('Lumber', COLOR_LUMBER),
        Ore: (' Ore', COLOR_ORE),
        Grain: ('Grain', COLOR_GRAIN),
        Wool: (' Wool', COLOR_WOOL),
        None: ('  *', COLOR_ANY),
    }[r]

def terrain_name(tr):
    '''Returns short name and color for the given terrain type'''
    return {
        Hills: ('Hill', COLOR_HILLS),
        Forest: ('Frst', COLOR_FOREST),
        Mountains: ('Mnts', COLOR_MOUNTAINS),
        Fields: ('Flds', COLOR_FIELDS),
        Pasture: ('Pstr', COLOR_PASTURE),
        Desert: ('Dsrt', COLOR_DESERT),
    }[tr]

def get_config(args):
    name = input('What is your name? ')
    config = { 'name': name }

    save_config(args, config)

    return config

def config_path(file = None):
    if sys.platform.startswith('win'):
        path = os.path.expanduser('~/AppData/Cashan')
    else:
        path = os.path.expanduser('~/.config/cashan')

    if file:
        return os.path.join(path, file)
    return path

def load_config(args):
    with open(args.config, 'r') as f:
        return json.load(f)

def save_config(args, config):
    dirname = os.path.dirname(args.config)
    if dirname:
        os.makedirs(dirname, exist_ok = True)

    with open(args.config, 'w') as f:
        json.dump(config, f)
        f.write('\n')

    print('Configuration saved to', args.config)

def resume_game(name, players):
    with open(name, 'r') as f:
        state = Cashan.load_state(json.load(f))

    names = [p.name for p in state.players]
    if set(names) != set(players):
        raise Exception('player names do not match saved game: '
            '{!r} in this game; expected {!r}'.format(players, names))

    return state

def save_game(name, state):
    os.makedirs(config_path('games'), exist_ok = True)
    name = config_path('games/{}.json'.format(name))

    with open(name, 'w') as f:
        json.dump(state, f)
        f.write('\n')

def options():
    args = argparse.ArgumentParser(usage = '%(prog)s [OPTIONS]')

    args.add_argument('-c', '--config', action = 'store',
        metavar = 'FILE', help = 'Path to configuration file')
    args.add_argument('--host', action = 'store',
        metavar = 'NAME', help = 'Host a game')
    args.add_argument('--join', action = 'store',
        metavar = 'NAME', help = 'Join an existing game')
    args.add_argument('-l', '--log', metavar = 'FILE',
        help = 'Log debug messages to FILE')
    args.add_argument('--resume', action = 'store',
        help = 'Resume a saved game')
    args.add_argument('--save', action = 'store_true',
        help = 'Save game state at the beginning of each round')
    args.add_argument('--no-save', action = 'store_true',
        help = 'Do not save game state')
    args.add_argument('-p', '--players', type = int, action = 'store',
        metavar = 'N', help = 'Number of players in multiplayer', default = 4)
    args.add_argument('--server', action = 'store',
        metavar = 'HOST:PORT', help = 'Address of multiplayer server')

    args.add_argument('-V', '--version',
        action = 'version', version = 'cashan ' + VERSION)

    return args

AI_NAMES = ['Alice', 'Bob', 'Eve', 'Mallory']

def fill_names(players, n):
    '''
    Adds n AI names to the list of players and returns the shuffled list of names
    '''
    names = list(set(AI_NAMES) - set(players))
    random.shuffle(names)

    players.extend(names[:n])
    random.shuffle(players)
    return players

def parse_address(addr):
    addr, sep, port = addr.rpartition(':')

    if not sep:
        raise ValueError('invalid address')

    if addr.startswith('[') and addr.endswith(']'):
        addr = addr[1:-1]

    return (addr, int(port))

def auth_server(args, config):
    if args.server:
        server = args.server
        write_config = True
    elif 'server' in config:
        server = config['server']
        write_config = False
    else:
        server = input('Enter host:port of server: ')
        write_config = True

    address = parse_address(server)

    if write_config:
        config['server'] = server
        save_config(args, config)

    sock = socket.socket()
    sock.connect(address)
    conn = Connection(sock)

    conn.write_message({
        'action': 'hello',
        'name': config['name'],
        'version': VERSION,
    })

    msg = conn.recv_message()

    if msg is None:
        raise ClientError('connection closed')

    action = msg.get('action')

    if action == 'error':
        raise ClientError('got error from server: {}'.format(msg.get('error')))

    if action != 'hello':
        raise ClientError('unexpected message from server: {!r}', msg)

    # Successful authentication; return connection
    return conn

def wait_for_game_start(conn):
    while True:
        msg = conn.recv_message()

        if msg is None:
            raise ClientError('connection closed')

        action = msg.get('action')

        if action == 'host':
            print('Hosting game', msg.get('name'))
            print('Waiting for more players...')
        elif action == 'error':
            raise ClientError(msg.get('error'))
        elif action == 'join_game':
            print('Joined game', msg.get('name'))
            print('Waiting for more players...')
        elif action == 'join':
            print(msg.get('name'), 'joined the game')
        elif action == 'leave':
            print(msg.get('name'), 'left the game')
        elif action == 'ping':
            conn.write_message({ 'action': 'pong' })
        elif action == 'start':
            return msg
        else:
            raise ClientError('unexpected message: {!r}'.format(msg))

def make_save_name():
    return time.strftime('game %Y-%m-%d %H:%M:%S')

if __name__ == '__main__':
    args = options().parse_args()

    if args.config is None:
        args.config = config_path('cashan.cfg')
    if args.log:
        LOG_FILE = open(args.log, 'w')
    if args.players < 2 or args.players > 4:
        raise Exception('number of players must be between 2 and 4')
    if args.host and args.join:
        raise Exception('--host and --join are mutually exclusive')
    if args.join and args.resume:
        raise Exception('--join and --resume are incompatible')

    try:
        config = load_config(args)
    except FileNotFoundError:
        config = get_config(args)

    if args.host or args.join:
        conn = auth_server(args, config)

        if args.host:
            conn.write_message({ 'action': 'host', 'name': args.host,
                'players': args.players })
        elif args.join:
            conn.write_message({ 'action': 'join', 'name': args.join })

        state = wait_for_game_start(conn)

        if 'players' in state:
            players = state.get('players', list)

            if args.resume:
                state = resume_game(args.resume, players)
            else:
                random.shuffle(players)
                state = Cashan(random_grid(), players)

            conn.write_message({ 'action': 'send', 'body':
                { 'action': 'start', 'state': state.dump_state() } })
        elif 'state' in state:
            state = Cashan.load_state(state.get('state', dict))
        else:
            raise ClientError('unexpected message: {!r}'.format(state))

        play_state = PlayState(
            ai_players = [],
            self_player = index_of(state.players,
                lambda p: p.name == config['name']))
    else:
        conn = None
        names = fill_names([config['name']], args.players - 1)
        ai_players = [0, 1, 2, 3]
        self_player = names.index(config['name'])
        ai_players.pop(self_player)

        state = Cashan(random_grid(), names)

        play_state = PlayState(
            self_player = self_player,
            ai_players = ai_players)

    if args.save or (args.host and not args.no_save):
        args.save_name = args.host or args.join or make_save_name()
    else:
        args.save_name = None

    try:
        main(CashanGame, args, config, state, play_state, conn)
    finally:
        if LOG_FILE is not None:
            LOG_FILE.flush()
