#!/usr/bin/python3
# -*- coding: utf-8

from copy import copy
from random import choice, randrange, shuffle

__all__ = [
    'Cashan',
    'Card', 'Development', 'Progress', 'SpecialCard',
    'Harbor', 'Item', 'Terrain', 'Object', 'Player',
    'beginner_grid', 'random_grid', 'roll',
    'Position', 'Edge', 'Intr',
    'Resource', 'Brick', 'Lumber', 'Ore', 'Grain', 'Wool', 'RESOURCES',
    'Knight', 'VictoryPoint', 'Monopoly', 'RoadBuilding', 'YearOfPlenty',
    'DEVELOPMENT',
    'LargestArmy', 'LongestRoad',
    'Road', 'Settlement', 'City', 'DevelopmentCard',
    'Hills', 'Forest', 'Mountains', 'Fields', 'Pasture', 'Desert', 'Sea',

    'MIN_LARGEST_ARMY', 'MIN_LONGEST_ROAD',

    'purchasable', 'resource_cards',

    'EDGE_N', 'EDGE_NE', 'EDGE_NW', 'EDGE_S', 'EDGE_SE', 'EDGE_SW',
    'INTR_NE', 'INTR_NW', 'INTR_E', 'INTR_SE', 'INTR_SW', 'INTR_W',
    'adjacent_cells', 'adjacent_to_edge', 'adjacent_to_intersection',
    'edges', 'intersections', 'edge_intersections', 'intr_edges',
    'edge_adjacent', 'intr_adjacent',
    'canonical_edge', 'canonical_intr', 'canonical_cell', 'pos_key',
]

# Minimum number of played Knight cards to be awarded "Largest Army"
MIN_LARGEST_ARMY = 3

# Minimum length of continuous road to be awarded "Longest Road"
MIN_LONGEST_ROAD = 5

class Cashan:

    def __init__(self, grid, player_names):
        self.grid = grid
        self.roads = Grid()
        self.buildings = Grid()
        self.robber = grid.find_desert()
        self.development_cards = development_cards()
        self.resources = resource_cards(19)
        self.special_cards = { LargestArmy, LongestRoad }
        self.players = [Player(name) for name in player_names]
        self.starting_positions = starting_positions()

    def dump_state(self):
        return {
            'grid': [add_field(cell.dump_state(), pos = pos)
                for pos, cell in self.grid],
            'roads': [{ 'pos': pos, 'edge': edge, 'owner': road.owner }
                for (pos, edge), road in self.roads],
            'buildings': [add_field(bldg.dump_state(), pos = pos, intr = intr)
                for (pos, intr), bldg in self.buildings],
            'robber': self.robber,
            'development_cards': [c.name for c in self.development_cards],
            'resources': { r.name: n for r, n in self.resources.items() },
            'special_cards': [c.name for c in self.special_cards],
            'players': [p.dump_state() for p in self.players],
            'starting_positions': self.starting_positions,
        }

    @classmethod
    def load_state(cls, state):
        obj = cls.__new__(cls)

        obj.grid = grid = Grid()
        for cell in state['grid']:
            grid[tuple(cell['pos'])] = Cell.load_state(cell)

        obj.roads = roads = Grid()
        for road in state['roads']:
            roads[(tuple(road['pos']), road['edge'])] = Object.load_state(road)

        obj.buildings = bldgs = Grid()
        for bldg in state['buildings']:
            bldgs[(tuple(bldg['pos']), bldg['intr'])] = Object.load_state(bldg)

        obj.robber = tuple(state['robber'])
        obj.development_cards = [Development.get(c) for c in state['development_cards']]
        obj.resources = { Resource.get(r): n for r, n in state['resources'].items() }
        obj.special_cards = { SpecialCard.get(c) for c in state['special_cards'] }
        obj.players = [Player.load_state(p) for p in state['players']]
        obj.starting_positions = [(tuple(pos), intr) for pos, intr in state['starting_positions']]

        return obj

    def accessible_harbors(self, player):
        return list(set(self.grid[p].harbor
            for (pos, intr), bldg in self.buildings if bldg.owner == player
                for p, _ in adjacent_to_intersection(pos, intr)
                    if p in self.grid and self.grid[p].harbor is not None))

    def can_build_road(self, player, pos, edge) -> bool:
        '''Returns whether the given player may build a road at the given location'''
        # Requires the following:
        # 1. The edge is a valid edge.
        # 2. No road exists at the target location.
        # 3. The target road would extend at least one existing road
        #    owned by the player.
        # 4. The intersection shared by the extended road does not contain
        #    a building owned by another player.
        #    (It may be empty or contain a building owned by the player.)
        return (self.edge_exists(pos, edge) and
            (pos, edge) not in self.roads and
            any(self.roads[pp, e].owner == player
                for p, i in edge_intersections(pos, edge)
                        if (p, i) not in self.buildings or
                            self.buildings[p, i].owner == player
                    for pp, e in intr_edges(p, i) if (pp, e) in self.roads))

    def can_build_settlement(self, player, pos, intr) -> bool:
        '''Returns whether the given player may build a Settlement at the given location'''
        # Requires the following:
        # 1. The intersection is a valid intersection.
        # 2. There is no building at the target location.
        # 3. There is at least one road owned by the player in an adjacent edge.
        # 4. There are no buildings in any adjacent intersection.
        return (self.intr_exists(pos, intr) and
            (pos, intr) not in self.buildings and
            any(self.roads[(p, e)].owner == player
                for p, e in intr_edges(pos, intr) if (p, e) in self.roads) and
            not any((p, i) in self.buildings for p, i in intr_adjacent(pos, intr)))

    def count_victory_points(self, player) -> int:
        '''
        Returns the player's total victory points,
        including unplayed VictoryPoint cards.
        '''
        players = self.players

        n = player.development_cards[VictoryPoint]
        n += sum([1
            for _, obj in self.buildings.items()
                if players[obj.owner] is player and obj.ty is Settlement])
        n += sum([2
            for _, obj in self.buildings.items()
                if players[obj.owner] is player and obj.ty is City])

        if LargestArmy in player.special_cards:
            n += 2
        if LongestRoad in player.special_cards:
            n += 2

        return n

    def cell_exists(self, pos, include_sea = False) -> bool:
        return pos in self.grid and (include_sea or
            self.grid[pos].terrain is not Sea)

    def edge_exists(self, pos, edge) -> bool:
        '''Returns whether the edge is a valid edge on which to build a road'''
        opp, _ = adjacent_to_edge(pos, edge)
        return any(p in self.grid and self.grid[p].terrain is not Sea
            for p in [pos, opp])

    def get_trade_ratio(self, player, resource):
        '''
        Returns the best possible trade ratio for the given player,
        trading the given resource.
        If resource is None, only general harbors will be considered.
        '''
        harbors = self.accessible_harbors(player)

        # Default trade ratio is 4:1
        ratio = (4, 1)

        for h in harbors:
            if h.resource is resource or h.resource is None:
                ratio = min(ratio, h.ratio)

        return ratio

    def intr_exists(self, pos, intr) -> bool:
        return any(p in self.grid and self.grid[p].terrain is not Sea
            for p, _ in adjacent_to_intersection(pos, intr))

    def building_exists(self, pos, intr) -> bool:
        '''Returns whether a building exists at the given intersection'''
        return (pos, intr) in self.buildings

    def road_exists(self, pos, edge) -> bool:
        '''Returns whether a road exists at the given edge'''
        return (pos, edge) in self.roads

    def longest_road(self, player: int) -> int:
        roads = [edge for edge, road in self.roads if road.owner == player]

        if not roads:
            return 0

        return max(self.longest_road_inner(player, edge, intr, 1, set())
            for edge in roads
                for intr in edge_intersections(*edge))

    def longest_road_inner(self, player, edge, fwd_intr, length, visited) -> int:
        '''
        Follows the road indicated by edge in one direction along
        fwd_intr and returns the maximum length of any
        branches.
        '''
        # An opposing player's settlement breaks up a continuous road
        bldg = self.buildings.get(fwd_intr)
        if bldg is not None and bldg.owner != player:
            return length

        visited.add(edge)

        max_length = length

        for e in intr_edges(*fwd_intr):
            if e in visited:
                continue

            road = self.roads.get(e)
            if road is not None and road.owner == player:
                intrs = edge_intersections(*e)
                intrs.remove(fwd_intr)
                next_intr, = intrs

                max_length = max(max_length,
                    self.longest_road_inner(player, e, next_intr,
                        length + 1, visited))

        visited.remove(edge)
        return max_length

def add_field(obj, **kws):
    obj.update(kws)
    return obj

class Card:

    INSTANCES = {}

    def __init__(self, name):
        self.name = name
        self.add_instance(name, self)

    @classmethod
    def add_instance(cls, name, inst):
        if name in cls.INSTANCES:
            raise Exception('duplicate {} {!r}'.format(cls.__name__, name))
        cls.INSTANCES[name] = inst

    @classmethod
    def get(cls, name):
        inst = cls.INSTANCES[name]
        if not isinstance(inst, cls):
            raise Exception('expected cached instance of type {!r}; found {!r}'
                .format(cls.__name__, type(inst).__name__))
        return inst

    def __repr__(self):
        return self.name

class SpecialCard(Card): pass
class Resource(Card): pass

class Development(Card):

    def __init__(self, name, effect):
        super().__init__(name)
        self.effect = effect.strip()

class Progress(Development): pass

class Item(Card):

    def __init__(self, name, cost):
        super().__init__(name)
        self.cost = cost

class Harbor:

    def __init__(self, ratio, resource):
        self.ratio = ratio
        self.resource = resource

    def __repr__(self):
        return 'Harbor({!r}, {!r})'.format(self.ratio, self.resource)

    def dump_state(self):
        obj = {
            'ratio': self.ratio,
        }
        if self.resource is not None:
            obj['resource'] = self.resource.name
        return obj

    @classmethod
    def load_state(cls, state):
        ratio = tuple(state['ratio'])
        resource = Resource.get(state['resource']) if 'resource' in state else None
        return cls(ratio, resource)

class Terrain(Card):

    def __init__(self, name, resource):
        super().__init__(name)
        self.resource = resource

class Object:
    '''Represents a settlement, city, or road placed on the field'''

    def __init__(self, ty, owner):
        self.ty = ty
        self.owner = owner

    def dump_state(self):
        if self.ty is Road:
            return { 'owner': self.owner }
        else:
            return { 'type': self.ty.name, 'owner': self.owner }

    @classmethod
    def load_state(cls, state):
        ty = Item.get(state.get('type', 'Road'))
        return cls(ty, state['owner'])

class Player:

    def __init__(self, name):
        self.name = name
        self.cities = 4
        self.settlements = 5
        self.roads = 15
        self.development_cards = {
            Knight: 0,
            Monopoly: 0,
            RoadBuilding: 0,
            YearOfPlenty: 0,
            VictoryPoint: 0,
        }
        self.played_development_cards = {
            Knight: 0,
        }
        self.resources = resource_cards(0)
        self.special_cards = set()

    def __repr__(self):
        return '<Player {!r}>'.format(self.name)

    def can_buy(self, item):
        return all(self.resources[r] >= n for n, r in item.cost)

    def dump_state(self):
        return {
            'name': self.name,
            'cities': self.cities,
            'settlements': self.settlements,
            'roads': self.roads,
            'development_cards': { c.name: n
                for c, n in self.development_cards.items() },
            'played_development_cards': { c.name: n
                for c, n in self.played_development_cards.items() },
            'resources': { r.name: n for r, n in self.resources.items() },
            'special_cards': [c.name for c in self.special_cards],
        }

    @classmethod
    def load_state(cls, state):
        obj = cls.__new__(cls)

        obj.name = state['name']
        obj.cities = state['cities']
        obj.settlements = state['settlements']
        obj.roads = state['roads']
        obj.development_cards = { Development.get(c): n
            for c, n in state['development_cards'].items() }
        obj.played_development_cards = { Development.get(c): n
            for c, n in state['played_development_cards'].items() }
        obj.resources = { Resource.get(r): n
            for r, n in state['resources'].items() }
        obj.special_cards = { SpecialCard.get(c) for c in state['special_cards'] }

        return obj

def development_cards():
    '''Returns a shuffled list of Development cards'''
    deck = sum([
        [Knight] * 14,
        [Monopoly] * 2,
        [RoadBuilding] * 2,
        [YearOfPlenty] * 2,
        [VictoryPoint] * 5,
    ], [])

    shuffle(deck)
    return deck

def resource_cards(n):
    '''Returns a dict of counts of Resource cards'''
    return {
        # Copy objects because sometimes an object is passed
        Brick: copy(n),
        Lumber: copy(n),
        Ore: copy(n),
        Grain: copy(n),
        Wool: copy(n),
    }

def starting_positions():
    intrs = [
        (( 0, -1), INTR_NW),
        (( 0, -1), INTR_E),
        (( 1, -1), INTR_E),
        (( 1,  0), INTR_E),
        (( 0,  1), INTR_E),
        (( 0,  1), INTR_SW),
        ((-1,  1), INTR_SW),
        ((-1,  0), INTR_SW),
        ((-1,  0), INTR_NW),
    ]
    return [canonical_intr(pos, intr) for pos, intr in intrs]

def purchasable():
    '''Returns a list of all purchasable item types'''
    return [Road, Settlement, City, DevelopmentCard]

class Grid:
    '''
    Represents a hexagonal grid.  Cells are indexed by (x, y).
    '''

    def __init__(self):
        self.cells = {}

    def __contains__(self, key):
        return key in self.cells

    def __len__(self):
        return len(self.cells)

    def __delitem__(self, key):
        del self.cells[key]

    def __getitem__(self, key):
        return self.cells[key]

    def __setitem__(self, key, value):
        self.cells[key] = value

    def find_desert(self):
        '''Returns the grid position of the Desert cell'''
        for pos, cell in self.cells.items():
            if cell.terrain is Desert:
                return pos
        raise RuntimeError('no Desert cell found')

    def get(self, key, default = None):
        return self.cells.get(key, default)

    def __iter__(self):
        return iter(self.items())

    def items(self):
        return self.cells.items()

def beginner_grid():
    grid = Grid()

    # Beginner setup from rules
    grid[(-2,  0)] = Cell(Hills, 5)
    grid[(-2,  1)] = Cell(Fields, 6)
    grid[(-2,  2)] = Cell(Pasture, 11)
    grid[(-1, -1)] = Cell(Forest, 8)
    grid[(-1,  0)] = Cell(Mountains, 3)
    grid[(-1,  1)] = Cell(Fields, 4)
    grid[(-1,  2)] = Cell(Pasture, 5)
    grid[( 0, -2)] = Cell(Fields, 9)
    grid[( 0, -1)] = Cell(Forest, 11)
    grid[( 0,  0)] = Cell(Desert, None)
    grid[( 0,  1)] = Cell(Forest, 3)
    grid[( 0,  2)] = Cell(Mountains, 8)
    grid[( 1, -2)] = Cell(Fields, 2)
    grid[( 1, -1)] = Cell(Hills, 6)
    grid[( 1,  0)] = Cell(Pasture, 4)
    grid[( 1,  1)] = Cell(Hills, 10)
    grid[( 2, -2)] = Cell(Mountains, 10)
    grid[( 2, -1)] = Cell(Pasture, 2)
    grid[( 2,  0)] = Cell(Forest, 9)

    grid[(-1, -2)] = Cell(Sea, None, Harbor((2, 1), Brick))
    grid[( 1, -3)] = Cell(Sea, None, Harbor((2, 1), Lumber))
    grid[( 3, -3)] = Cell(Sea, None, Harbor((3, 1), None))
    grid[( 3, -1)] = Cell(Sea, None, Harbor((2, 1), Grain))
    grid[( 2,  1)] = Cell(Sea, None, Harbor((2, 1), Ore))
    grid[( 0,  3)] = Cell(Sea, None, Harbor((3, 1), None))
    grid[(-2,  3)] = Cell(Sea, None, Harbor((2, 1), Wool))
    grid[(-3,  2)] = Cell(Sea, None, Harbor((3, 1), None))
    grid[(-3,  0)] = Cell(Sea, None, Harbor((3, 1), None))

    return grid

def random_grid():
    grid = Grid()

    terrain = sum([
        [Desert],
        [Fields] * 4,
        [Forest] * 4,
        [Pasture] * 4,
        [Mountains] * 3,
        [Hills] * 3,
    ], [])

    numbers = [
        2,
        3, 3,
        4, 4,
        5, 5,
        6, 6,
        # No '7' tokens
        8, 8,
        9, 9,
        10, 10,
        11, 11,
        12,
    ]

    harbors = [
        Harbor((2, 1), Brick),
        Harbor((2, 1), Lumber),
        Harbor((2, 1), Ore),
        Harbor((2, 1), Grain),
        Harbor((2, 1), Wool),
    ] + [Harbor((3, 1), None)] * 4

    shuffle(terrain)
    shuffle(numbers)
    shuffle(harbors)

    cells = [
                            (-2,  0), (-2,  1), (-2,  2),
                  (-1, -1), (-1,  0), (-1,  1), (-1,  2),
        ( 0, -2), ( 0, -1), ( 0,  0), ( 0,  1), ( 0,  2),
        ( 1, -2), ( 1, -1), ( 1,  0), ( 1,  1),
        ( 2, -2), ( 2, -1), ( 2,  0),
    ]

    harbor_cells = choice([
        [
            (-1, -2),
            ( 1, -3),
            ( 3, -3),
            ( 3, -1),
            ( 2,  1),
            ( 0,  3),
            (-2,  3),
            (-3,  2),
            (-3,  0),
        ],
        [
            ( 0, -3),
            ( 2, -3),
            ( 3, -2),
            ( 3,  0),
            ( 1,  2),
            (-1,  3),
            (-3,  3),
            (-3,  1),
            (-2, -1),
        ],
    ])

    for pos in cells:
        tr = terrain.pop()
        if tr is Desert:
            grid[pos] = Cell(tr, None)
        else:
            grid[pos] = Cell(tr, numbers.pop())

    for pos in harbor_cells:
        grid[pos] = Cell(Sea, None, harbors.pop())

    return grid

class Cell:

    def __init__(self, terrain, number, harbor = None):
        self.terrain = terrain
        self.number = number
        self.harbor = harbor

    def __repr__(self):
        if self.harbor is None:
            return 'Cell({!r}, {!r})'.format(self.terrain, self.number)
        else:
            return 'Cell({!r}, {!r}, harbor={!r})'.format(
                self.terrain, self.number, self.harbor)

    def dump_state(self):
        obj = {
            'terrain': self.terrain.name,
        }
        if self.number is not None:
            obj['number'] = self.number
        if self.harbor is not None:
            obj['harbor'] = self.harbor.dump_state()

        return obj

    @classmethod
    def load_state(cls, state):
        tr = Terrain.get(state['terrain'])
        n = state.get('number')
        harbor = Harbor.load_state(state['harbor']) if 'harbor' in state else None
        return cls(tr, n, harbor)

def roll():
    '''Rolls two six-sided dice and returns the sum'''
    a = randrange(1, 7)
    b = randrange(1, 7)
    return a + b

Brick = Resource('Brick')
Lumber = Resource('Lumber')
Ore = Resource('Ore')
Grain = Resource('Grain')
Wool = Resource('Wool')
RESOURCES = [Brick, Lumber, Ore, Grain, Wool]

Knight = Development('Knight',
'''
Move the robber. Steal 1 resource card from
the owner of an adjacent settlement or city.
''')
VictoryPoint = Development('Victory Point',
'''
Adds 1 Victory Point to your total.
'''
)

# Progress cards
Monopoly = Development('Monopoly',
'''
When you play this card, announce 1 type
of resource. All other players must give you
all their resource cards of that type.
''')
RoadBuilding = Development('Road Building',
'''
Place 2 new roads as if you had just built them.
''')
YearOfPlenty = Development('Year of Plenty',
'''
Take any 2 resources from the bank.
Add them to your hand. They can be 2 of
the same resource or 2 different resources.
''')

DEVELOPMENT = [Knight, Monopoly, RoadBuilding, YearOfPlenty, VictoryPoint]

LargestArmy = SpecialCard('Largest Army')
LongestRoad = SpecialCard('Longest Road')

Road = Item('Road', [
    (1, Brick),
    (1, Lumber),
])
Settlement = Item('Settlement', [
    (1, Brick),
    (1, Lumber),
    (1, Wool),
    (1, Grain),
])
City = Item('City', [
    (3, Ore),
    (2, Grain),
])
DevelopmentCard = Item('Development Card', [
    (1, Ore),
    (1, Wool),
    (1, Grain),
])

Hills = Terrain('Hills', Brick)
Forest = Terrain('Forest', Lumber)
Mountains = Terrain('Mountains', Ore)
Fields = Terrain('Fields', Grain)
Pasture = Terrain('Pasture', Wool)
Desert = Terrain('Desert', None)
Sea = Terrain('Sea', None)

class Position:

    @classmethod
    def instance_check(cls, inst):
        return (isinstance(inst, (tuple, list)) and
            len(inst) == 2 and all(isinstance(i, int) for i in inst))

    @classmethod
    def instance_prepare(cls, inst):
        return tuple(inst)

class Edge:

    @classmethod
    def instance_check(cls, inst):
        return isinstance(inst, str) and inst in EDGES

class Intr:

    @classmethod
    def instance_check(cls, inst):
        return isinstance(inst, str) and inst in INTRS

EDGE_N = 'n'
EDGE_NE = 'ne'
EDGE_NW = 'nw'
EDGE_S = 's'
EDGE_SE = 'se'
EDGE_SW = 'sw'
EDGES = { EDGE_N, EDGE_NE, EDGE_NW, EDGE_S, EDGE_SE, EDGE_SW }

INTR_NE = 'ne'
INTR_NW = 'nw'
INTR_E = 'e'
INTR_SE = 'se'
INTR_SW = 'sw'
INTR_W = 'w'
INTRS = { INTR_NE, INTR_NW, INTR_E, INTR_SE, INTR_SW, INTR_W }

def edges(pos):
    '''Returns each edge adjacent to the given cell'''
    return [canonical_edge(pos, e) for e in
        [EDGE_N, EDGE_NE, EDGE_NW, EDGE_S, EDGE_SE, EDGE_SW]]

def intersections(pos):
    '''Returns each intersection adjacent to the given cell'''
    return [canonical_intr(pos, i) for i in
        [INTR_NE, INTR_NW, INTR_E, INTR_SE, INTR_SW, INTR_W]]

def edge_intersections(pos, edge):
    '''Returns the two intersections adjacent to the given edge'''
    return [canonical_intr(p, i) for p, i in edge_intr_inner(pos, edge)]

def edge_intr_inner(pos, edge):
    x, y = pos
    if edge == EDGE_N:
        return [(pos, INTR_NW), (pos, INTR_NE)]
    elif edge == EDGE_NE:
        return [(pos, INTR_NE), (pos, INTR_E)]
    elif edge == EDGE_NW:
        return [(pos, INTR_W), (pos, INTR_NW)]
    elif edge == EDGE_S:
        return [(pos, INTR_SE), (pos, INTR_SW)]
    elif edge == EDGE_SE:
        return [(pos, INTR_E), (pos, INTR_SE)]
    elif edge == EDGE_SW:
        return [(pos, INTR_SW), (pos, INTR_W)]
    else:
        raise Exception('invalid edge: {!r}'.format(edge))

def edge_adjacent(pos, edge):
    '''Returns the four edges sharing an intersection with this edge'''
    return list(set((pp, e)
        for p, i in edge_intersections(pos, edge)
            for pp, e in intr_edges(p, i) if (pp, e) != (pos, edge)))

def intr_adjacent(pos, intr):
    '''Returns the three intersections adjacent to this intersection'''
    return list(set((pp, i)
        for p, e in intr_edges(pos, intr)
            for pp, i in edge_intersections(p, e) if (pp, i) != (pos, intr)))

def intr_edges(pos, intr):
    '''Returns the three edges adjacent to the intersection'''
    return [canonical_edge(p, e) for p, e in intr_edges_inner(pos, intr)]

def intr_edges_inner(pos, intr):
    x, y = pos
    if intr == INTR_NE:
        return [(pos, EDGE_N), (pos, EDGE_NE), ((x, y - 1), EDGE_SE)]
    elif intr == INTR_NW:
        return [(pos, EDGE_NW), (pos, EDGE_N), ((x - 1, y), EDGE_NE)]
    elif intr == INTR_E:
        return [(pos, EDGE_NE), (pos, EDGE_SE), ((x + 1, y - 1), EDGE_S)]
    elif intr == INTR_SE:
        return [(pos, EDGE_SE), (pos, EDGE_S), ((x + 1, y), EDGE_SW)]
    elif intr == INTR_SW:
        return [(pos, EDGE_S), (pos, EDGE_SW), ((x, y + 1), EDGE_NW)]
    elif intr == INTR_W:
        return [(pos, EDGE_SW), (pos, EDGE_NW), ((x - 1, y + 1), EDGE_N)]
    else:
        raise Exception('invalid intersection: {!r}'.format(intr))

def adjacent_cells(pos):
    '''Returns the set of cell positions adjacent to the given position'''
    x, y
    return [
        (x - 1, y), (y - 1, x),
        (x + 1, y), (y + 1, x),
        (x - 1, y + 1), (x + 1, y - 1),
    ]

def adjacent_to_edge(pos, edge):
    '''
    Returns the position of the cell adjacent along the given edge
    and the edge specifier from that cell.
    '''
    x, y = pos
    if edge == EDGE_N:
        return ((x, y - 1), EDGE_S)
    elif edge == EDGE_NE:
        return ((x + 1, y - 1), EDGE_SW)
    elif edge == EDGE_NW:
        return ((x - 1, y), EDGE_SE)
    elif edge == EDGE_S:
        return ((x, y + 1), EDGE_N)
    elif edge == EDGE_SE:
        return ((x + 1, y), EDGE_NW)
    elif edge == EDGE_SW:
        return ((x - 1, y + 1), EDGE_NE)
    else:
        raise Exception('invalid edge: {!r}'.format(edge))

def adjacent_to_intersection(pos, intr):
    '''
    Returns the three cells adjacent to this intersection and
    the intersection specifier from those cells.
    '''
    x, y = pos
    if intr == INTR_NE:
        return [(pos, intr), ((x, y - 1), INTR_SE), ((x + 1, y - 1), INTR_W)]
    elif intr == INTR_NW:
        return [(pos, intr), ((x - 1, y), INTR_E), ((x, y - 1), INTR_SW)]
    elif intr == INTR_E:
        return [(pos, intr), ((x + 1, y - 1), INTR_SW), ((x + 1, y), INTR_NW)]
    elif intr == INTR_SE:
        return [(pos, intr), ((x + 1, y), INTR_W), ((x, y + 1), INTR_NE)]
    elif intr == INTR_SW:
        return [(pos, intr), ((x, y + 1), INTR_NW), ((x - 1, y + 1), INTR_E)]
    elif intr == INTR_W:
        return [(pos, intr), ((x - 1, y + 1), INTR_NE), ((x - 1, y), INTR_SE)]
    else:
        raise Exception('invalid intersection: {!r}'.format(intr))

def canonical_edge(pos, edge):
    cells = [(pos, edge), adjacent_to_edge(pos, edge)]
    return min(cells, key = lambda k: pos_key(k[0]))

def canonical_intr(pos, intr):
    cells = adjacent_to_intersection(pos, intr)
    return min(cells, key = lambda k: pos_key(k[0]))

def canonical_cell(cells):
    '''
    Returns the canonical cell position of a set of positions.
    Because edges and intersections are adjacent to multiple cells,
    a single cell must be deterministically chosen to be considered the
    "owner" of any given edge or intersection.
    '''
    return min(cells, key = pos_key)

def pos_key(pos):
    '''
    Returns sorting key for a canonical position sort.
    This guarantees that there will be only one canonical "closest to zero"
    cell position.
    '''
    x, y = pos
    return abs(x), abs(y), sign(x), sign(y)

def sign(n):
    if n == 0:
        return 0
    elif n < 0:
        return 1
    else:
        return 2
