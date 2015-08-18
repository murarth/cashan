# Cashan

ncurses-based game of resource management.

Cashan requires Python 3.x

Run:

    ./cashan_game.py

Run multiplayer server:

    ./cashan_server.py

Host a multiplayer game:

    ./cashan_game.py --host game-name

Join a multiplayer game:

    ./cashan_game.py --join game-name

# Rules

Cashan is played on a hexagonal grid of tiles.

Each tile is marked with a number and a type of terrain. Each type of terrain
produces a unique resource (except for the Desert, which does not produce
anything):

* **Hills** produce **Brick**.
* **Forest** produces **Lumber**.
* **Mountains** produce **Ore**.
* **Fields** produce **Grain**.
* **Pasture** produces **Wool**.

During each turn, a player will roll a pair of dice. The sum of the dice
determines which tiles produce resources on that turn. Players may then use
these resources to build roads on the edges of tiles or settlements on the
intersections.

The goal is to increase your Victory Points. Once a player has 10 Victory Points,
they can declare victory and win the game! Victory points are gained in a number
of ways:

* Each **Settlement** is worth **1** Victory Point.
* Each **City** is worth **2** Victory Points.

And some other stuff. Just figure it out.
