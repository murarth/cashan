#!/usr/bin/python3
# -*- coding: utf-8

import curses
from functools import partial
import signal
import time

__all__ = [
    'ctrl', 'main',
    'Game',
]

def ctrl(ch):
    return ord(ch) & 0x1f

def main(game_class, *args, **kws):
    stdscr = curses.initscr()
    try:
        game_class(stdscr, *args, **kws).go()
    finally:
        curses.endwin()

class Game(object):

    GAME_TITLE = NotImplemented

    def __init__(self, stdscr):
        self.stdscr = stdscr
        self.message = None
        self.message_timeout = None
        self.queue_redraw = True
        self.quit = False
        self.stopped = False

    def clear_message(self):
        '''
        Clears message line
        '''
        self.message = None
        self.message_timeout = None
        self.queue_redraw = True

    def set_message(self, msg, timeout = 1):
        '''
        Sets a message to display for the given timeout, in seconds.
        If timeout is None, the message will be displayed until clear_message
        is called.
        '''
        self.message = msg
        if timeout is None:
            self.message_timeout = None
        else:
            self.message_timeout = time.time() + timeout
        self.queue_redraw = True

    def draw(self):
        '''
        Draws the contents of the screen
        '''
        win = self.stdscr

        y, x = win.getmaxyx()
        win.clear()

        try:
            self.draw_title(y, x)

            if self.stopped:
                self.draw_stopped(y, x)
            else:
                self.draw_field(y, x)

            self.draw_message(y, x)
            self.refresh()
        except curses.error as e:
            msg = 'Screen is too small'
            self.draw_line(y - 1, 0, msg[:x], curses.A_BOLD)
            win.refresh()

    def draw_field(self, y, x):
        raise NotImplementedError

    def draw_message(self, y, x):
        '''Draws message'''
        win = self.stdscr
        if self.message:
            win.addstr(y - 1, 0, self.message[:x], curses.A_BOLD)

    def draw_stopped(self, y, x):
        raise NotImplementedError

    def draw_centered(self, y, x, s, attr = 0):
        '''
        Draws a string centered on the screen.
        y is line to draw, x is the max x value of the screen.
        '''
        self.stdscr.addstr(y, (x - len(s)) // 2, s, attr)

    def draw_line(self, y, x, s, attr = 0):
        self.stdscr.addstr(y, x, s, attr)

    def draw_title(self, y, x):
        '''Draws the title to the screen'''
        self.stdscr.addstr(0, 1, self.GAME_TITLE)
        self.stdscr.chgat(0, 0, x, curses.A_REVERSE)

    def go(self):
        self.init_ui()
        self.start_game()

        while not self.quit:
            self.before_tick()

            if self.queue_redraw:
                self.draw()
                self.queue_redraw = False

            self.poll_input()

            self.after_tick()

        self.end_game()

    def after_tick(self):
        if self.message_timeout and self.message_timeout <= time.time():
            self.clear_message()

    def before_tick(self):
        pass

    def end_game(self):
        pass

    def poll_input(self):
        ch = self.stdscr.getch()

        if ch != -1:
            self.handle_input(ch)

    def handle_input(self, ch):
        raise NotImplementedError

    def init_ui(self):
        self.stdscr.timeout(100)
        self.stdscr.keypad(True)
        curses.curs_set(0)
        curses.noecho()
        signal.signal(signal.SIGWINCH, self.win_resized)
        self.init_colors()

    def init_colors(self):
        curses.start_color()
        curses.use_default_colors()

    def new_game(self):
        raise NotImplementedError

    def quit_game(self):
        self.quit = True

    def start_game(self):
        raise NotImplementedError

    def redraw(self):
        self.queue_redraw = True

    def refresh(self):
        self.stdscr.refresh()

    def win_resized(self, *args):
        curses.endwin()
        curses.initscr()
        self.queue_redraw = True
