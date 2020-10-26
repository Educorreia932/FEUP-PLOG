:- consult('play.pl').
:- consult('board.pl').
:- consult('display.pl').

:- dynamic(board/1).

game :- play.
