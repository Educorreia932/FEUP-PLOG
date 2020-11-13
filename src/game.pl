:- consult('play.pl').
:- consult('board.pl').
:- consult('display.pl').

game :- play. % Starts game

valid_moves(+GameState, +Player, -ListOfMoves).
move(+GameState, +Move, -NewGameState).
game_over(+GameState, -Winner).
value(+GameState, +Player, -Value).
choose_move(+GameState, +Player, +Level, -Move).