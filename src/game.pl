:- use_module(library(random)).

:- consult('board.pl').
:- consult('display.pl').
:- consult('menu.pl').
:- consult('moves.pl').

% Defines what color is playing next

next_player(w, b).
next_player(b, w).

% Starts game

play :-
    % repeat,
        main_menu,
        read_input(Input),
        process_main_menu_input(Input),
        Input =:= 2.

% Player VS Player

game_loop(Player, GameState, 1) :-
    display_game(GameState, Player),
    choose_move_input(Player, GameState, NewGameState),
    clear_screen,
    next_player(Player, NextPlayer),
    game_loop(NextPlayer, NewGameState, 1).

% AI with random difficulty level

game_loop(Player, GameState, 2) :-
    display_game(GameState, Player),
    choose_move(GameState, Player, 2, NewGameState),
    clear_screen,
    next_player(Player, NextPlayer),
    game_loop(NextPlayer, NewGameState, 2).

% Random difficulty level

choose_move(GameState, Player, 2, Move) :-
    valid_moves(GameState, Player, ListOfMoves),
    length(ListOfMoves, NumberOfMoves),
    random(0, NumberOfMoves, R),
    nth0(R, ListOfMoves, Move).

% game(player, pc, strategy, GameState) :-
% game(pc, pc, strat1, strat2, GameState) :-

% game_over(GameState, Winner).
% value(GameState, Player, Value).
% choose_move(GameState, Player, Level, Move).
