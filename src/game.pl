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

game(Player, GameState) :-  
    display_game(GameState, Player),
    choose_move(Player, GameState, NewGameState),
    clear_screen,
    next_player(Player, NextPlayer),
    game(NextPlayer, NewGameState).

% game(player, pc, strategy, GameState) :-
% game(pc, pc, strat1, strat2, GameState) :-

% game_over(GameState, Winner).
% value(GameState, Player, Value).
% choose_move(GameState, Player, Level, Move).
