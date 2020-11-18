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

% move(GameState, Move, NewGameState) :-

game(player, player, GameState) :-  
    display_game(GameState, b), !.

% TODO: Only for debug purposes
game :-
    generate_board(6, 6), 
    initial(GameState), 
    display_game(GameState, b).

% game(player, pc, strategy, GameState) :-
% game(pc, pc, strat1, strat2, GameState) :-

game_over(GameState, Winner).
value(GameState, Player, Value).
choose_move(GameState, Player, Level, Move).