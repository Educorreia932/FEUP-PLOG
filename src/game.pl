:- consult('board.pl').
:- consult('display.pl').
:- consult('menu.pl').
:- consult('play.pl').

% Starts game

play :-
    % repeat,
        main_menu,
        read(Input),
        process_main_menu_input(Input),
        Input =:= 2.

valid_moves(+GameState, +Player, -ListOfMoves).
move(+GameState, +Move, -NewGameState).
game_over(+GameState, -Winner).
value(+GameState, +Player, -Value).
choose_move(+GameState, +Player, +Level, -Move).