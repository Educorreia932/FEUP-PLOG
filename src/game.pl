:- use_module(library(lists)).
:- use_module(library(random)).

:- consult('board.pl').
:- consult('display.pl').
:- consult('menu.pl').
:- consult('moves.pl').

% Defines what color is playing next

next_player(w, b).
next_player(b, w).

% Starts game

start_game(player, player, Rows, Columns) :-
    generate_board(Rows, Columns, GameState),
    game_loop(b, GameState, 0, 0, [1]).

% End of game loop

game_loop(_, GameState, 1, 1, _) :-
    game_over(GameState, Winner),
    format('The winner is ~w', Winner), !.

% Player VS Player

game_loop(Player, GameState, _, _, [1]) :-
    display_game(GameState, Player),
    choose_move_input(Player, GameState, NewGameState),
    clear_screen,
    next_player(Player, NextPlayer),
    game_loop(NextPlayer, NewGameState, 1).

% AI with random difficulty level

game_loop(Player, GameState, _, _, [3, 1]) :-
    display_game(GameState, Player),
    choose_move(GameState, Player, 1, NewGameState),
    (GameState == NewGameState -> 
        (Player == b -> BlackFinished is 1;
         Player == w -> WhiteFinished is 1);
     BlackFinished is 0, WhiteFinished is 0
    ),
    sleep(0.5),
    clear_screen,
    next_player(Player, NextPlayer),
    game_loop(NextPlayer, NewGameState, BlackFinished, WhiteFinished, [3, 1]).

% Random difficulty level

choose_move(GameState, Player, _, GameState) :-
    valid_moves(GameState, Player, []).

choose_move(GameState, Player, 1, Move) :-
    valid_moves(GameState, Player, ListOfMoves),
    length(ListOfMoves, NumberOfMoves),
    random(0, NumberOfMoves, R),
    nth0(R, ListOfMoves, Move).

game_over(GameState, Winner) :-
    value(GameState, b, BlackValue),
    value(GameState, w, WhiteValue),
    (BlackValue > WhiteValue -> Winner = 'Black';
    Winner = 'White').

% Calculate value 

value(GameState, Player, Value) :-
    flatten(GameState, Stacks),                     % Retrieve list of all stacks
    player_stacks(Stacks, Player, PlayerStacks),    % Retrieve player's stacks
    green_pieces(PlayerStacks, GreenPieces),        % Count the number of green pieces in each stack
    sum(GreenPieces, Value).                        % Get the total number of green pieces

% Retrieve stacks controlled by player

player_stacks(Stacks, Player, PlayerStacks) :- 
    findall(Stack, player_controls(Stacks, Stack, Player), PlayerStacks).

player_controls(Stacks, Stack, Player) :-
    member(Stack, Stacks),
    nth0(0, Stack, Player).

% Count the number of green pieces in each stack and map it

green_pieces([], []).

green_pieces([C|R], [TC|CR]) :-
    count(C, g, TC),
    green_pieces(R, CR).

