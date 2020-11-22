:- use_module(library(lists)).
:- use_module(library(random)).

:- consult('board.pl').
:- consult('display.pl').
:- consult('menu.pl').
:- consult('moves.pl').
:- consult('utils.pl').


% Defines what color is playing next

next_player(w, b).
next_player(b, w).

% Starts game

start_game(player, player, Rows, Columns) :-            % Starts PvP game
    generate_board(Rows, Columns, GameState),           % Generates board
    game_loop(player, player, b, GameState, 0, 0).      % Starts game with black playing first

start_game(player, ai, Strat, Rows, Columns) :-         % Starts Player vs AI game
    generate_board(Rows, Columns, GameState),           % Generates board
    print('Not yet implemented'), nl.                   % Starts game with white playing first

start_game(ai, ai, Strat1, Start2, Rows, Columns) :-    % Starts AI vs AI game
    generate_board(Rows, Columns, GameState),           % Generates board
    game_loop(ai, ai, b, GameState, 0, 0).              % Starts game with white playing first


% End of game loop

game_loop(_, _, _, GameState, 1, 1) :-
    game_over(GameState, Winner),
    format('The winner is ~w', Winner), !.


% Player VS Player

game_loop(player, player, Player, GameState, _, _) :-
    display_game(GameState, Player),
    move_input(Player, GameState, NewGameState),
    (GameState == NewGameState -> 
        (Player == b -> BlackFinished is 1;
         Player == w -> WhiteFinished is 1);
     BlackFinished is 0, WhiteFinished is 0
    ),
    clear_screen,
    next_player(Player, NextPlayer),
    game_loop(player, player, NextPlayer, NewGameState, BlackFinished, WhiteFinished).


% AI vs AI

game_loop(ai, ai, Player, GameState, _, _) :-
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

