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

start_game(Strats, Rows, Columns) :-   % Starts PvP game
    generate_board(Rows, Columns, GameState),              % Generates board
    game_loop(b, Strats, GameState, 0, 0).                 % Starts game with black playing first

start_game(Strats, Rows, Columns) :-       % Starts Player vs AI game
    generate_board(Rows, Columns, GameState),              % Generates board
    game_loop(b, Strats, GameState, 0, 0).                 % Starts game with black playing first

start_game(Strats, Rows, Columns) :-     % Starts AI vs AI game
    generate_board(Rows, Columns, GameState),              % Generates board
    game_loop(b, Strats, GameState, 0, 0).                 % Starts game with black playing first

% Game Over

game_over(GameState, Winner) :-
    value(GameState, b, BlackValue),          % Calculates value for black 
    value(GameState, w, WhiteValue),          % Calculates value for white
    winner(BlackValue, WhiteValue, Winner).   % Sets winner

winner(BlackValue, WhiteValue, 'Black') :- BlackValue > WhiteValue.
winner(BlackValue, WhiteValue, 'White') :- BlackValue < WhiteValue.
winner(BlackValue, WhiteValue, 'Draw') :- BlackValue =:= WhiteValue.

game_loop(_, _, GameState, 1, 1) :-
    game_over(GameState, Winner),
    (Winner = 'Draw' -> format('There\'s no winner', Winner);   
    format('\nThe winner is ~w', Winner)), !.

% Convert player color  to an index

player_index(b, 0).
player_index(w, 1).

% Check if player has already finished playing

finished_playing(GameState, NewGameState, _, 0) :-  
    GameState \== NewGameState, !.                  % Differents GameStates means player can play

finished_playing(GameState, GameState, w, 1).  % White player can't play
finished_playing(GameState, GameState, b, 1).  % Black player can't play

% Player strategy

get_move(Player, GameState, 'player', NewGameState) :-
    move_input(Player, GameState, NewGameState).

% AI strategy

get_move(Player, GameState, Strat, NewGameState) :-
    choose_move(GameState, Player, Strat, NewGameState),
    sleep(0.5).

% Game loop

game_loop(Player, Strats, GameState, _BlackFinished, _WhiteFinished) :-
    display_game(GameState, Player),                     % Display the current state of the game
    player_index(Player, PlayerIndex),                   % Convert current player to an index
    nth0(PlayerIndex, Strats, Strat),                    % Get current strategy
    get_move(Player, GameState, Strat, NewGameState),    % Get next move
    clear_screen,                                        % Clear the screen
    next_player(Player, NextPlayer),                     
    
    finished_playing(GameState, NewGameState, Player, Finished),
    (Player = b -> game_loop(NextPlayer, Strats, NewGameState, Finished, _WhiteFinished);
     Player = w -> game_loop(NextPlayer, Strats, NewGameState, _BlackFinished, Finished)
    ), !.

% There are no valid moves

choose_move(GameState, Player, _, GameState) :-
    valid_moves(GameState, Player, []).             

% Random AI

choose_move(GameState, Player, randomAI, Move) :-
    valid_moves(GameState, Player, ListOfMoves),    % Calculates valid moves
    length(ListOfMoves, NumberOfMoves),             % Gets number of valid moves
    random(0, NumberOfMoves, R),                    % Choose a random number
    nth0(R, ListOfMoves, Move).                     % Choose a random move

% Smart AI

choose_move(GameState, Player, smartAI, Move) :-
    valid_moves(GameState, Player, ListOfMoves),     % Calculates valid moves
    moves_values(ListOfMoves, Player, MovesValues),  % Calculate value for each move
    max_list(MovesValues, _, Index),                 % Get the highest value move
    nth0(Index, ListOfMoves, Move).                  % Choose the highest value move

% Calculate value of move

value(GameState, Player, Value) :-
    flatten(GameState, AllStacks),                  % Retrieve list of all stacks
    get_stacks(AllStacks, Player, PlayerStacks),    % Retrieve player's stacks
    green_pieces(PlayerStacks, GreenPieces),        % Count the number of green pieces in each stack
    sum(GreenPieces, Value).                        % Get the total number of green pieces

% Map values of each move

moves_values([], _, []).

moves_values([C|R], Player, [Value|CR]) :-
    value(C, Player, Value),
    moves_values(R, Player, CR).

% Retrieve stacks controlled by player

get_stacks(ListOfStacks, Player, PlayerStacks) :-   % Puts all the stacks controlled by one player in a list
    findall(Stack, get_player_stack(ListOfStacks, Stack, Player), PlayerStacks).    

get_player_stack(ListOfStacks, Stack, Player) :-
    member(Stack, ListOfStacks),                    % Get stack from list
    nth0(0, Stack, Player).                         % Verify if top piece is of the player's color

% Count the number of green pieces in each stack and map it

green_pieces([], []).

green_pieces([C|R], [TC|CR]) :-
    count(C, g, TC),
    green_pieces(R, CR).

