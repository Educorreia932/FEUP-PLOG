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

start_game('Player VS Player', Rows, Columns) :-                  % Starts PvP game
    generate_board(Rows, Columns, GameState),                     % Generates board
    game_loop('Player VS Player', b, GameState, 0, 0).            % Starts game with black playing first

start_game('Player VS AI', Strat, Rows, Columns) :-               % Starts Player vs AI game
    generate_board(Rows, Columns, GameState),                     % Generates board
    print('Not yet implemented'), nl.                             % Starts game with white playing first

start_game('AI VS AI', Strat1, Strat2, Rows, Columns) :-          % Starts AI vs AI game
    generate_board(Rows, Columns, GameState),                     % Generates board
    game_loop('AI VS AI', b, [Strat1, Strat2], GameState, 0, 0).  % Starts game with white playing first

% Game Over

game_over(GameState, Winner) :-
    value(GameState, b, BlackValue),                % Calculates value for black 
    value(GameState, w, WhiteValue),                % Calculates value for white
    winner(BlackValue, WhiteValue, Winner).         % Sets winner

winner(BlackValue, WhiteValue, 'Black') :- BlackValue > WhiteValue.
winner(BlackValue, WhiteValue, 'White') :- BlackValue < WhiteValue.
winner(BlackValue, WhiteValue, 'Draw') :- BlackValue =:= WhiteValue.

game_loop(_, _, _, GameState, 1, 1) :-
    game_over(GameState, Winner),
    (Winner = 'Draw' -> format('There\'s no winner', Winner); 
    format('The winner is ~w', Winner)), !.

% Player VS Player

game_loop('Player VS Player', Player, [], GameState, _, _) :-
    display_game(GameState, Player),
    move_input(Player, GameState, NewGameState),
    (GameState == NewGameState -> 
        (Player == b -> BlackFinished is 1;
         Player == w -> WhiteFinished is 1);
     BlackFinished is 0, WhiteFinished is 0
    ),
    clear_screen,
    next_player(Player, NextPlayer),
    game_loop('Player VS Player', NextPlayer, [], NewGameState, BlackFinished, WhiteFinished).

% Player VS AI
    
game_loop('Player VS AI', Player, [Strat], GameState, _, _).

% AI vs AI

game_loop('AI VS AI', Player, [Strat1, Start2], GameState, _, _) :-
    display_game(GameState, Player),
    choose_move(GameState, Player, NewGameState, smartAI),
    (GameState == NewGameState -> 
        (Player == b -> BlackFinished is 1;
         Player == w -> WhiteFinished is 1);
     BlackFinished is 0, WhiteFinished is 0
    ),
    sleep(0.5),
    clear_screen,
    next_player(Player, NextPlayer),
    game_loop('AI VS AI', NextPlayer, [Strat1, Strat2], NewGameState, BlackFinished, WhiteFinished).

% Choose move

choose_move(GameState, Player, GameState, _) :-
    valid_moves(GameState, Player, []).             % There are no valid moves

choose_move(GameState, Player, Move, randomAI) :-
    valid_moves(GameState, Player, ListOfMoves),    % Calculates valid moves
    length(ListOfMoves, NumberOfMoves),             % Gets number of valid moves
    random(0, NumberOfMoves, R),                    % Choose a random number
    nth0(R, ListOfMoves, Move).                     % Choose a random move

choose_move(GameState, Player, Move, smartAI) :-
    valid_moves(GameState, Player, ListOfMoves),    % Calculates valid moves
    moves_values(ListOfMoves, Player, MovesValues),  % Calculate value for each move
    max_list(MovesValues, _, Index),                % Get the highest value move
    nth0(Index, ListOfMoves, Move).                 % Choose the highest value move

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
