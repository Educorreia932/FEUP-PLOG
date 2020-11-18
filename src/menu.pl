read_input(Input) :-
    get_code(Character),
    read_input_all(Character, Characters),       
    name(Input, Characters).

read_input_all(10, []).
read_input_all(13, []).

read_input_all(Character, [Character|T]) :-
    get_code(C),
    read_input_all(C, T).

% Start Menu
main_menu:-
    nl,
    print('================================================'), nl,
    print('Please choose one of the following options: '), nl,
    print('1 - Start'), nl,
    print('2 - Exit'), nl,
    print('================================================'), nl.

process_main_menu_input(2). 

process_main_menu_input(1) :- 
    table_menu,
    read_input(Input),
    process_table_menu_input(Input),
    Input =:= 4.

table_menu:-
    nl,
    print('================================================'), nl,
    print('Please select the size of the board: '), nl,
    print('1 - 6x6'), nl,
    print('2 - 6x9'), nl,
    print('3 - 9x9'), nl,
    print('4 - Go Back'), nl,
    print('================================================'), nl.

process_table_menu_input(4) :- !.

process_table_menu_input(1) :- 
    generate_board(6, 6, GameState),       % Generates a board
    choose_game(GameState).                % Unifies GameState

process_table_menu_input(2) :- 
    generate_board(6, 9, GameState),       % Generates a board
    choose_game(GameState).

process_table_menu_input(3) :- 
    generate_board(9, 9, GameState),       % Generates a board
    choose_game(GameState).                 

% Game Menu

choose_game(GameState) :-
    game_menu,
    read_input(Input),
    process_game_menu_input(Input, GameState).

game_menu:-
    nl,
    print('================================================'), nl,
    print('Please choose one of the following options: '), nl,
    print('1 - Player vs Player '), nl,
    print('2 - Player vs PC'), nl,
    print('3 - PC vs PC'), nl,
    print('4 - Instructions'), nl,
    print('5 - Go Back'), nl,
    print('================================================'), nl.

process_game_menu_input(1, GameState) :- game(player, player, GameState).
process_game_menu_input(4, GameState) :- instructions.
% process_game_menu_input(5, GameState).

% Prints instructions of game
instructions :-
    nl,
    print('============================================================================================================================================='), nl,
    print('GREENER RULES'), nl,
    print('---------------------------------------------------------------------------------------------------------------------------------------------'), nl,
    print('Players take turns (starting by Black) capturing pyramids or stacks of any colour on the same row or collumn and with no stacks between them.'), nl,
    print('On your turn you must make one capture if possiwble, otherwise you pass the turn.'), nl,
    print('The game ends when all players pass in succession.'), nl,
    print('The player with the most green  pyramids captured (being part of stacks they control) wins the game.'), nl,
    print('In case of a tie, the player with the highest stack wins. If the tie persists, play again.'), nl,
    print('=============================================================================================================================================='), nl.

% Choose AI Strategy

strategy_menu :-
    nl,
    print('============================================================================================================================================='), nl,
    print('Please choose a strategy for the AI :'), nl,
    print('1 - Random'), nl,
    print('2 - Smart'), nl,
    print('============================================================================================================================================='), nl.

choose_strategy :-
    strategy_menu,
    read_input(Input),
    process_strategy_input(Input).

