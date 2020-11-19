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
    print('|                   GREENER                    |'), nl,
    print('================================================'), nl,
    print('                                                '), nl,
    print('                  1 - Start                     '), nl,
    print('                  2 - How to play               '), nl,
    print('                  3 - Exit                      '), nl,
    print('                                                '), nl,
    print('================================================'), nl.

process_main_menu_input(1) :- 
    table_menu,
    read_input(Input),
    process_table_menu_input(Input),
    Input =:= 4, !.

process_main_menu_input(2) :- instructions, !. 
process_main_menu_input(3), !. 

table_menu:-
    nl,
    print('================================================'), nl,
    print('|                    BOARD                     |'), nl,
    print('================================================'), nl,
    print('                                                '), nl,
    print('                   1 - 6x6                      '), nl,
    print('                   2 - 6x9                      '), nl,
    print('                   3 - 9x9                      '), nl,
    print('                   4 - Back                     '), nl,
    print('                                                '), nl,
    print('================================================'), nl.

process_table_menu_input(4) :- !.

process_table_menu_input(1) :- 
    generate_board(6, 6, GameState),       % Generates a board
    choose_game(GameState), !.                % Unifies GameState

process_table_menu_input(2) :- 
    generate_board(6, 9, GameState),       % Generates a board
    choose_game(GameState), !.

process_table_menu_input(3) :- 
    generate_board(9, 9, GameState),       % Generates a board
    choose_game(GameState), !.                 

% Game Menu

choose_game(GameState) :-
    game_menu,
    read_input(Input),
    process_game_menu_input(Input, GameState).

game_menu:-
    nl,
    print('================================================'), nl,
    print('|                    GAME                      |'), nl,
    print('================================================'), nl,
    print('                                                '), nl,
    print('                1 - Player vs Player            '), nl,
    print('                2 - Player vs AI                '), nl,
    print('                3 - AI vs AI                    '), nl,
    print('                4 - Back                        '), nl,
    print('                                                '), nl,
    print('================================================'), nl.

process_game_menu_input(1, GameState) :- 
    game_loop(b, GameState, 1, 0, 0), !.

process_game_menu_input(3, GameState) :- 
    game_loop(b, GameState, 2, 0, 0), !.

% Ask play to choose a cell

choose_cell_input(Row, Column) :-
    read_input(J),
    read_input(I),
    column_index(J, Column),
    Row is I - 1.

% Ask player for its turn's move

choose_move_input(Player, GameState, NewGameState) :-
    nl,
    print('What\'s your move?'), nl,    
    choose_cell_input(I0, J0),
    choose_cell_input(I1, J1),
    process_choose_move_input(Player, GameState, [I0, J0, I1, J1], NewGameState),
    nl.

process_choose_move_input(Player, GameState, [I0, J0, I1, J1], NewGameState) :- 
    move(GameState, [I0, J0, I1, J1], NewGameState),
    valid_move(Player, GameState, NewGameState).

% Prints instructions of game

instructions :-
    nl,
    print('================================================================'), nl,
    print('|                           HOW TO PLAY                        |'), nl,
    print('================================================================'), nl,
    print('                                                                '), nl,
    print('   Players take turns (starting by Black) capturing pyramids or '), nl, 
    print('  stacks of any colour on the same row or column and with no    '), nl,
    print('  stacks between them.                                          '), nl,
    print('                                                                '), nl,
    print('   On your turn you must make one capture if possible, otherwise'), nl,
    print('   you pass the turn.                                           '), nl,
    print('                                                                '), nl,
    print('   The game ends when all players pass in succession.           '), nl,
    print('                                                                '), nl,
    print('   The player with the most green pyramids captured (being      '), nl,
    print('  part of stacks they control) wins the game.                   '), nl,
    print('   In case of a tie, the player with the highest stack wins.    '), nl,
    print('   If the tie persists, play again.                             '), nl,
    print('                                                                '), nl,
    print('   Good luck and have fun!                                      '), nl,
    print('                                                                '), nl,
    print('================================================================'), nl.

% Choose AI Strategy

strategy_menu :-
    nl,
    print('================================================'), nl,
    print('|                  AI STRATEGY                 |'), nl,
    print('================================================'), nl,
    print('                                                '), nl,
    print('                  1 - Random                    '), nl,
    print('                  2 - Smart                     '), nl,
    print('                                                '), nl,
    print('================================================'), nl.

choose_strategy :-
    strategy_menu,
    read_input(Input),
    process_strategy_input(Input).

wait_enter :-
	write('Press <Any Key> to continue.\n'),
	get_char(_), !.