
main_menu:-
    nl,
    print('================================================'), nl,
    print('Please choose one of the following options: '), nl,
    print('1 - Start'), nl,
    print('2 - Exit'), nl,
    print('================================================'), nl.

process_main_menu_input(2). 
process_main_menu_input(1) :- 
    game_menu,
    read(Input),
    process_game_menu_input(Input),
    Input =:= 5.

game_menu:-
    nl,
    print('================================================'), nl,
    print('Please choose one of the following options: '), nl,
    print('1 - Player vs PC '), nl,
    print('2 - Player vs Player'), nl,
    print('3 - PC vs PC'), nl,
    print('4 - Instructions'), nl,
    print('5 - Go Back'), nl,
    print('================================================'), nl.

process_game_menu_input(5) :- !.
process_game_menu_input(4) :- instructions, !.
process_game_menu_input(N) :- 
    table_menu,
    read(Input),
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
process_table_menu_input(N) :- 
    generate_board, % Generates a board
    initial(GameState), % Unifies GameState
    display_game(GameState, _). % Displays the GameState.

% process_table_menu_input(1) :- .
% process_table_menu_input(2) :- .
% process_table_menu_input(3) :- .

% Prints instructions of game
instructions :-
    nl,
    print('============================================================================================================================================='), nl,
    print('GREENER RULES'), nl,
    print('---------------------------------------------------------------------------------------------------------------------------------------------'), nl,
    print('Players take turns (starting by Black) capturing pyramids or stacks of any colour on the same row or collumn and with no stacks between them.'), nl,
    print('On your turn you must make one capture if possible, otherwise you pass the turn.'), nl,
    print('The game ends when all players pass in succession.'), nl,
    print('The player with the most green  pyramids captured (being part of stacks they control) wins the game.'), nl,
    print('In case of a tie, the player with the highest stack wins. If the tie persists, play again.'), nl,
    print('=============================================================================================================================================='), nl.


