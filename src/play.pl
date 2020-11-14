play :-
    generate_board, % Generates a board
    initial(GameState), % Unifies GameState
    display_game(GameState, _). % Displays the GameState
/*
play :-
    repeat,
        main_menu,
        read(Input),
        process_main_menu_input(Input),
        Input =:= 2.
*/