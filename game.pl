% TODO:
display_game(+GameState, +Player).

play() :-
    initial(GameState),
    display_game(GameState, _).

initial(-GameState). 