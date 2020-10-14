% play().
% initial(-GameState).
% valid_moves(+GameState, +Player, -ListOfMoves).
% move(+GameState, +Move, -NewGameState).
% game_over(+GameState, -Winner).
% value(+GameState, +Player, -Value).
% choose_move(+GameState, +Player, +Level, -Move).

% Initial Configuration of Board
board([
    [[w, 5], [], [], [], [], []],
    [[], [], [], [], [], []],
    [[], [], [], [], [], []],
    [[], [], [], [], [], []],
    [[], [], [], [], [], []],
    [[], [], [], [], [], []]
]).

display_row([]).
display_row([A|B]) :-
    piece(A, Piece),
    write(Piece),
    display_row(B).

display_board([]).
display_board([H|T]) :-
    display_row(H), nl,
    display_board(T).

display_board :-
    board(X),
    display_board(X).

generate_board.

% Convert piece type to output
piece([], '.'). % Empty
piece([b|T], 'B'). % Black
piece([g|T], 'G'). % Green
piece([w|T], 'W'). % White    

display_game :-
    write('Board').
