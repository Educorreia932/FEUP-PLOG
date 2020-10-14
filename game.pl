% play().
% initial(-GameState).
% valid_moves(+GameState, +Player, -ListOfMoves).
% move(+GameState, +Move, -NewGameState).
% game_over(+GameState, -Winner).
% value(+GameState, +Player, -Value).
% choose_move(+GameState, +Player, +Level, -Move).




% Initial Configuration of Board
board([
    [[], [], [], [], [], []],
    [[], [], [], [], [], []],
    [[], [], [], [], [], []],
    [[], [], [], [], [], []],
    [[], [], [], [], [], []],
    [[], [], [], [], [], []]
]).

% Display game board

display_row([]).
display_row([H|T]) :-
    stack(H, Stack),
    write(Stack),
    display_row(T).

display_board([]).
display_board([H|T]) :-
    display_row(H), nl,
    display_board(T).

display_board :-
    board(X),
    display_board(X).

% Generate game board, filling it with pieces

piece(0, 'B').
piece(1, 'G').
piece(2, 'W').

generate_piece(Piece) :-
    % random(0, 3, PieceNumber),
    piece(0, Piece).

generate_row([]).

generate_row([H|T]) :-
    generate_piece(Piece),
    assert(row([H|[Piece, 0]|T])),
    generate_row(T),
    write(row(X)).

generate_board :- 
    board(X),
    generate_row(X).

    % retract
    % assert





% Convert stack type to output

stack([], '..'). % Empty

stack(List, Piece) :-
    atom_chars(Piece, List).

display_game :-
    write('Board').
