:- use_module(library(system)).

% Checks if two pair of coordinates correspond to the same cell

is_same_cell(I0, J0, I1, J1) :-
    I0 =:= I1,
    J0 =:= J1.

% Returns the stack present int the cell with coordinates I, J from the board

get_cell(Board, I, J, Stack) :-
    nth0(I, Board, Row),        % Select row
    nth0(J, Row, Stack).        % Select stack from row

% Converts a column's letter to an index

column_index(ColumnIn, ColumnOut) :-
    atom_length(ColumnIn, Length),
    Length =:= 1,
    char_code(ColumnIn, Code),
    ColumnOut is Code - 65.

% List of all pieces

pieces(W, G, B, Pieces) :-
    build(w, W, L1),        % Creates list with W white pieces
    build(g, G, L2),        % Creates list with G green pieces
    build(b, B, L3),        % Creates list with B black pieces
    append(L1, L2, L),      % Appends lists
    append(L, L3, Pieces).  % Appends lists

% Returns a shuffled list from a list of pieces

shuffle_board(Shuffled, Collumns, Rows) :-
    Size = Collumns * Rows,                 % Calculates size of board
    W is div(Size, 4),                      % Calculates number of white pieces
    B = W,                                  % Number of black pieces is the same as white pieces
    G = Size - (W + B),                     % Calculates number of green pieces
    pieces(W, G, B, Pieces),                % Creates list with all pieces
    random_permutation(Pieces, Shuffled).   % Shuffles list of pieces.

% Fills a row with pieces

fill_row(Pieces, Collumns, FilledRow) :-
    take(Pieces, Collumns, Row),
    create(Row, FilledRow).

% Fills board row by row using a list of pieces

fill_board(_, _, 0, _).

fill_board(Pieces, Collumns, Rows, Board) :-
    fill_row(Pieces, Collumns, FilledRow),
    remove_n(Pieces, Collumns, P),              
    append(FilledBoard, [FilledRow], Board),
    R is Rows - 1,
    fill_board(P, Collumns, R, FilledBoard).

% Generates random game board, filling it with pieces

generate_board(Collumns, Rows, Board) :-

    shuffle_board(Shuffled, Collumns, Rows),            % Shuffles all pieces from the list
    fill_board(Shuffled, Collumns, Rows, Board).       % Fills board with the pieces from list
