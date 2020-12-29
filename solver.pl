:- use_module(library(clpfd)).

:- include('display.pl').
:- include('utils.pl').

solve(Blocked, Rows, Columns, Square) :-
    % Domain and variables definition
    length(Rows, Size),                          % Size of square
    Nvars is Size * Size,                        % Number of Cells is the square's area
    length(Cells, Nvars),                        % Get list of Cells
    domain(Cells, 0, 1),                         % 0 - empty, 1 - full

    % Restrictions  
    % blocked_restrictions(Blocked, Cells),               % Restrict cells marked with X
    % column_restrictions(Columns, 0, Size, Cells),       % Restrict Columns
    % row_restrictions(Rows, 0, Size, Cells),             % Restrict Rows

    generate_squares(Size, StartsX, StartsY, SquareSizes, NumSquares),     % Generates non-overlaping & non-touching squares 
    squares_restrictions(StartsX, StartsY, SquareSizes, Size, Cells),

    % Solution search
    VarsList = [Cells, StartsX, StartsY, SquareSizes, NumSquares],
    flatten(VarsList, Vars),
    labeling([], Vars),

    print(StartsX), nl,
    print(StartsY), nl,
    print(SquareSizes), nl,

    unflatten(Cells, Size, Square).

% Restrict cells marked with X

blocked_restrictions([], _).                % Stop recursion

blocked_restrictions([Index|T], Cells):-
    element(Index, Cells, 0),               % cant paint X cells  
    blocked_restrictions(T, Cells).         % recursion

% Restrict number of filled cells in each row

row_restrictions([], _, _, _).                  % Stop recursion

row_restrictions([H|T], Index, Size, Square) :-
    get_row(Index, 0, Size, Square, Row),       % Get Row
    sum(Row, #=, H),                            % Constraint sum of row
    I is Index + 1,                             % Increment Line index
    row_restrictions(T, I, Size, Square).       % Recursion

% Restrict number of filled cells in each column

column_restrictions([], _, _, _).                   % Stop recursion

column_restrictions([H|T], Index, Size, Square) :-
    get_column(Index, 0, Size, Square, Column),     % Get column
    sum(Column, #=, H),                             % Constraint sum of column
    I is Index + 1,                                 % Increment Column Index
    column_restrictions(T, I, Size, Square).        % Recursion

% Restrict all filled cells to be composed of squares

% Restrictions for all squares

squares_restrictions([], [], [], _, _).

squares_restrictions([StartX|T1], [StartY|T2], [SquareSize|T3], Size, Cells) :-
    square_restrictions(StartX, StartY, SquareSize, Size, Cells, SquareSize),
    squares_restrictions(T1, T2, T3, Size, Cells).

% Restrictions for a single square

square_restrictions(_, _, _, _, _, 0).

square_restrictions(X, Y, SquareSize, Size, Cells, N) :-
    square_line_restrictions(X, Y, SquareSize, Size, Cells),
    Y1 is Y + 1,
    N1 is N - 1,
    square_restrictions(X, Y1, SquareSize, Size, Cells, N1).

% Restrictions for a single square line

square_line_restrictions(ColumnIndex, RowIndex, SquareSize, Size, Cells) :-
    get_row(RowIndex, 0, Size, Cells, Row),
    trim(Row, ColumnIndex, Line),
    prefix_length(Line, SquareLine, SquareSize),
    sum(SquareLine, #=, SquareSize).
    