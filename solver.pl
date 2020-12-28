:- use_module(library(clpfd)).
:- use_module(library(lists)).

:- include('display.pl').
:- include('utils.pl').

solve(Blocked, Rows, Columns, Square) :-
    % Domain and variables definition
    length(Rows, Size),                         % Size of square
    Nvars is Size * Size,                       % Number of vars is the square's area
    length(Vars, Nvars),                        % Get list of vars
    domain(Vars, 0, 1),                         % 0 - empty, 1 - full

    % Restrictions  
    blocked_restrictions(Blocked, Vars),
    row_restrictions(Rows, 0, Size, Vars),
    collumn_restrictions(Columns, 0, Size, Vars),

    % Solution search
    labeling([], Vars),
    unflatten(Vars, Size, Square).

blocked_restrictions([], _).
blocked_restrictions([H|T], Vars):-
    nth0(H, Vars, Elem),
    Elem #= 0,
    blocked_restrictions(T, Vars).

row_restrictions([], _, _, _).

row_restrictions([H|T], Index, Size, Square) :-
    get_row(Index, 0, Size, Square, Row),
    sum(Row, #=, H),
    I is Index + 1,
    row_restrictions(T, I, Size, Square).

collumn_restrictions([], _, _, _).

collumn_restrictions([H|T], Index, Size, Square) :-
    get_column(Index, 0, Size, Square, Column),
    sum(Column, #=, H),
    I is Index + 1,
    collumn_restrictions(T, I, Size, Square).

generate_squares(Size, StartsX, StartsY, SquareSizes) :-
    MaxNumSquares is Size * Size,

    NumSquares #> 1,
    NumSquares #< MaxNumSquares,

    length(StartsX, NumSquares),
    length(StartsY, NumSquares),
    length(SquareSizes, NumSquares),

    S is Size - 1,

    domain(StartsX, 0, S),
    domain(StartsY, 0, S),
    domain(SquareSizes, 1, Size),

    construct_squares(Size, StartsX, StartsY, SquareSizes, Squares),
    disjoint2(Squares, [margin(0, 0, 1, 1)]),

    append(StartsX, StartsY, Starts),
    append(Starts, SquareSizes, StartsSizes),
    append(StartsSizes, [NumSquares], Vars),
    labeling([], Vars).

construct_squares(_, [], [], [], []).

construct_squares(Size, [StartX|T1], [StartY|T2], [SquareSize|T3], [square(StartX, SquareSize, StartY, SquareSize)|T4]) :-
    sum([StartX, SquareSize], #=<, Size),
    sum([StartY, SquareSize], #=<, Size),
    construct_squares(Size, T1, T2, T3, T4).
    