:- use_module(library(clpfd)).
:- use_module(library(lists)).

:- include('display.pl').

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

trim(L, N, S) :-       % Trim N elements from a list
  length(P, N),        % Generate an unbound prefix list of the desired length
  append(P, S, L).     % Get the desired suffix.

unflatten([], _, []).

unflatten(Vars, Size, [H|T]) :-
    prefix_length(Vars, H, Size),
    trim(Vars, Size, VarsOut), 
    unflatten(VarsOut, Size, T).

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

get_row(Index, N, Size, Square, Row) :-
    N >= Index * Size,
    prefix_length(Square, Row, Size). 

get_row(Index, N, Size, [_|T], Row) :-
    N < Index * Size,
    N1 is N + 1,
    get_row(Index, N1, Size, T, Row). 

get_column(_, N, Size, _, []) :-
    Aux is Size ** 2,
    N >= Aux.

get_column(Index, N, Size, Square, [H|T]) :-
    I is Index + N,  
    N1 is N + Size,
    nth0(I, Square, H),
    get_column(Index, N1, Size, Square, T).
        
build(_, 0, []).        % End of recursion

build(X, N, [X|T]) :-   % Adds N pieces to list
    N1 is N - 1,
    build(X, N1, T).    % Recursion

square_restrictions(Size, StartsX, StartsY, SquareSizes) :-
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

    generate_squares(Size, StartsX, StartsY, SquareSizes, Squares),
    disjoint2(Squares, [margin(0, 0, 1, 1)]),

    append(StartsX, StartsY, Starts),
    append(Starts, SquareSizes, StartsSizes),
    append(StartsSizes, [NumSquares], Vars),
    labeling([], Vars).

generate_squares(_, [], [], [], []).

generate_squares(Size, [StartX|T1], [StartY|T2], [SquareSize|T3], [square(StartX, SquareSize, StartY, SquareSize)|T4]) :-
    sum([StartX, SquareSize], #=<, Size),
    sum([StartY, SquareSize], #=<, Size),
    generate_squares(Size, T1, T2, T3, T4).
    