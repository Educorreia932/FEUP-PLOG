:- use_module(library(clpfd)).
:- use_module(library(lists)).

square(Rows, Columns, Vars) :-
    % Domain and variables definition

    length(Rows, Size),                         % Size of square
    Nvars is Size * Size,                       % Number of vars is the square's area
    length(Vars, Nvars),                        % Get list of vars
    domain(Vars, 0, 1),                         % 0 - empty, 1 - full
    
    % Restrictions  
    rowRestrictions(Rows, 0, Size, Vars),
    collumnRestrictions(Columns, 0, Size, Vars),

    % Solution search
    labeling([], Vars).

rowRestrictions([], _, _, _).

rowRestrictions([H|T], Index, Size, Square) :-
    getRow(Index, 0, Size, Square, Row),
    sum(Row, #=, H),
    I is Index + 1,
    rowRestrictions(T, I, Size, Square).

collumnRestrictions([], _, _, _).

collumnRestrictions([H|T], Index, Size, Square) :-
    getColumn(Index, 0, Size, Square, Column),
    sum(Column, #=, H),
    I is Index + 1,
    collumnRestrictions(T, I, Size, Square).

getRow(Index, N, Size, Square, Row) :-
    N >= Index * Size,
    prefix_length(Square, Row, Size). 

getRow(Index, N, Size, [_|T], Row) :-
    N < Index * Size,
    N1 is N + 1,
    getRow(Index, N1, Size, T, Row). 

getColumn(_, N, Size, _, []) :-
    Aux is Size ** 2,
    N >= Aux.

getColumn(Index, N, Size, Square, [H|T]) :-
    I is Index + N,  
    N1 is N + Size,
    nth0(I, Square, H),
    getColumn(Index, N1, Size, Square, T).
        
build(_, 0, []).        % End of recursion

build(X, N, [X|T]) :-   % Adds N pieces to list
    N1 is N - 1,
    build(X, N1, T).    % Recursion
