:- use_module(library(clpfd)).
:- use_module(library(lists)).

:- include('utils.pl').

square(Rows, Columns, Solution) :-
    % Domain and variables definition

    length(Rows, NRows),                        % Number of rows
    length(Columns, NColumns),                  % Number of columns
    Nvars is NRows * NColumns,                  % Number of vars is the square's area
    length(Vars, Nvars),                        % Get list of vars
    domain(Vars, 0, 1),                         % 0 - empty, 1 - full
    
    % Restrictions  

    % Rows Restictions
    rowRestrictions([H|T], Index, Size, Square),
    % Column Restictions
    collumnRestrictions([H|T], Index, Size, Square),

    % Solution search
    labeling([], List).

rowRestrictions([], _, _, _).

rowRestrictions([H|T], Index, Size, Square) :-
    getRow(Index, 0, Size, Square, Row),
    sumlist(Row, Filled),
    Filled #= H,
    I is Index + 1,
    rowRestrictions(T, I, Size, Square).

collumnRestrictions([], _, _, _).

collumnRestrictions([H|T], Index, Size, Square) :-
    getColumn(Index, 0, Size, Square, Column),
    sumlist(Column, Filled),
    Filled #= H,
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
        