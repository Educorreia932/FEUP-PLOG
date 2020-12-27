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
    % por cada row aplica restrictions

    % Column Restictions
    % por cada coluna aplica restrictions
    columnRestrictions(NRows, NColumns, Square),

    % Solution search
    labeling([], List).

rowRestrictions(0, _, _).

rowRestrictions(NRows, NColumns, Square) :-
    N is NRows - 1,
    rowRestrictions(N, NColumns, Square).

columnRestrictions(0, _, _).

columnRestrictions(NRows, NColumns, Square) :-
    N is NColumns - 1,
    columnRestrictions(N, NColumns, Square).

getColumn(_, N, Size, _, []) :-
    Aux is Size ** 2,
    N >= Aux.

getColumn(Index, N, Size, Square, [H|T]) :-
    I is Index + N,  
    N1 is N + Size,
    nth0(I, Square, H),
    print(H),      
    getColumn(Index, N1, Size, Square, T).
        