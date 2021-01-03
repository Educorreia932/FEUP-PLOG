:- use_module(library(clpfd)).

:- include('utils.pl').

fd_length(L, N) :-
   N #>= 0,
   fd_length(L, N, 0).

fd_length([], N, N0) :-
   N #= N0.

fd_length([_|L], N, N0) :-
   N1 is N0+1,
   N #>= N1,
   fd_length(L, N, N1).

solve(Blocked, Rows, Columns, Vars) :-
    % Domain and variables definition

    length(Rows, Size),   

    NumSquares #> 11,

    fd_length(StartsX, NumSquares),
    fd_length(StartsY, NumSquares),                   
    fd_length(SquareSizes, NumSquares),              
    
    S is Size - 1,           
                           
    domain(StartsX, 0, S),                         
    domain(StartsY, 0, S),                          
    domain(SquareSizes, 1, Size),    

    (
        foreach(X, StartsX), 
        foreach(Y, StartsY), 
        foreach([X, Y], StartsXY) 
    do 
        true
    ), lex_chain(StartsXY),              

    construct_squares(Size, StartsX, StartsY, SquareSizes, Squares), 

    % Constraints

    disjoint2(Squares, [margin(0, 0, 1, 1)]),
    
    lines_constraints(0, Rows, StartsX, SquareSizes),
    lines_constraints(0, Columns, StartsY, SquareSizes),

    % Solution search

    VarsList = [NumSquares, StartsX, StartsY, SquareSizes],
    flatten(VarsList, Vars),
    labeling([], Vars).

construct_squares(_, [], [], [], []). 

construct_squares(Size, [StartX|T1], [StartY|T2], [SquareSize|T3], [square(StartX, SquareSize, StartY, SquareSize)|T4]) :-
    StartX + SquareSize #=< Size,              
    StartY + SquareSize #=< Size,
    construct_squares(Size, T1, T2, T3, T4).  

% Rows and columns NumFilledCells cells constraints

lines_constraints(_, [], _, _).

lines_constraints(Index, [NumFilledCells|T], Starts, SquareSizes) :-
    line_constraints(Index, NumFilledCells, Starts, SquareSizes),
    I is Index + 1,
    lines_constraints(I, T, Starts, SquareSizes).

line_constraints(Index, NumFilledCells, Starts, SquareSizes) :-
    (
        foreach(Start, Starts),
        foreach(SquareSize, SquareSizes),
        foreach(Usage, Usages),
        param(Index)
    do
        Intersect #<=> (Start #=< Index #/\ Index #< Start + SquareSize),
        Usage #= Intersect * SquareSize
    ),
    sum(Usages, #=, NumFilledCells).
    
% Check if a square intersects a row or column

intersect(Index, Start, SquareSize) :-
    Start #=< Index,
    Index #=< Start + SquareSize.
