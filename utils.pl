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


trim(L, N, S) :-       % Trim N elements from a list
length(P, N),        % Generate an unbound prefix list of the desired length
append(P, S, L).     % Get the desired suffix.

unflatten([], _, []).

unflatten(Vars, Size, [H|T]) :-
    prefix_length(Vars, H, Size),
    trim(Vars, Size, VarsOut), 
    unflatten(VarsOut, Size, T).

% Get the indexes that correspond to all squares

squares_indexes([], [], [], [], _).

squares_indexes([StartX|T1], [StartY|T2], [SquareSize|T3], [Indexes|T4], Size) :-
    EndY is StartY + SquareSize,
    square_indexes(StartX, StartY, EndY, SquareSize, Indexes, Size),
    squares_indexes(T1, T2, T3, T4, Size).

% Get the indexes that correspond to a single square

square_indexes(_, Y, Y, _, [], _).

square_indexes(StartX, StartY, EndY, SquareSize, [Indexes|T], Size) :-
    LowerBound is StartY * Size + StartX, 
    HigherBound is LowerBound + SquareSize,
    findall(Index, between(LowerBound, HigherBound, Index), Indexes),
    Y is StartY + 1,
    square_indexes(StartX, Y, EndY, SquareSize, T, Size).
