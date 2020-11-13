:- use_module(library(random)).
:- use_module(library(system)).

% Initial Configuration of Board

:- dynamic(initial/1).

build(_, 0, _).

build(X, N, [X|T]) :- 
    N1 is N - 1,
    build(X, N1, T).

% List of all pieces
pieces(W, G, B, Pieces) :-
    build(w, W, L1),
    build(g, G, L2),
    build(b, B, L3),
    append(L1, L2, L12),
    append(L12, L3, Pieces).

% Fills board row by row using a list of pieces
fill_board([], _).

fill_board([H0, H1, H2, H3, H4, H5|T], [[[H0], [H1], [H2], [H3], [H4], [H5]]|T1]) :-
    fill_board(T, T1).

% Returns a shuffled list from a list of pieces
shuffle_board(Shuffled) :-
    pieces(9, 18, 9, Pieces),
    random_permutation(Pieces, Shuffled).

% Generates random game board, filling it with pieces from list
generate_board :-
    now(T), % Seed for RNG
    setrand(T), % For randomness 
    shuffle_board(Shuffled), % Shuffles all pieces from the list
    fill_board(Shuffled, Board), % Fills board with the pieces from list
    assert(initial(Board)).
