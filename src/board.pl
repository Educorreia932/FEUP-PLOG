:- consult('utils.pl').

:- use_module(library(random)).
:- use_module(library(system)).
:- use_module(library(lists)).

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
    append(L1, L2, L),
    append(L, L3, Pieces).

% Returns a shuffled list from a list of pieces

shuffle_board(Shuffled) :-
    pieces(9, 18, 9, Pieces),
    random_permutation(Pieces, Shuffled).

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

generate_board :-
    now(T),                             % Seed for RNG
    setrand(T),                         % For randomness 
    shuffle_board(Shuffled),            % Shuffles all pieces from the list
    fill_board(Shuffled, 6, 6, Board),  % Fills board with the pieces from list
    assert(initial(Board)).
