:- use_module(library(random)).
:- use_module(library(system)).

% Initial Configuration of Board

:- dynamic(initial/1).
:- dynamic(remaining_pieces/3).

piece(0, 'w').
piece(1, 'g').
piece(2, 'b').

pieces(['w', 'w', 'w', 'w', 'w', 'w', 'w', 'w', 'w',
        'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g',
        'b', 'b', 'b', 'b', 'b', 'b', 'b', 'b', 'b']).

% Generate game board, filling it with pieces

fill_board([], _).

fill_board([H0, H1, H2, H3, H4, H5|T], [[[H0], [H1], [H2], [H3], [H4], [H5]]|T1]) :-
    fill_board(T, T1).

shuffle_board(Shuffled) :-
    pieces(P),
    random_permutation(P, Shuffled).

generate_board :-
    now(T),
    setrand(T),
    shuffle_board(Shuffled),
    fill_board(Shuffled, Board),
    assert(initial(Board)).