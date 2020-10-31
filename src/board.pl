:- use_module(library(random)).
:- use_module(library(system)).

% Initial Configuration of Board

:- dynamic(initial/1).

% List of all 18 pieces: 9white, 9 black & 18 green
pieces(['w', 'w', 'w', 'w', 'w', 'w', 'w', 'w', 'w',
        'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g',
        'b', 'b', 'b', 'b', 'b', 'b', 'b', 'b', 'b']).


% Fills board row by row using a list of pieces
fill_board([], _).

fill_board([H0, H1, H2, H3, H4, H5|T], [[[H0], [H1], [H2], [H3], [H4], [H5]]|T1]) :-
    fill_board(T, T1).

% Returns a shuffled list from a list of pieces
shuffle_board(Shuffled) :-
    pieces(P),
    random_permutation(P, Shuffled).

% Generates random game board, filling it with pieces from list
generate_board :-
    now(T),
    setrand(T), % For randomness 
    shuffle_board(Shuffled), % Shuffles all pieces from the list
    fill_board(Shuffled, Board), % FIlls board with the pieces from list
    assert(initial(Board)).