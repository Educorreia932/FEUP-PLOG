:- use_module(library(random)).

% Initial Configuration of Board

initial([
    [[w], [g], [g], [w], [g], [b]],
    [[b], [g], [g], [g], [w], [w]],
    [[g], [w], [w], [g], [b], [b]],
    [[g], [b], [b], [g], [g], [g]],
    [[b], [g], [g], [w], [b], [g]],
    [[g], [w], [b], [g], [w], [g]]
]).

% Generate game board, filling it with pieces

generate_piece(Piece) :-
    random(0, 3, PieceNumber),
    piece(PieceNumber, Piece).

generate_row([]).

generate_row([_H|T]) :-
    generate_piece(Piece),
    assert(row([[Piece, 0]|T])),
    generate_row(T),
    write(row(_X)). 

generate_board :- 
    board(X),
    generate_row(X).

    % retract
    % assert