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