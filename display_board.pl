% play().
% initial(-GameState).
% valid_moves(+GameState, +Player, -ListOfMoves).
% move(+GameState, +Move, -NewGameState).
% game_over(+GameState, -Winner).
% value(+GameState, +Player, -Value).
% choose_move(+GameState, +Player, +Level, -Move).

:- use_module(library(random)).

% Initial Configuration of Board
:- dynamic(board/1).

board([
    [[w, 1], [w , 1], [], [], [], []],
    [[], [b, 3], [], [], [b, 1], []],
    [[], [], [], [b, 4], [], []],
    [[b, 2], [g, 3], [], [b, 1], [], []],
    [[], [w, 1], [], [], [], []],
    [[], [], [], [g, 2], [], []]
]).

% Display game board

display_piece(H) :-
    piece(H, CharCode),
    CharCode == 32,
    put_code(CharCode),
    put_code(CharCode),
    put_code(CharCode).

display_piece(H) :-
    piece(H, CharCode),
    put_code(CharCode),
    put_code(32).

display_top_middle([H|[]]).

display_top_middle([H|T]) :-
    put_code(9472), % ─
    put_code(9472), % ─
    put_code(9472), % ─
    put_code(9516), % ┬
    display_top_middle(T).

display_top([H|T]) :-
    put_code(9484), % ┌
    display_top_middle([H|T]),
    put_code(9472), % ─
    put_code(9472), % ─
    put_code(9472), % ─
    put_code(9488). % ┐

display_row_middle([]).

display_row_middle([H|T]) :-
    display_piece(H),
    put_code(9474), % │
    display_row_middle(T).

display_row([H|T]) :-
    put_code(9474), % │
    display_row_middle([H|T]).

display_line_middle([H|[]]).

display_line_middle([H|T]) :-
    put_code(9472), % ─
    put_code(9472), % ─
    put_code(9472), % ─
    put_code(9532), % ┼
    display_line_middle(T).

display_line([H|T]) :-
    put_code(9474), % │
    display_line_middle([H|T]),
    put_code(9472), % ─
    put_code(9472), % ─
    put_code(9472), % ─
    put_code(9474). % │

display_bottom_middle([H|[]]).

display_bottom_middle([H|T]) :-
    put_code(9472), % ─
    put_code(9472), % ─
    put_code(9472), % ─
    put_code(9524), % ┴
    display_bottom_middle(T).

display_bottom([H|T]) :-
    put_code(9492), % └
    display_bottom_middle([H|T]),
    put_code(9472), % ─
    put_code(9472), % ─
    put_code(9472), % ─
    put_code(9496). % ┘

display_board_middle([H|[]]) :-
    display_row(H), nl.

display_board_middle([H|T]) :-
    display_row(H), nl,
    display_line(H), nl,
    display_board_middle(T).

display_board([H|T]) :-
    display_top(H), nl,
    display_board_middle([H|T]),
    display_bottom(H).

display_board :-
    board(X),
    display_board(X).

% Generate game board, filling it with pieces

piece([w,_], 9898). % ⚪
piece([g,_], 128994). % 🟢
piece([b,_], 9899). % ⚫
piece([], 32).

generate_piece(Piece) :-
    random(0, 3, PieceNumber),
    piece(0, Piece).

generate_row([]).

generate_row([H|T]) :-
    generate_piece(Piece),
    assert(row([[Piece, 0]|T])),
    generate_row(T),
    write(row(X)). 

generate_board :- 
    board(X),
    generate_row(X).

    % retract
    % assert
