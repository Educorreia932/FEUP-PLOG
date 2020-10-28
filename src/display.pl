% Count the number of elements equal to N inside a list

count([], _C, 0).

count([C|T], C, N) :- 
    count(T, C, N1), 
    N is N1 + 1.

count([X|T], C, N) :- 
    X \= C, 
    count(T, C, N).

character([w], 9651). % △
character([w, _], 9651). % △
character([g], 9709). % ◭
character([g, _], 9709). % ◭
character([b], 9650). % ▲
character([b, _], 9650). % ▲
character([], 32).

color([w], '[1;0m').
color([w, _], '[1;0m').
color([g], '[1;32m').
color([g, _], '[1;32m').
color([b], '[1;90m').
color([b, _], '[1;90m'). 

% Display game board

display_stack(H) :-
    character(H, CharCode),
    CharCode == 32,
    put_code_n(CharCode, 5).

display_stack(H) :-
    character(H, CharCode),
    put_code(32),
    count(H, 'g', GreenPieces), % Count the number of green pieces in the stack
    G is GreenPieces + 48,
    put_code(G),
    color(H, Color), 
    put_code(27),
    print(Color), % Apply color to piece
    put_code(CharCode),
    put_code(27),
    print('[0m'), % Reset color
    length(H, N), 
    N1 is N + 48,
    put_code(N1),
    put_code(32).

display_collumn_id([], _ID).

display_collumn_id([_H|T], ID) :-
    put_code_n(32, 3),
    put_code(ID),
    put_code_n(32, 2),
    ID1 is ID + 1,
    display_collumn_id(T, ID1).

% Prints a code N times
put_code_n(_C, 0).

put_code_n(C, N) :-
    put_code(C),
    N1 is N - 1,
    put_code_n(C, N1).

display_top_middle([_H|[]]).

display_top_middle([_H|T]) :-
    put_code_n(9472, 5), % ─
    put_code(9516), % ┬
    display_top_middle(T).

display_top([H|T]) :-
    put_code(32),
    put_code(32),
    put_code(9484), % ┌
    display_top_middle([H|T]),
    put_code_n(9472, 5), % ─
    put_code(9488). % ┐

display_row_id(ID) :-
    put_code(ID),
    put_code(32).

display_row_middle([]).

display_row_middle([H|T]) :-
    display_stack(H),
    put_code(9474), % │
    display_row_middle(T).

display_row([H|T], ID) :-
    display_row_id(ID),
    put_code(9474), % │
    display_row_middle([H|T]).

display_line_middle([_H|[]]).

display_line_middle([_H|T]) :-
    put_code_n(9472, 5), % ─
    put_code(9532), % ┼
    display_line_middle(T).

display_line([H|T]) :-
    put_code(32),
    put_code(32),
    put_code(9474), % │
    display_line_middle([H|T]),
    put_code_n(9472, 5), % ─
    put_code(9474). % │

display_bottom_middle([_H|[]]).

display_bottom_middle([_H|T]) :-
    put_code_n(9472, 5), % ─
    put_code(9524), % ┴
    display_bottom_middle(T).

display_bottom([H|T]) :-
    put_code(32),
    put_code(32),
    put_code(9492), % └
    display_bottom_middle([H|T]),
    put_code_n(9472, 5), % ─
    put_code(9496). % ┘

display_board_middle([H|[]], _ID) :-
    display_row(H, _ID), nl.

display_board_middle([H|T], ID) :-
    display_row(H, ID), nl,
    display_line(H), nl,
    ID1 is ID + 1,
    display_board_middle(T, ID1).

display_board([H|T]) :-
    put_code(32),
    put_code(32),
    display_collumn_id(H, 65), nl,
    display_top(H), nl,
    display_board_middle([H|T], 49),
    display_bottom(H).

display_game(_GameState, _Player) :-
    display_board(_GameState).