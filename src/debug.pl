:- use_module(library(lists)).
initial([
    [[b], [w], [g], [g], [g], [g]],
    [[g], [w], [b,g,g], [g], [b], [w]],
    [[w], [g], [], [g], [w], [g]],
    [[g], [g], [w], [b], [g], [b]], 
    [[g], [g], [w], [b], [g], [w]],
    [[w], [b], [g], [g], [b], [g]]
]).

valid_piece_move_up(_, [_, _, I, _], []) :-
    I =:= 0, !.
    
valid_piece_move_up(GameState, [I0, J0], [I0, J0, I1, J0]) :-
    I0 \== I1,                   % Start cell is different from end cell
    nth1(I1, GameState, Row),    % Select row
    nth1(J0, Row, Stack),        % Select stack from row
    \+ is_empty(Stack), !.       % Stack is not empty

valid_piece_move_up(GameState, [I0, J0, I1, J0], ValidMove) :-   
    I is I1 - 1,                % Go up a collumn
    valid_piece_move_up(GameState, [I0, J0, I, J0], ValidMove).

is_empty(List) :-
    length(List, Length),
    Length =:= 0.

