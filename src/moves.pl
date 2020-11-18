:- consult('utils.pl').

% Move piece from a cell to other

move(BoardIn, [I0, J0], [I1, J1], BoardOut) :-
    nth0(I0, BoardIn, RowStart),             % Select starting row
    nth0(J0, RowStart, StackStart),          % Select stack from row

    replace(RowStart, J0, [], RowAux),       % Remove stack from starting cell
    replace(BoardIn, I0, RowAux, BoardAux),          

    nth0(I1, BoardAux, RowEnd),              % Select final row
    nth0(J1, RowEnd, StackEnd),              % Select stack from row
    append(StackStart, StackEnd, Stack),     % Add the piece to the top of the stack
    
    replace(RowEnd, J1, Stack, Row),         % Move stack to final cell
    replace(BoardAux, I1, Row, BoardOut).

% All possible and valid moves

valid_moves(BoardIn, Player, ListOfMoves) :-
    findall(BoardOut, valid_move(Player, BoardIn, _Coords, BoardOut), ListOfMoves).

valid_move(Player, BoardIn, [I, J], BoardOut) :-
    nth1(I, BoardIn, Row),      % Select row
    nth1(J, Row, Stack),        % Select stack from row
    \+ is_empty(Stack),         % Stack is not empty
    nth0(0, Stack, Player),     % Piece is controlled by player
    I1 is I - 1,
%    I2 is I + 1,
%    J1 is J - 1,
%    J2 is J + 1,
    valid_move_up(BoardIn, [I, J], [I1, J], BoardOut)
%    valid_move_down(BoardIn, [I0, J0], [I4, J0], BoardOut);
%   valid_move_left(BoardIn, [I0, J0], [I0, J3], BoardOut);
%    valid_move_right(BoardIn, [I0, J0], [I0, J4], BoardOut)
    . 

valid_move_up(_, _, [I1, _], []) :-
    I1 =< 0, !.

valid_move_up(BoardIn, [I0, J0], [I1, J1], BoardOut) :-
    nth1(I1, BoardIn, Row),                               % Select row
    nth1(J1, Row, Stack),                                 % Select stack from row
    \+ is_empty(Stack),                                   % Stack is not empty.
    move(BoardIn, [I0, J0], [I1, J1], BoardOut),          % Move piece
    !.       

valid_move_up(BoardIn, [I0, J0], [I1, J1], BoardOut) :-
    I2 is I1 - 1,                                                  % Go up by a row
    valid_piece_move_up(BoardIn, [I0, J0], [I2, J1], BoardOut).
