:- consult('utils.pl').

% Iterates through the pieces of a board

iterate_pieces(_, _, _, Width, Height, I, J) :- % Stop recursion
    I =:= Width,
    J =:= Height, !.

iterate_pieces(GameState, Player, ListOfMoves, I, J) :-                         
    valid_piece_moves(GameState, Player, ListOfPieceMoves, Width, Height, I, J),
    append(L, ListOfMoves, ListOfPieceMoves),  % Adds the list of moves calculated
    (J =:= Width -> I1 is I + 1, J1 is 1 ; J1 is J + 1), 
    iterate_pieces(Game State, Player, L, I1, J1).

% All possible and valid moves

valid_moves(GameState, Player, ListOfMoves) :-
    length(GameState, Height),      % Number of rows
    nth0(GameState, 0, Row),        % Gets element at index 0
    length(Row, Width),             % Number of collumns
    iterate_pieces(GameState, Player, ListOfMoves, Width, Height, 1, 1).    

valid_piece_moves(GameState, Player, [L1, L2, L3, L4], Width, Height, I, J) :-
    nth1(I, GameState, Row),                                        % Select row
    nth1(J, Row, Stack),                                            % Select stack from row
    nth0(0, Stack, Piece),                                          % Select stack's top piece
    Piece == Player,                                                % Player's color piece
    valid_piece_move_up(GameState, [I, J, I, J], L1),               % Get valid move up
    valid_piece_move_down(GameState, [I, J, I, J], L2, Height),     % Get valid move down
    valid_piece_move_left(GameState, [I, J, I, J], L3),             % Get valid move left
    valid_piece_move_right(GameState, [I, J, I, J], L4, Right).     % Get valid move right
    
% Valid move above

valid_piece_move_up(_, [_, _, I, _], []) :- % Stop Recursion 
    I =:= 0, !.
    
valid_piece_move_up(GameState, [I0, J0, I1, J0], [I0, J0, I1, J0]) :-
    I0 \== I1,                   % Start cell is different from end cell
    nth1(I1, GameState, Row),    % Select row
    nth1(J0, Row, Stack),        % Select stack from row
    \+ is_empty(Stack), !.       % Stack is not empty

valid_piece_move_up(GameState, [I0, J0, I1, J0], ValidMove) :-   
    I is I1 - 1,                 % Go up a row
    valid_piece_move_up(GameState, [I0, J0, I, J0], ValidMove).

% Valid move below

valid_piece_move_down(_, [_, _, I, _], [], Height) :-
    I > Height, !.
    
valid_piece_move_down(GameState, [I0, J0, I1, J0], [I0, J0, I1, J0], Height) :-
    I0 \== I1,                   % Start cell is different from end cell
    nth1(I1, GameState, Row),    % Select row
    nth1(J0, Row, Stack),        % Select stack from row
    \+ is_empty(Stack), !.       % Stack is not empty

valid_piece_move_down(GameState, [I0, J0, I1, J0], ValidMove, Height) :-   
    I is I1 + 1,                 % Go down a row
    valid_piece_move_down(GameState, [I0, J0, I, J0], ValidMove, Height).

% Valid move left

valid_piece_move_left(_, [_, _, _, J], []) :-
    J =:= 0, !.
    
valid_piece_move_left(GameState, [I0, J0, I0, J1], [I0, J0, I0, J1]) :-
    J0 \== J1,                   % Start cell is different from end cell
    nth1(I0, GameState, Row),    % Select row
    nth1(J1, Row, Stack),        % Select stack from row
    \+ is_empty(Stack), !.       % Stack is not empty

valid_piece_move_left(GameState, [I0, J0, I0, J1], ValidMove) :-   
    J is J1 - 1,                 % Go to left a collumn
    valid_piece_move_left(GameState, [I0, J0, I0, J], ValidMove).

% Valid move right

valid_piece_move_right(_, [_, _, _, J], [], Width) :-   % Stop Recursion
    J > Width, !.
    
valid_piece_move_right(GameState, [I0, J0, I0, J1], [I0, J0, I0, J1], Width) :-
    J0 \== J1,                   % Start cell is different from end cell
    nth1(I0, GameState, Row),    % Select row
    nth1(J1, Row, Stack),        % Select stack from row
    \+ is_empty(Stack), !.       % Stack is not empty

valid_piece_move_right(GameState, [I0, J0, I0, J1], ValidMove, Width) :-   
    J is J1 + 1,                 % Go to right a collumn
    valid_piece_move_right(GameState, [I0, J0, I0, J], ValidMove, Width).
