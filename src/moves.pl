:- use_module(library(between)).

% Move piece from a cell to other

move(GameState, [I0, J0, I1, J1], NewGameState) :-
    nth0(I0, GameState, RowStart),           % Select starting row
    nth0(J0, RowStart, StackStart),          % Select stack from row

    replace(RowStart, J0, [], RowAux),       % Remove stack from starting cell
    replace(GameState, I0, RowAux, BoardAux),          

    nth0(I1, BoardAux, RowEnd),              % Select final row
    nth0(J1, RowEnd, StackEnd),              % Select stack from row
    append(StackStart, StackEnd, Stack),     % Add the piece to the top of the stack
    
    replace(RowEnd, J1, Stack, Row),         % Move stack to final cell
    replace(BoardAux, I1, Row, NewGameState), !.

% Returns the board's width and height

board_dimensions(Board, Width, Height) :- 
    length(Board, Height),
    nth0(0, Board, Row),      
    length(Row, Width).

% Checks if there are any pieces in the same line between two cells

has_pieces_between(Board, I, J0, I, J1) :-    % Same row
    exclusive_between(J0, J1, J),
    get_cell(Board, I, J, Stack),
    \+ is_empty(Stack).

has_pieces_between(Board, I0, J, I1, J) :-    % Same column
    exclusive_between(I0, I1, I),
    get_cell(Board, I, J, Stack),
    \+ is_empty(Stack).


% All possible and valid moves

valid_moves(BoardIn, Player, ListOfMoves) :-
    findall(BoardOut, valid_move(Player, BoardIn, BoardOut), ListOfMoves).

valid_move(Player, BoardIn, BoardOut) :-
    board_dimensions(BoardIn, Width, Height),

    exclusive_between(-1, Height, I0),                    % Generate start cell coordinates
    exclusive_between(-1, Width, J0),
    get_cell(BoardIn, I0, J0, StackStart),                % Get start cell
    nth0(0, StackStart, Player),                          % Piece is controlled by player

    exclusive_between(-1, Height, I1),                    % Generate end cell coordinates
    exclusive_between(-1, Width, J1),
    get_cell(BoardIn, I1, J1, StackEnd),                  % Get end cell
    \+ is_empty(StackEnd),                                % Cell is not empty

    \+ is_same_cell(I0, J0, I1, J1),                      % Start and cell coordinates are different
    (I0 == I1; J0 == J1),
    \+ has_pieces_between(BoardIn, I0, J0, I1, J1),
    move(BoardIn, [I0, J0, I1, J1], BoardOut). 
