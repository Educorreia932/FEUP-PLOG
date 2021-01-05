:- use_module(library(clpfd)).

:- include('utils.pl').

solve(RowsNumbers, ColumnsNumbers, Rows) :-
    % Domain and variables definition

    length(RowsNumbers, Size),                      % Get size of square
    generate_grid(Rows, Size),                      % Generate grid representing square

    % Constraints
    
    transpose(Rows, Columns),                       % Transpose rows matrix to get columns
    %line_constraints(RowsNumbers, Rows),            % Apply row constraints
    %line_constraints(ColumnsNumbers, Columns),      % Apply columns constraints

    is_upper_left_corner(0, 0, Rows, 1), 
    is_square(0, 0, Rows, Columns, Size, 1),

    is_upper_left_corner(0, 9, Rows, 1), 
    is_square(0, 9, Rows, Columns, Size, 1),

    is_upper_left_corner(1, 2, Rows, 1), 
    is_square(1, 2, Rows, Columns, Size, 1),

    is_upper_left_corner(2, 0, Rows, 1), 
    is_square(2, 0, Rows, Columns, Size, 1),

    is_upper_left_corner(4, 0, Rows, 1), 
    is_square(4, 0, Rows, Columns, Size, 1),

    square_constraint(0, 0, Rows, Columns, Size),   % Apply square constraints

    % Solution search
    
    flatten(Rows, Vars),
    labeling([], Vars).

% Line constraints

line_constraints([], []).

line_constraints([FilledCells|T1], [GridLine|T2]) :-
    sum(GridLine, #=, FilledCells),
    line_constraints(T1, T2).

% Square constraints

square_constraint(Size, _, _, _, Size).                     % Reached end of grid

square_constraint(I, Size, Rows, Columns, Size) :-          % Reached end of row
    NewI is I + 1,                                          % Skip to next row
    square_constraint(NewI, 0, Rows, Columns, Size).

square_constraint(I, J, Rows, Columns, Size) :-
    is_upper_left_corner(I, J, Rows, IsUpperLeftCorner),    % Check if its upper left corner
    is_square(I, J, Rows, Columns, Size, IsSquare),         % Check if its square
    IsUpperLeftCorner #=> IsSquare,                         % If cell is upper left corner, there must be a square

    NextJ is J + 1,                                         % Next Cell
    square_constraint(I, NextJ, Rows, Columns, Size).       % Recursion

% Check if cell is upper left corner 

is_upper_left_corner(I, J, Rows, IsUpperLeftCorner) :-
    get_cell(I, J, Rows, Cell),                 % Get current cell

    TopI is I - 1,
    get_cell(TopI, J, Rows, TopCell),           % Get cell above
    
    LeftJ is J - 1,
    get_cell(I, LeftJ, Rows, LeftCell),         % Get left cell
    
    get_cell(TopI, LeftJ, Rows, TopLeftCell),   % Get diagonal cell

    (Cell #= 1 #/\ TopCell #= 0 #/\ LeftCell #= 0 #/\ TopLeftCell #= 0) #<=> IsUpperLeftCorner.

% Check if there is a square

is_square(I, J, Rows, Columns, Size, IsSquare) :-
    square_line(I, J, Rows, Size, Width, 1),
    
    TopI is I - 1,
    LeftJ is J - 1,

    square_line(TopI, J, Rows, Size, BorderWidth, 0),
    square_line(LeftJ, I, Columns, Size, BorderHeight, 0),

    BottomI is I + 1,

    Before #<=> Width #>= 1,

    square_interior(BottomI, J, Rows, Size, Width, Before, 0), 

    IsSquare #<=> (
        Before #/\ 
        BorderWidth #>= Width #/\
        BorderHeight #>= Width 
    ).

square_interior(Size, _, _, Size, 0, _, 0).

square_interior(Size, _, _, Size, Width, _, Counter) :-
    Counter #= Width - 1.

square_interior(I, J, Rows, Size, Width, Before, Counter) :-
    square_line(I, J, Rows, Size, Length, 1),

    IsSquareLine #= ((Length #= Width) #/\ Before),
    NewCounter #= Counter + IsSquareLine,
    NewI is I + 1,

    square_interior(NewI, J, Rows, Size, Width, IsSquareLine, NewCounter).

% Gets square size with top left corner at I row and J column

square_line(I, J, Rows, Size, Length, Value) :-
    square_line(I, J, Rows, Size, 1, 0, Length, Value).

% Reached end of line - update length

square_line(_, Size, _, Size, _, Length, Length, _).

% Count filled consecutive cells

square_line(I, J, Rows, Size, CellBefore, Counter, Length, Value) :-
    get_cell(I, J, Rows, Cell),                        
    IsValue #<=> ((Cell #= Value) #/\ CellBefore),     
    
    NewCounter #= Counter + IsValue,                
    NewJ is J + 1,                                   
    
    square_line(I, NewJ, Rows, Size, IsValue, NewCounter, Length, Value).  
    