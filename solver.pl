:- use_module(library(clpfd)).

:- include('utils.pl').

solve(RowsNumbers, ColumnsNumbers, Rows) :-
    % Domain and variables definition

    length(RowsNumbers, Size),                      % Get size of square
    generate_grid(Rows, Size),                      % Generate grid representing square

    % Constraints
    
    transpose(Rows, Columns),                       % Transpose rows matrix to get columns
    line_constraints(RowsNumbers, Rows),            % Apply row constraints
    line_constraints(ColumnsNumbers, Columns),      % Apply columns constraints
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

    square_line(TopI, LeftJ, Rows, Size, BorderWidth, 0),
    square_line(TopI, LeftJ, Columns, Size, BorderHeight, 0),

    BottomI is I + 1,

    square_interior(BottomI, J, Rows, Columns, Size, Width, 1, Height),

    IsSquare #<=> (
        Height #= Width - 1 #/\ 
        BorderWidth #>= Width + 2 #/\ 
        BorderHeight #>= Width + 2
    ).

square_interior(Size, _, _, _, Size, Width, _, Width).

square_interior(I, J, Rows, Columns, Size, Width, Before, Counter) :-
    square_line(I, J, Rows, Size, Length, 1),

    IsSquareLine #= ((Length #= Width) #/\ Before),
    NewCounter #= Counter + IsSquareLine,
    NewI is I + 1,

    square_interior(NewI, J, Rows, Columns, Size, Width, IsSquareLine, NewCounter).

% Gets Square Size with top left corner at I row and J column

square_line(I, J, Rows, Size, Length, Value) :-
    NextJ is J + 1,
    CellBefore #= 1,
    Counter #= 1,
    square_line(I, NextJ, Rows, Size, CellBefore, Counter, Length, Value).

% Reached end of line - update length

square_line(_, Size, _, Size, _, Length, Length, _).

% Count filled consecutive cells

square_line(I, J, Rows, Size, CellBefore, Counter, Length, Value) :-
    get_cell(I, J, Rows, Cell),                        
    IsValue #<=> ((Cell #= Value) #/\ CellBefore),     
    
    NewCounter #= Counter + IsValue,                
    NewJ is J + 1,                                   
    
    square_line(I, NewJ, Rows, Size, IsValue, NewCounter, Length, Value).  
    
    