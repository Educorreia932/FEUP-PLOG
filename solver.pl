:- use_module(library(clpfd)).

:- include('utils.pl').

solve(RowsNumbers, ColumnsNumbers, Rows) :-
    % Domain and variables definition

    length(RowsNumbers, Size),                      % Get size of square
    generate_grid(Rows, Size),                      % Generate grid representing square

    % Constraints
    
    transpose(Rows, Columns),                       % Transpose rows matrix to get columns
    % line_constraints(RowsNumbers, Rows),            % Apply row constraints
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

square_constraint(Size, _, _, _, Size).                 % Reached end of grid

square_constraint(I, Size, Rows, Columns, Size) :-      % Reached end of row
    NewI is I + 1,                                      % Skip to next row
    square_constraint(NewI, 0, Rows, Columns, Size).

square_constraint(I, J, Rows, Columns, Size) :-
    is_upper_left_corner(I, J, Rows, IsUpperLeftCorner),    % Check if its upper left corner
    is_square(I, J, Rows, Columns, Size, IsSquare),         % Check if its square
    IsUpperLeftCorner #=> IsSquare,                         % If cell is upper left corner, there must be a square

    NextJ is J + 1,                                         % Next Cell
    square_constraint(I, NextJ, Rows, Columns, Size).       % Recursion

% Check if cell is upper left corner 

is_upper_left_corner(I, J, Rows, IsUpperLeftCorner) :-
    % Get current cell

    get_cell(I, J, Rows, Cell),                 

    % Get Top cell

    TopI is I - 1,
    get_cell(TopI, J, Rows, TopCell),           
    
    % Get Left cell
    
    LeftJ is J - 1,
    get_cell(I, LeftJ, Rows, LeftCell),         

    % Get Upper left cell   
    
    get_cell(TopI, LeftJ, Rows, TopLeftCell),  

    (Cell #= 1 #/\ TopCell #= 0 #/\ LeftCell #= 0 #/\ TopLeftCell #= 0) #<=> IsUpperLeftCorner.

% Check if there is a square

is_square(I, J, Rows, Columns, Size, IsSquare) :-
    TopI is I - 1,                          % Diagonal line index
    LeftJ is J - 1,                         % Diagonal column index

    is_square_outline(TopI, LeftJ, Rows, Columns, Size, 0, IsBorder, BorderSize), % Constraint square border
    is_square_interior(I, J, Rows, Columns, IsInterior, SquareSize),             % Constraint square interior
    
    (IsBorder #/\ (BorderSize #= SquareSize + 2)) #<=> IsSquare.

is_square_interior(I, J, Rows, Columns, Size, IsInterior, SquareSize, Counter) :-
    is_square_outline(I, J, Rows, Columns, Size, 1, IsOutline, S),
    BottomI is I + 1,
    RightJ is J + 1,
    IsOutline #=> (SquareSize #= S),
    SmallerSquareSize #= SquareSize - 1,
    Counter #= (SquareSize #> 0),
    is_square_interior(BottomI, RightJ, Rows, Columns, Size, IsInterior, SmallerSquareSize).

is_square_outline(I, J, Rows, Columns, Size, Value, IsOutline, Width) :-
    is_square_line(I, J, Rows, Size, Width, Value),             % Get Square Width
    is_square_line(I, J, Columns, Size, Height, Value),         % Get Square Height
    
    % IsOutline is true if Width = Height
    (
        Height #>= 1 #/\ 
        Width #>= 1 #/\ 
        Height #= Width
    ) #<=> IsOutline. 

% Gets Square Size with top left corner at I row and J column

is_square_line(I, J, Rows, Size, Length, Value) :-
    NextJ is J + 1,
    is_square_line(I, NextJ, Rows, Size, 1, 1, Length, Value).

% Reached end of line - Update length

is_square_line(_, Size, _, Size, _, Counter, Counter, _).     

% Count filled consecutive cells

is_square_line(I, J, Rows, Size, CellBefore, Counter, Length, Value) :-
    get_cell(I, J, Rows, Cell),                      % Get current cell
    Cell #= Value #/\ CellBefore #<=> IsValue,      % Current Cell is filled if it is 1 and the cell before was filled too
    
    NewCounter #= Counter + IsValue,                % Update length
    NewJ is J + 1,                                   % Update column 
    
    is_square_line(I, NewJ, Rows, Size, IsValue, NewCounter, Length, Value).   % Recursion  
