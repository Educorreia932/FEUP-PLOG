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

    get_cell(I, J, Rows, Cell),                 % Get current cell
    (Cell #= 1) #<=> IsFilled,                  % Cell is filled

    % Top cell

    TopI is I - 1,
    get_cell(TopI, J, Rows, TopCell),           % Get cell above
    (TopCell #= 0) #<=> IsTopBlank,             % Top cell is empty
    
    % Left cell
    
    LeftJ is J - 1,
    get_cell(I, LeftJ, Rows, LeftCell),         % Get left cell
    (LeftCell #= 0) #<=> IsLeftBlank,           % Left cell is empty

    % Upper left cell   
    
    get_cell(TopI, LeftJ, Rows, TopLeftCell),   % Get diagonal cell
    (TopLeftCell #= 0) #<=> IsTopLeftBlank,     % Diagonal Cell is empty

    (IsFilled #/\ IsTopBlank #/\ IsLeftBlank #/\ IsTopLeftBlank) #<=> IsUpperLeftCorner.


% Check if there is a square

is_square(I, J, Rows, Columns, Size, IsSquare) :-
    square_size(I, J, Rows, Size, Width),             % Get Square Width
    square_size(I, J, Columns, Size, Height),         % Get Square Height
    
    % IsSquare is true if width=height
    ((Height #>= 1) #/\ (Width #>= 1) #/\ (Height #= Width)) #<=> IsSquare. 


% Gets Square Size with top left corner at I row and J column

square_size(I, J, Rows, Size, Length) :-
    NextJ is J + 1,
    square_size_aux(I, NextJ, Rows, Size, 1, 1, Length).


% Reached end of line - update length

square_size_aux(_, Size, _, Size, _, Counter, Counter).     

% Count filled consecutive cells

square_size_aux(I, J, Rows, Size, CellBefore, Counter, Length) :-

    get_cell(I, J, Rows, Cell),                      % Get current cell
    ((Cell #= 1) #/\ CellBefore) #<=> IsFilled,      % Current Cell is filled if it is 1 and the cell before was filled too
    
    NewCounter #= Counter + IsFilled,                % Update length
    NewJ is J + 1,                                   % Update column 
    
    square_size_aux(I, NewJ, Rows, Size, IsFilled, NewCounter, Length).   % Recursion  

    