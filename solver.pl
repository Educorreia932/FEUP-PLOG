:- use_module(library(clpfd)).

:- include('utils.pl').

solve(RowsNumbers, ColumnsNumbers, Rows) :-
    % Domain and variables definition

    length(RowsNumbers, Size),           % Get size of square
    generate_grid(Rows, Size),           % Generate grid representing square

    % Constraints
    
    transpose(Rows, Columns),
    line_constraints(RowsNumbers, Rows),
    line_constraints(ColumnsNumbers, Columns),
    find_square(0, 0, Rows, Size),

    % Solution search
    
    flatten(Rows, Vars),
    labeling([], Vars).

generate_grid(Grid, Size) :-
    generate_grid(Grid, Size, Size).

generate_grid([], _, 0).

generate_grid([GridRow|T], Size, Counter) :-
    C is Counter - 1,
    length(GridRow, Size),
    domain(GridRow, 0, 1),
    generate_grid(T, Size, C).

% Line constraints

line_constraints([], []).

line_constraints([FilledCells|T1], [GridLine|T2]) :-
    sum(GridLine, #=, FilledCells),
    line_constraints(T1, T2).

% Square constraints

find_square(Size, _, _, Size).

find_square(I, Size, _, Size) :-        % Next row
    NewI is I + 1,  
    find_square(NewI, 0, _, Size).

find_square(I, J, Rows, Size) :-
    get_cell(I, J, Rows, Cell),                                      

    Cell #= 1 #<=> IsFilled,                   % If cell is filled  

    % Top cell

    TopI is I - 1,
    get_cell(TopI, J, Rows, TopCell),
    TopCell #= 0 #<=> IsTopBlank,
    
    % Left cell
    
    LeftJ is J - 1,
    get_cell(I, LeftJ, Rows, LeftCell),
    LeftCell #= 0 #<=> IsLeftBlank,

    % Upper left cell   
    
    get_cell(TopI, LeftJ, Rows, TopLeftCell),
    TopLeftCell #= 0 #<=> IsTopLeftBlank,

    (IsFilled #/\ IsTopBlank #/\ IsLeftBlank #/\ IsTopLeftBlank) #<=> IsSquare, % Is top left corner of a cell
    square_constraints(I, J, Rows, SquareSize),
    SquareSize #>= 0 #<=> IsSquare,

    NextJ is J + 1,
    
    find_square(I, NextJ, Rows, Size).   

square_constraints(_, J, Rows, _) :-
    length(Rows, Length),   
    J == Length.          

square_constraints(I, J, Rows, IsSquare) :-
    NewI is I + 1,
    NewJ is J + 1,
    square_lines_constraints(I, J, Rows, IsSquare),
    square_constraints(NewI, NewJ, Rows, IsSquare).

square_lines_constraints(I, J, Rows, IsSquare) :-
    get_cell(BottomI, J, Rows, BottomCell),
    get_cell(I, RightJ, Rows, RightCell),
    % Verificar interior
    ((BottomI - I #= RightJ - J) #/\ (BottomI - I #>= 0) #/\ (RightJ - J #>= 0) #/\ (BottomCell #= RightCell)) #<=> IsSquare.
