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
    square_constraints(0, 0, Rows, Size),

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

square_constraints(Size, _, _, Size).

square_constraints(I, size, _, Size) :-        % Next row
    NewI is I + 1,  
    square_constraints(NewI, 0, _, Size).

square_constraints(I, J, Rows, _) :-
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

    (IsFilled #/\ IsTopBlank #/\ IsLeftBlank) #<=> IsSquare,   % Is top left corner of a cell

    is_square_constraint(I, J, Rows, _, IsSquare).             % Apply square constraints

is_square_constraint(_, _, _, _, 0) :- fail.                   % Do not apply constraints if it isn't a square's top left corner cell

is_square_constraint(I, J, Rows, LineSize, 1) :-
    transpose(Rows, Columns),
    nth0(I, Rows, Row),
    nth0(J, Columns, Column),
    is_square_line_constraint(I, J, Row, LineWidth, 1),
    is_square_line_constraint(J, I, Column, LineHeight, 1),

    (LineWidth #= LineHeight) #=> (
        LineSize #= LineWidth,
        NewLineSize #= LineSize - 1,
        NewI is I + 1, 
        NewJ is I + 1, 
        is_square_constraint(NewI, NewJ, Rows, NewLineSize, 1)
    ).

is_square_line_constraint(I, Line, LineSize, Value) :-
    is_square_line_constraint(I, Line, LineSize, Value, 1).

is_square_line_constraint(_, _, _, _ , 0).  % Cell is different from value

is_square_line_constraint(Index, Line, LineSize, Value, 1) :-
    nth0(Index, Line, Cell),
    Cell #= Value #<=> IsValue,                                       % Check if cell is filled or not filled
    LineSize #= S + IsValue,                                          % line length decrement
    NewIndex is Index + 1,                                            % Update cell in
    is_square_line_constraint(NewIndex, Line, S, Value, IsValue).
