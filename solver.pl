:- use_module(library(clpfd)).

:- include('utils.pl').

solve(RowsNumbers, ColumnsNumbers, Rows) :-
    % Domain and variables definition

    length(RowsNumbers, Size),           % Get size of square
    generate_grid(Rows, Size), 

    % Constraints
    
    line_restrictions(RowsNumbers, Rows),
    transpose(Rows, Columns),
    line_restrictions(ColumnsNumbers, Columns),

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

line_restrictions([], []).

line_restrictions([FilledCells|T1], [GridLine|T2]) :-
    sum(GridLine, #=, FilledCells),
    line_restrictions(T1, T2).
