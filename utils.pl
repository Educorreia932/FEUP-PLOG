:- use_module(library(between)).
:- use_module(library(lists)).

% Generate Square Grid

generate_grid(Grid, Size) :-
    generate_grid(Grid, Size, Size).

generate_grid([], _, 0).

generate_grid([GridRow|T], Size, Counter) :-
    C is Counter - 1,
    length(GridRow, Size),
    domain(GridRow, 0, 1),
    generate_grid(T, Size, C).


% Generate indexes from cells in grid

generate_indexes(Indexes, Size) :-
    generate_indexes(Indexes, 0, 0, Size).

generate_indexes([], Size, _, Size).            % Reached last row

generate_indexes(Indexes, I, Size, Size) :-     % Reached last column
    NewI is I + 1,                              % Skip to next row
    generate_indexes(Indexes, NewI, 0, Size).   

generate_indexes([[I, J]|T], I, J, Size) :-     % Iterating the columns of row 
    NewJ is J + 1,
    generate_indexes(T, I, NewJ, Size).


% Trim N elements from a list

trim(L, N, S) :-         
    length(P, N),        % Generate an unbound prefix list of the desired length
    append(P, S, L).     % Get the desired suffix.


% Flattens a list by one level

flatten([], []).

flatten([A|B], L) :- 
    is_list(A),
    flatten(B, B1), 
    !,
    append(A, B1, L).

flatten([A|B], [A|B1]) :- 
    flatten(B, B1).


% Get cell at Iᵗʰ row and Jᵗʰ column

get_cell(I, J, Rows, Cell) :-   
    nth0(I, Rows, Row),
    nth0(J, Row, Cell).

get_cell(-1, _, _, 0).          % Top border

get_cell(_, -1, _, 0).          % Left border

get_cell(I, _, Rows, 0) :-      % Bottom border
    length(Rows, Size),
    I == Size.

get_cell(_, J, Rows, 0) :-      % Right border
    length(Rows, Size),
    J == Size.