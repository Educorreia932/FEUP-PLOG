:- use_module(library(between)).
:- use_module(library(lists)).

trim(L, N, S) :-         % Trim N elements from a list
    length(P, N),        % Generate an unbound prefix list of the desired length
    append(P, S, L).     % Get the desired suffix.
    
flatten([], []).

flatten([A|B], L) :- 
    is_list(A),
    flatten(B, B1), 
    !,
    append(A, B1, L).

flatten([A|B], [A|B1]) :- 
    flatten(B, B1).

get_cell(I, J, Rows, Cell) :-   % Get cell at Iᵗʰ row and Jᵗʰ column
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