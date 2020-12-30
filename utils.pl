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