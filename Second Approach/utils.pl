:- use_module(library(between)).
:- use_module(library(lists)).


% Flattens a list by one-level

flatten([], []).

flatten([A|B],L) :- 
    is_list(A),
    flatten(B,B1), 
    !,
    append(A,B1,L).

flatten([A|B], [A|B1]) :- 
    flatten(B, B1).

