% Converts a list into a list of lists

create([], []).

create([H|T], [[H]|T2]) :-
    create(T, T2).

% Takes the first N elements of a list

take(L, N, L1) :- 
    length(L1, N), 
    append(L1, _, L).

% Removes the first N elements of a list

remove_n(List, N, ShorterList) :-
    length(Prefix, N),
    append(Prefix, ShorterList, List).