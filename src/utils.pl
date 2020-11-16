% Count the number of elements equal to N inside a list

count([], _C, 0).

count([C|T], C, N) :- 
    count(T, C, N1), 
    N is N1 + 1.            % Increment N because it is what we are looking for

count([X|T], C, N) :- 
    X \= C,                 % Different from what we are looking for, no need to increment N
    count(T, C, N).

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