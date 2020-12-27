impoe(X, L) :-
    length(Mid, X),
    append(L1, [X|_], L), 
    append(_, [X|Mid], L1).

impoeN(0, _).

impoeN(X, L) :-
    X1 is X - 1,
    impoe(X, L),
    impoeN(X1, L).

langford(0, _).

langford(N, L) :-
    N1 is 2 * N,
    length(L, N1),
    impoeN(N, L).
