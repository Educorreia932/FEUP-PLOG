/* 5. a) */

membro(X, [H, T]) :-
    X = H.

membro(X, [H, T]) :-
    membro(X, T).

/* 5. b) */

membro(X, L) :-
    append(_, [X|_], L). 

/* 5. c) */
last(L, X) :- 
    append(_, [X], L). 