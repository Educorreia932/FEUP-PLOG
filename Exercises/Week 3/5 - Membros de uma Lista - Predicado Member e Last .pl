/* 5. a) */

membro(X, [X, T]).

membro(X, [H, T]) :-
    membro(X, T).

/* 5. b) */

membro(X, L) :-
    append(_, [X|_], L). 

/* 5. c) */
last(L, X) :- 
    append(_, [X], L). 