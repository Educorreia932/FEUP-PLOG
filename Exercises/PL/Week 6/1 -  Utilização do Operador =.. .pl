map([], _, []).

map([C|R], Transfor, [TC|CR]):-
    apply(Transfor, [C, TC]),
    map(R, Transfor, CR).

apply(P, LArgs) :- 
    G =.. [P|LArgs], 
    G. 
