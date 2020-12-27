elemsComuns([], [], _).

elemsComuns([H|T], [H|C], List2) :-
    member(H, List2),
    elemsComuns(T, C, List2).

elemsComuns([_|T], Common, List2) :-
    elemsComuns(T, Common, List2).