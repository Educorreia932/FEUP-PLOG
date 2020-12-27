:- consult('info.pl').

allPerfs :-
    participant(ID, Age, Performance),
    format("~w:~w:~w", [ID, Performance, Age]), nl,
    fail.

allPerfs.