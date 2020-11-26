:- consult('info.pl').

madeItThrough(Participant) :-
    performance(Participant, Times),
    member(120, Times).
