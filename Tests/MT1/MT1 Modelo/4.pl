:- consult('info.pl').

sumlist([], 0).

sumlist([H|T], Sum) :-
   sumlist(T, Rest),
   Sum is H + Rest.

participantTimes(Participant, Total) :-
    performance(Participant, Times),
    sumlist(Times, Total).
 
bestParticipant(P1, P2, P) :-
    participantTimes(P1, T1),
    participantTimes(P2, T2),
    T1 \== T2,
    T is max(T1, T2),
    participantTimes(P, T).