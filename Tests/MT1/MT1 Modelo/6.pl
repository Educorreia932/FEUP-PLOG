:- consult('info.pl').

noClickOnButton([]).

noClickOnButton([H|T]) :-
    H =:= 120,
    noClickOnButton(T).

successful(Participant) :-
    performance(Participant, Times),
    noClickOnButton(Times).

nSuccessfulParticipants(T) :-
    findall(Participant, successful(Participant), ListOfParticipants),
    length(ListOfParticipants, T).
