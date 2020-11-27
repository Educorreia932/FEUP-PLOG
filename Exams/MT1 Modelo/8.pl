:- consult('1.pl').

:- use_module(library(lists)).

eligibleOutcome(Id, Perf, TT) :-
    performance(Id, Times),
    madeItThrough(Id),
    participant(Id, _, Perf),
    sumlist(Times, TT).

nextPhase(N, Participants) :-
    setof(TT-Id-Perf, (eligibleOutcome(Id, Perf, TT)), EligibleParticipants),
    keysort(EligibleParticipants, SortedParticipants),   
    reverse(SortedParticipants, FullParticipants),   
    prefix_length(FullParticipants, Participants, N).
