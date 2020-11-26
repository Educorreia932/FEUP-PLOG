:- consult('info.pl').

juriFans(JuriFansList) :-
    auxJuriFans([], [], JuriFansList).
    
auxJuriFans(Visited, AuxJuriFans, JuriFansList) :-
    performance(Participant, Times),
    participant(Participant, _, _),
    \+member(Participant, Visited),
    findall(N, (nth1(N, Times, Time), Time == 120), NotPressed),
    append(AuxJuriFans, [Participant-NotPressed], NewAuxJuriFans),
    append(Visited, [Participant], NewVisited),
    auxJuriFans(NewVisited, NewAuxJuriFans, JuriFansList).

auxJuriFans(_, AuxJuriFans, AuxJuriFans).