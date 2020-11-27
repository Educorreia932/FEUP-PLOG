:- consult('info.pl').

juriTimes([Participant|T], JuriMember, _, _) :-
    juriTimesWrapped([Participant|T], JuriMember, [], 0).

juriTimesWrapped([], _ , Times, Total) :-
    print('Times = '),
    print(Times), nl,
    format('Total = ~w', Total).

juriTimesWrapped([Participant|T], JuriMember, Times, Total) :-
    performance(Participant, ParticipantTimes),
    nth1(JuriMember, ParticipantTimes, JuriTime),
    append(Times, [JuriTime], NewTimes),
    NewTotal is Total + JuriTime,
    juriTimesWrapped(T, JuriMember, NewTimes, NewTotal).

nth1(1, [H|_], H).

nth1(Index, [H | List], Elem) :-
  Index > 1,
  NextIndex is Index - 1,
  nth1(NextIndex, List, Elem).
