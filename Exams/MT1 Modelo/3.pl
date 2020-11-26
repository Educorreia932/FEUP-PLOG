:- consult('info.pl').

:- dynamic(abstentions/1).

abstentions(0).

patientJuri(JuriMember) :-
    patientJuriWrapped(JuriMember),
    abstentions(Abstentions),
    Abstentions >= 2.

patientJuriWrapped(JuriMember) :-
    performance(_, Times),
    nth1(JuriMember, Times, JuriTime),
    JuriTime =:= 120,
    abstentions(Abstentions),
    NewAbstentions is Abstentions + 1,
    assert(abstentions(NewAbstentions)),
    retract(abstentions(Abstentions)).
    