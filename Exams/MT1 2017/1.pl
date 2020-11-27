:- consult('info.pl').

achievedALot(Player) :-
    played(Player, _, _, PercentUnlocked),
    PercentUnlocked >= 80.
