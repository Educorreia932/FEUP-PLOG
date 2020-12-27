:- consult('info.pl').

updatePlayer(Player, Game, Hours, Percentage) :-
    retract(played(Player, Game, OldHours, OldPercentage)),
    NewHours is OldHours + Hours,
    NewPercentage is OldPercentage + Percentage,
    assert(played(Player, Game, NewHours, NewPercentage)).