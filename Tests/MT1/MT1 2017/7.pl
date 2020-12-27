:- consult('info.pl').

ageRange(MinAge, MaxAge, Players) :-
    findall(Player, (player(Player, _, Age), Age >= MinAge, Age =< MaxAge), Players).