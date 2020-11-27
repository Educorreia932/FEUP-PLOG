:- consult('info.pl').

averageAge(Game, AverageAge) :-
    findall(Age, (player(_, Player, Age), played(Player, Game, _, _)), PlayerAges),
    average(PlayerAges, AverageAge).

average(List, Average):- 
    sumlist(List, Sum),
    length( List, Length),
    Length > 0, 
    Average is Sum / Length.