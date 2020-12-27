:- consult('info.pl').

sumlist([], 0).

sumlist([H|T], Sum) :-
   sumlist(T, Rest),
   Sum is H + Rest.

timePlayingGames(Player, Games, ListOfTimes, SumTimes) :-
    length(Games, N),
    timePlayingGamesWrapped(Player, Games, ListOfTimes, N),
    sumlist(ListOfTimes, SumTimes).

timePlayingGames(Player, Games, [0], 0) :-
    length(Games, N),
    \+ timePlayingGamesWrapped(Player, Games, _, N).

timePlayingGamesWrapped(_, _, _, 0).

timePlayingGamesWrapped(Player, [Game|T1], [HoursPlayed|T2], N) :-
    N1 is N - 1,
    played(Player, Game, HoursPlayed, _),
    timePlayingGamesWrapped(Player, T1, T2, N1).