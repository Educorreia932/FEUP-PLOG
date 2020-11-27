:- consult('info.pl').

mostEffectivePlayers(Game, Players) :-
    findall(Player-Efficiency, (played(Player, Game, Hours, Percentage), Efficiency is Percentage / Hours), PlayersEfficiencies),
    topEfficiency(PlayersEfficiencies, Top),
    topPlayers(Top, PlayersEfficiencies, Players).

topPlayers(_, [], _).

topPlayers(Top, [Player-Efficiency|T], TopPlayers) :-
    Top =:= Efficiency,
    append(Players, [Player], TopPlayers),
    topPlayers(Top, T, Players).

topPlayers(Top, [_-Efficiency|T], TopPlayers) :-
    Top \= Efficiency,
    topPlayers(Top, T, TopPlayers).

topEfficiency([_-Efficiency], Efficiency).

topEfficiency([_-Efficiency|T], M) :-
    topEfficiency(T, M), 
    M >= Efficiency.

topEfficiency([_-Efficiency|T], Efficiency) :-
    topEfficiency(T, M),
    Efficiency > M.
    