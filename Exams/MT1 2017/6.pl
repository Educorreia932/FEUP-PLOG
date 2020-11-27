:- consult('info.pl').

fewHours(Player, Games) :-
    fewHoursAux(Player, [], Games).
    
fewHoursAux(Player, GamesAcc, Games) :-
    played(Player, Game, HoursPlayed, _),
    HoursPlayed < 10,
    \+ (member(Game, GamesAcc)),!,
    append(GamesAcc, [Game], NewGamesAcc),
    fewHoursAux(Player, NewGamesAcc, Games).
    
fewHoursAux(_, GamesAcc, GamesAcc).
    