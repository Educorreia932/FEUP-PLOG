:- consult('info.pl').

isAgeAppropriate(Name, Game) :-
    player(Name, _, PlayerAge),
    game(Game, _, MinAge),
    PlayerAge >= MinAge.