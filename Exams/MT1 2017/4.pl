:- consult('info.pl').

listGamesOfCategory(Cat) :-
    game(Game, Categories, MinAge),
    member(Cat, Categories),
    format('~w (~w)', [Game, MinAge]), nl,
    fail.

listGamesOfCategory(_).
