:- consult('info.pl').

curto(Movie) :-
    film(Movie, _, Duration, _),
    Duration < 125.