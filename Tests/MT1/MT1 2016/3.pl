:- consult('info.pl').

niceGuy(User) :-
    vote(User, MovieRatings),
    member(Film1-Rating1, MovieRatings),
    member(Film2-Rating2, MovieRatings),
    Rating1 >= 8,
    Rating2 >= 8,
    Film1 \= Film2.