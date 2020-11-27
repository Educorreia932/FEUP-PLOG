:- consult('info.pl').

diff(User1, User2, Difference, Film) :-
    vote(User1, MovieRatings1),
    vote(User2, MovieRatings2),
    member(Film-Rating1, MovieRatings1),
    member(Film-Rating2, MovieRatings2),
    Difference is abs(Rating2 - Rating1).
