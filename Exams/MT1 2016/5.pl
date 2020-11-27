:- consult('info.pl').

printCategory(Category) :-
    film(Title, Categories, Duration, AvgClassification),
    member(Category, Categories),
    format('~w (~wmin, ~w/10)', [Title, Duration, AvgClassification]), nl,
    fail.

printCategory(_).
