mostSimilar(Film, Similarity , Films) :-
    setof(F, (similarity(F, _, S), F \= Film, S >= 10), Films),
    (Films = [] -> Similarity = 0;
    Similarity = 30).

