:- use_module(library(clpfd)). 

flatten(List, Result) :- 
    flatten(List, [], Result).

flatten([], Result, Result).

flatten([Head| Tail], Part, Result) :- 
    is_list(Head),
    !, 
    flatten(Head, HR),
    append(Part, HR, PR),
    flatten(Tail, PR, Result).

flatten([Head| Tail], Part, Result) :- 
    append(Part, [Head], PR),
    flatten(Tail, PR, Result).

flatten(_, _, _) :-
    fail.

tres_musicos(Instrumentos) :-
    % Domain and variables definition
    Solucao = [Nomes, Instrumentos, Dias],
    Nomes = [Joao, Antonio, Francisco],
    Instrumentos = [Harpa, Violoncelo, Piano],
    Dias = [Terca, Quinta],

    flatten(Solucao, List),
    domain(List, 1, 3),

    % Restrictions
    all_distinct(Nomes),
    all_distinct(Instrumentos),
    all_distinct(Dias),

    Antonio #\= Piano,
    Joao #= Quinta,
    Joao #\= Violoncelo,
    Violoncelo #= Quinta,

    % Solution search

    labeling([], List),
    write(Solucao), nl.
