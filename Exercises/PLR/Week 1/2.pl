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

zebra_puzzle(Zebra, Agua) :-
    % Domain and variables definition
    Solucao = [Nacionalidade, Animais, Bebida, Cores, Tabaco],
    Nacionalidade = [Ingles, Espanhol, Noruegues, Ucraniano, Portugues],
    Animais = [Cao, Raposa, Iguana, Cavalo, Zebra],
    Bebida = [Sumo, Cha, Cafe, Leite, Agua],
    Cores = [Vermelho, Verde, Branco, Amarelo, Azul],
    Tabaco = [Chesterfields, Winston, LukyStrike, SGLights, Marlboro],

    flatten([Nacionalidade, Animais, Bebida, Cores, Tabaco], List),
    domain(List, 1, 5),

    % Restrictions
    all_distinct(Nacionalidade),
    all_distinct(Animais),
    all_distinct(Bebida),
    all_distinct(Cores),
    all_distinct(Tabaco),
    
    Ingles #= Vermelho,
    Espanhol #= Cao,
    Noruegues #= 1,
    Amarelo #= Marlboro,
    abs(Chesterfields - Raposa) #= 1, 
    abs(Noruegues - Azul) #= 1,
    Winston #= Iguana,
    LukyStrike #= Sumo,
    Ucraniano #= Cha,
    Portugues #= SGLights,
    abs(Marlboro - Cavalo) #= 1,
    Verde #= Cafe,
    Verde #= Branco + 1,
    Leite #= 3,

    % Solution search
    labeling([], List),
    write(Solucao), nl.