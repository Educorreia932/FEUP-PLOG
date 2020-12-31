:- use_module(library(clpfd)).

constroi(NEmb, Orcamento, EmbPorObjeto, CustoPorObjeto, EmbUsadas, Objetos) :-
    % Variables
    length(Objetos, 4),   % Does it have to be hardcoded?

    % Domain
    length(EmbPorObjeto, N),
    domain(Objetos, 1, N),

    % Constraints
    all_distinct(Objetos),
    sumById(Objetos, EmbPorObjeto, EmbUsadas),
    sumById(Objetos, CustoPorObjeto, CustoTotal),
    EmbUsadas #=< NEmb,
	CustoTotal #=< Orcamento,

    % Labelling
    labeling([maximize(EmbUsadas)], Objetos).       

sumById([], _, 0).

sumById([Id|Ids], ToSum, Sum):-
	sumById(Ids, ToSum, AccSum),
	element(Id, ToSum, CurrSum),
	Sum #= AccSum + CurrSum.

% constroi(30, 100, [6,4,12,20,6], [20,50,10,20,15], EmbUsadas, Objetos).
% constroi(50, 100, [6,4,12,20,6], [20,50,10,20,15], EmbUsadas, Objetos).
