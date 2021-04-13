:- use_module(library(clpfd)).
:- use_module(library(lists)).

objeto(piano, 3, 30).
objeto(cadeira, 1, 10).
objeto(cama, 3, 15).
objeto(mesa, 2, 15).

homens(4).

tempo_max(60).

furniture :-
    % Variables
    homens(NumHomens),
    tempo_max(TempoMax),
    findall(Objeto, objeto(Objeto, _, _), Objetos),
    findall(Homens, objeto(_, Homens, _), HomensRequeridos),
    findall(Duracao, objeto(_, _, Duracao), Duracoes),

    % Domain
    length(Objetos, NumObjetos),
    length(StartTimes, NumObjetos),
    length(EndTimes, NumObjetos),
    domain(StartTimes, 1, TempoMax),
    domain(EndTimes, 1, TempoMax),

    % Constraints
    get_tasks(Duracoes, StartTimes, EndTimes, HomensRequeridos, 1, [], Tasks),
    maximum(Tempo, EndTimes),
    Tempo #=< TempoMax,
    cumulative(Tasks, [limit(NumHomens)]),
    append(StartTimes, EndTimes, Vars),

    % Labeling
    labeling([minimize(Tempo)], Vars),
    write(Tempo), nl,
    write(StartTimes).

get_tasks([], [], [], [], _, Tasks, Tasks).

get_tasks([Duracao|T1], [StartTime|T2], [EndTime|T3], [Homens|T4], Count, Accum, Tasks) :-
    Task = task(StartTime, Duracao, EndTime, Homens, Count),
    append(Accum, Task, NewAccum),
    NewCount is Count + 1,
    get_tasks(T1, T2, T3, T4, NewCount, NewAccum, Tasks).

