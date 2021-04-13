constroi_binarias(_, _, _, []).

constroi_binarias(I, K, Vars, [LBin|LBins]) :-
    I =< K, !,
    constroi_bins(I, Vars, LBin),
    I1 is I + 1,
    constroi_binarias(I1, K, Vars, LBins).


constroi_bins(_, [], []).

constroi_bins(I, [Var|T1], [LBin|T2]) :-
    LBin #<=> I #= Var,
    constroi_bins(I, T1, T2).
