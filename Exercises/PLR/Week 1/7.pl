:- use_module(library(clpfd)). 

peru(Price) :-
    Vars = [FirstDigit, LastDigit],
    domain(Vars, 0, 9),
    (FirstDigit * 1000 + 670 + LastDigit) mod 72 #= 0,
    (FirstDigit * 1000 + 670 + LastDigit) div 72 #= Price,
    labeling([], Vars).