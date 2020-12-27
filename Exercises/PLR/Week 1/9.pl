:- use_module(library(clpfd)). 

doesnt_contain(_, 0).
doesnt_contain(Digit, Number) :-
    Number mod 10 #\= Digit,
    NextNumber #= Number // 10,
    doesnt_contain(Digit, NextNumber).

zero_zeros(A, B) :-
    Vars = [A, B],
    domain(Vars, 1, 999999999),
    doesnt_contain(0, A),
    doesnt_contain(0, B),
    A * B #= 1000000000,
    labeling([], Vars).
