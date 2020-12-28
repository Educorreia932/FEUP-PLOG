print_square([]).

print_square([Row|T]) :-
    print_row(Row),
    print_square(T).

print_row([1|T]) :-
    print_row(T).

print_row([1|T]) :-
    print_row(T).

print_row(['X'|T]) :-
    put_code(9746),
    print_row(T).
