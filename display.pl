print_square([], ColumnNumbers, []) :-
    print_columns_numbers(ColumnNumbers).          

print_square([RowNumber|T1], ColumnNumbers, [Line|T2]) :-    
    print_row(Line, RowNumber),     % Print row
    print_square(T1, ColumnNumbers, T2).               % Print rest of square

print_row([], RowNumber) :-
    format(' ~w\n', RowNumber).

print_row([0|T], RowNumber) :-         % Blank cell
    put_code(9633),         
    print_row(T, RowNumber).           

print_row([1|T], RowNumber) :-         % Filled cell
    put_code(9632),
    print_row(T, RowNumber).

print_row(['X'|T], RowNumber) :-       % Blocked cell
    put_code(9746),
    print_row(T, RowNumber).

print_columns_numbers([]).

print_columns_numbers([ColumnNumber|T]) :-
    print(ColumnNumber),
    print_columns_numbers(T).
