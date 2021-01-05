:- include('solver.pl').
:- include('puzzles.pl').
:- include('display.pl').

% ==================================================
% Testing Purposes
% See puzzles in file 'puzzles.pl'
% ==================================================

test_puzzle1 :-
    puzzle1(Rows, Columns),
    solve(Rows, Columns, Square),
    Solution = [
        [1, 0, 0, 0, 0, 0, 0, 0, 0, 1],
        [0, 0, 1, 0, 0, 0, 0, 1, 0, 0],
        [1, 0, 0, 0, 0, 0, 0, 0, 0, 1],
        [0, 0, 0, 1, 0, 0, 0, 1, 0, 0],
        [1, 1, 0, 0, 0, 0, 0, 0, 0, 1],
        [1, 1, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 1, 1, 0], 
        [0, 1, 1, 1, 0, 0, 0, 1, 1, 0],
        [0, 1, 1, 1, 0, 0, 0, 0, 0, 0],
        [0, 1, 1, 1, 0, 0, 0, 0, 1, 0]
    ],
    nl, print('Result:'), nl,
    print_square(Rows, Columns, Square), nl.
    % nl, print('Expected:'), nl,
    % print_square(Rows, Columns, Solution), nl.

test_puzzle2 :-
    puzzle2(Rows, Columns),
    solve(Rows, Columns, Square),
    Solution = [
        [1, 0, 1, 0, 1, 1, 0, 0, 0, 0],
        [0, 0, 0, 0, 1, 1, 0, 1, 0, 1],
        [1, 0, 1, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 1, 1],
        [1, 1, 0, 0, 0, 1, 0, 0, 1, 1],
        [1, 1, 0, 1, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 1, 1, 1, 1],
        [0, 0, 0, 0, 0, 0, 1, 1, 1, 1],
        [0, 0, 0, 0, 0, 0, 1, 1, 1, 1],
        [0, 1, 0, 0, 0, 0, 1, 1, 1, 1]
    ],
    nl, print('Result:'), nl,
    print_square(Rows, Columns, Square), nl,
    nl, print('Expected:'), nl,
    print_square(Rows, Columns, Solution), nl.

test_puzzle3 :-
    puzzle3(Rows, Columns),
    solve(Rows, Columns, Square),
    Solution = [
        [0, 0, 0, 0, 0, 1, 1, 0, 1, 0],
        [0, 0, 0, 0, 0, 1, 1, 0, 0, 0],
        [1, 0, 1, 0, 0, 0, 0, 0, 0, 1],
        [0, 0, 0, 0, 1, 1, 0, 1, 0, 0],
        [0, 0, 1, 0, 1, 1, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 1, 1, 1],
        [0, 0, 0, 0, 0, 0, 0, 1, 1, 1],
        [0, 0, 0, 0, 0, 0, 0, 1, 1, 1],
        [0, 1, 0, 1, 1, 0, 0, 0, 0, 0],
        [0, 0, 0, 1, 1, 0, 1, 0, 0, 0]
    ],
    nl, print('Result:'), nl,
    print_square(Rows, Columns, Square), nl,
    nl, print('Expected:'), nl,
    print_square(Rows, Columns, Solution), nl.

test_puzzle4 :-
    puzzle4(Rows, Columns),
    solve(Rows, Columns, Square),
    Solution = [
        [1, 1, 1, 1, 1, 0, 1, 0, 1, 1],
        [1, 1, 1, 1, 1, 0, 0, 0, 1, 1],
        [1, 1, 1, 1, 1, 0, 1, 0, 0, 0],
        [1, 1, 1, 1, 1, 0, 0, 0, 0, 1],
        [1, 1, 1, 1, 1, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 1, 1, 1],
        [0, 0, 0, 1, 0, 0, 0, 1, 1, 1],
        [1, 0, 0, 0, 0, 0, 0, 1, 1, 1],
        [0, 0, 1, 0, 1, 1, 0, 0, 0, 0],
        [0, 0, 0, 0, 1, 1, 0, 1, 0, 0]
    ],
    nl, print('Result:'), nl,
    print_square(Rows, Columns, Square), nl,
    nl, print('Expected:'), nl,
    print_square(Rows, Columns, Solution), nl.

test_puzzle5 :- 
    puzzle5(Rows, Columns),
    solve(Rows, Columns, Square),
    Solution = [
        [1, 1, 1, 0, 1, 0, 1, 1, 1, 0],
        [1, 1, 1, 0, 0, 0, 1, 1, 1, 0],
        [1, 1, 1, 0, 0, 0, 1, 1, 1, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [1, 1, 0, 1, 1, 1, 0, 0, 0, 1],
        [1, 1, 0, 1, 1, 1, 0, 0, 0, 0],
        [0, 0, 0, 1, 1, 1, 0, 1, 0, 1],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 1, 1, 0, 1, 1, 0, 0, 1],
        [0, 0, 1, 1, 0, 1, 1, 0, 0, 0]
    ],
    nl, print('Result:'), nl,
    print_square(Rows, Columns, Square), nl,
    nl, print('Expected:'), nl,
    print_square(Rows, Columns, Solution), nl.

test_puzzle6 :-
    puzzle6(Rows, Columns),
    solve(Rows, Columns, Square),
    Solution = [
        [0, 1, 0, 0, 0, 1, 0, 0, 1, 1],
        [0, 0, 0, 0, 0, 0, 0, 0, 1, 1],
        [1, 0, 1, 1, 1, 0, 0, 0, 0, 0],
        [0, 0, 1, 1, 1, 0, 0, 0, 1, 0],
        [0, 0, 1, 1, 1, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 1, 0, 1],
        [0, 1, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 1, 0, 1, 1, 1, 0, 1],
        [1, 1, 0, 0, 0, 1, 1, 1, 0, 0],
        [1, 1, 0, 0, 0, 1, 1, 1, 0, 0]
    ],
    nl, print('Result:'), nl,
    print_square(Rows, Columns, Square), nl,
    nl, print('Expected:'), nl,
    print_square(Rows, Columns, Solution), nl.

test_puzzle7 :-
    puzzle7(Rows, Columns),
    solve(Rows, Columns, Square),
    Solution = [
        [0, 0, 1, 0, 1, 0, 1, 0, 0, 0],
        [1, 0, 0, 0, 0, 0, 0, 0, 1, 1],
        [0, 0, 0, 1, 1, 1, 0, 0, 1, 1],
        [1, 1, 0, 1, 1, 1, 0, 0, 0, 0],
        [1, 1, 0, 1, 1, 1, 0, 1, 1, 0],
        [0, 0, 0, 0, 0, 0, 0, 1, 1, 0],
        [1, 1, 1, 1, 0, 1, 0, 0, 0, 0],
        [1, 1, 1, 1, 0, 0, 0, 0, 1, 1],
        [1, 1, 1, 1, 0, 1, 1, 0, 1, 1],
        [1, 1, 1, 1, 0, 1, 1, 0, 0, 0]
    ],
    nl, print('Result:'), nl,
    print_square(Rows, Columns, Square), nl,
    nl, print('Expected:'), nl,
    print_square(Rows, Columns, Solution), nl.

test_puzzle8 :-
    puzzle8(Rows, Columns),
    solve(Rows, Columns, Square),
    Solution = [
        [1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0],
        [1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1],
        [1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0],
        [0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1],
        [0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1],
        [1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0],
        [1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1],
        [0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1],
        [0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1],
        [0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0],
        [0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0],
        [0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0]
    ],
    nl, print('Result:'), nl,
    print_square(Rows, Columns, Square), nl,
    nl, print('Expected:'), nl,
    print_square(Rows, Columns, Solution), nl.