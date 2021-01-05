:- include('solver.pl').
:- include('puzzles.pl').
:- include('display.pl').

% ==================================================
% Testing Purposes
% See puzzles in file 'puzzles.pl'
% ==================================================

test_puzzle1 :-
<<<<<<< HEAD:tests.pl
    puzzle1(Rows, Columns),
    solve(Rows, Columns, Square),
=======
    puzzle1(NumSquares, Rows, Columns),
    solve(NumSquares, Rows, Columns),
>>>>>>> disjointApproach:Second Approach/tests.pl
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
<<<<<<< HEAD:tests.pl
    nl, print('Result:'), nl,
    print_square(Rows, Columns, Square), nl.
    % nl, print('Expected:'), nl,
    % print_square(Rows, Columns, Solution), nl.

test_puzzle2 :-
    puzzle2(Rows, Columns),
    solve(Rows, Columns, Square),
=======
    print_square(Rows, Columns, Solution).

test_puzzle2 :-
    puzzle2(NumSquares, Rows, Columns),
    solve(NumSquares, Rows, Columns),
>>>>>>> disjointApproach:Second Approach/tests.pl
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
<<<<<<< HEAD:tests.pl
    nl, print('Result:'), nl,
    print_square(Rows, Columns, Square), nl,
    nl, print('Expected:'), nl,
    print_square(Rows, Columns, Solution), nl.

test_puzzle3 :-
    puzzle3(Rows, Columns),
    solve(Rows, Columns, Square),
=======
    print_square(Rows, Columns, Solution).

test_puzzle3 :-
    puzzle3(NumSquares, Rows, Columns),
    solve(NumSquares, Rows, Columns),
>>>>>>> disjointApproach:Second Approach/tests.pl
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
<<<<<<< HEAD:tests.pl
    nl, print('Result:'), nl,
    print_square(Rows, Columns, Square), nl,
    nl, print('Expected:'), nl,
    print_square(Rows, Columns, Solution), nl.

test_puzzle4 :-
    puzzle4(Rows, Columns),
    solve(Rows, Columns, Square),
=======
    print_square(Rows, Columns, Solution).

test_puzzle4 :-
    puzzle4(NumSquares, Rows, Columns),
    solve(NumSquares, Rows, Columns),
>>>>>>> disjointApproach:Second Approach/tests.pl
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
<<<<<<< HEAD:tests.pl
    nl, print('Result:'), nl,
    print_square(Rows, Columns, Square), nl,
    nl, print('Expected:'), nl,
    print_square(Rows, Columns, Solution), nl.

test_puzzle5 :- 
    puzzle5(Rows, Columns),
    solve(Rows, Columns, Square),
=======
    print_square(Rows, Columns, Solution).

test_puzzle5 :- 
    puzzle5(NumSquares, Rows, Columns),
    solve(NumSquares, Rows, Columns),
>>>>>>> disjointApproach:Second Approach/tests.pl
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
<<<<<<< HEAD:tests.pl
    nl, print('Result:'), nl,
    print_square(Rows, Columns, Square), nl,
    nl, print('Expected:'), nl,
    print_square(Rows, Columns, Solution), nl.

test_puzzle6 :-
    puzzle6(Rows, Columns),
    solve(Rows, Columns, Square),
=======
    print_square(Rows, Columns, Solution).

test_puzzle6 :-
    puzzle6(NumSquares, Rows, Columns),
    solve(NumSquares, Rows, Columns),
    Solution = [
        [0, 0, 0, 0, 0, 0, 0, 0, 1, 0],
        [0, 0, 0, 1, 0, 1, 0, 0, 0, 1],
        [0, 1, 0, 0, 0, 0, 0, 1, 0, 0],
        [0, 0, 0, 1, 0, 1, 0, 0, 0, 1],
        [0, 1, 0, 0, 0, 0, 0, 1, 0, 0],
        [0, 0, 0, 0, 1, 0, 0, 0, 0, 1],
        [0, 0, 1, 0, 0, 0, 0, 1, 0, 0],
        [1, 0, 0, 0, 1, 0, 0, 0, 0, 1],
        [0, 0, 1, 0, 0, 0, 1, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 1, 0]
    ],
    print_square(Rows, Columns, Solution).

test_puzzle7 :-
    puzzle7(NumSquares, Rows, Columns),
    solve(NumSquares, Rows, Columns),
>>>>>>> disjointApproach:Second Approach/tests.pl
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
<<<<<<< HEAD:tests.pl
    nl, print('Result:'), nl,
    print_square(Rows, Columns, Square), nl,
    nl, print('Expected:'), nl,
    print_square(Rows, Columns, Solution), nl.

test_puzzle7 :-
    puzzle7(Rows, Columns),
    solve(Rows, Columns, Square),
=======
    print_square(Rows, Columns, Solution).

test_puzzle8 :-
    puzzle8(NumSquares, Rows, Columns),
    solve(NumSquares, Rows, Columns),
>>>>>>> disjointApproach:Second Approach/tests.pl
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
<<<<<<< HEAD:tests.pl
    nl, print('Result:'), nl,
    print_square(Rows, Columns, Square), nl,
    nl, print('Expected:'), nl,
    print_square(Rows, Columns, Solution), nl.

test_puzzle8 :-
    puzzle8(Rows, Columns),
    solve(Rows, Columns, Square),
=======
    print_square(Rows, Columns, Solution).

test_puzzle9 :-
    puzzle9(NumSquares, Rows, Columns),
    solve(NumSquares, Rows, Columns),
>>>>>>> disjointApproach:Second Approach/tests.pl
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
<<<<<<< HEAD:tests.pl
    nl, print('Result:'), nl,
    print_square(Rows, Columns, Square), nl,
    nl, print('Expected:'), nl,
    print_square(Rows, Columns, Solution), nl.
=======
    print_square(Rows, Columns, Solution).
>>>>>>> disjointApproach:Second Approach/tests.pl
