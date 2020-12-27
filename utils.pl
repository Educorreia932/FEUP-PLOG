% Builds a list with N elements and value X

build(_, 0, []).         % End of recursion

build(X, N, [X|T]) :-   % Adds N pieces to list
    N1 is N - 1,
    build(X, N1, T).    % Recursion