:- use_module(library(clpfd)).

:- include('utils.pl').

solve(RowsNumbers, ColumnsNumbers, Rows) :-
    % Domain and variables definition

    length(RowsNumbers, Size),                      % Get size of square
    generate_grid(Rows, Size),                      % Generate grid representing square
    generate_indexes(Indexes, Size),                % Generate cells indexes

    % Constraints
    
    transpose(Rows, Columns),                       % Transpose rows matrix to get columns
    line_constraints(RowsNumbers, Rows),            % Apply row constraints
    line_constraints(ColumnsNumbers, Columns),      % Apply columns constraints
    square_constraint(0, 0, Rows, Indexes, Size),   % Apply square constraints

    % Solution search
    
    flatten(Rows, Vars),
    labeling([], Vars).


% Line constraints

line_constraints([], []).

line_constraints([FilledCells|T1], [GridLine|T2]) :-
    sum(GridLine, #=, FilledCells),
    line_constraints(T1, T2).

% Square constraints

square_constraint(Size, _, _, _, Size).                 % Reached end of grid

square_constraint(I, Size, Rows, Indexes, Size) :-      % Reached end of row
    NewI is I + 1,                                      % Skip to next row
    square_constraint(NewI, 0, Rows, Indexes, Size).

square_constraint(I, J, Rows, Indexes, Size) :-
    isUpperLeftCorner(I, J, Rows, IsUpperLeftCorner),
    isSquare(I, J, Rows, Indexes, IsSquare),
    IsUpperLeftCorner #=> IsSquare, 

    NextJ is J + 1,
    square_constraint(I, NextJ, Rows, Indexes, Size).

isUpperLeftCorner(I, J, Rows, IsUpperLeftCorner) :-
    get_cell(I, J, Rows, Cell),                                      
    (Cell #= 1) #<=> IsFilled,

    % Top cell

    TopI is I - 1,
    get_cell(TopI, J, Rows, TopCell),
    (TopCell #= 0) #<=> IsTopBlank,
    
    % Left cell
    
    LeftJ is J - 1,
    get_cell(I, LeftJ, Rows, LeftCell),
    (LeftCell #= 0) #<=> IsLeftBlank,

    % Upper left cell   
    
    get_cell(TopI, LeftJ, Rows, TopLeftCell),
    (TopLeftCell #= 0) #<=> IsTopLeftBlank,

    (IsFilled #/\ IsTopBlank #/\ IsLeftBlank #/\ IsTopLeftBlank) #=> IsUpperLeftCorner.

isSquare(FirstI, FirstJ, Rows, Indexes, IsSquare) :-
    BottomI - FirstI + 1 #= Height,                             % Last row - first row + 1 = height                    
    RightJ - FirstJ + 1 #= Width,                               % Last column - first column + 1 = width

    get_cell(BottomI, RightJ, Rows, BottomRightCorner),         % Get Bottom right corner                    
    get_cell(FirstI, FirstJ, Rows, UpperLeftCorner),            % Get Upper left corner

    SquareArea #= Height * Width,                               % Square Area must be Height * width
    SquareBorder #= (Height + 2) * (Width + 2) - SquareArea,    % Number of cells of the squares border

    (
        foreach([I, J], Indexes),           % Iterate Indexes
        foreach(FilledCell, FilledCells),   % Iterate FilledCells
        foreach(BorderCell, BorderCells),   % Iterate BorderCells
        param(FirstI),              
        param(BottomI),
        param(FirstJ),
        param(RightJ),
        param(Rows)
    do
        get_cell(I, J, Rows, Cell),         % Get current cells                  
        
        FilledCell #<=> (                   % Set Filled Cell value
            I #>= FirstI #/\                % I is greater than or equal to first row's index
            I #=< BottomI #/\               % I is less than or equal to last row's index
            J #>= FirstJ #/\                % J is greater or equal to first column's index
            J #=< RightJ #/\                % J is less than or equal to last column's index
            Cell #= 1                       % Cell is filled
        ),

        I1 #= FirstI - 1,  % Top
        J1 #= FirstJ - 1,  % Left
        I2 #= BottomI + 1, % Bottom
        J2 #= RightJ + 1,  % Right 

        BorderCell #<=> (
            (
                (I #= I1 #/\ J #>= J1 #/\ J #=< J2) #\    % Cell is at Top border
                (I #= I2 #/\ J #>= J1 #/\ J #=< J2) #\    % Cell is at Bottom border
                (J #= J1 #/\ I #>= I1 #/\ I #=< I2) #\    % Cell is at Left border
                (J #= J2 #/\ I #>= I1 #/\ I #=< I2)       % Cell is at Right border
            ) #/\       

            Cell #= 0   % Border of square must be empty -> disjoint squares
        )

    ),

    sum(FilledCells, #=, NumFilledCells),   % Constraint number of filled cells
    sum(BorderCells, #=, NumBorderCells),   % Constraint number of border cells

    (
        (Height #= Width) #/\ (Height #>= 1) #/\ (Width #>= 1) #/\  % Width & Height constraints
        (BottomRightCorner #= 1) #/\ (UpperLeftCorner #= 1) #/\     % Corners constraints
        (NumFilledCells #= SquareArea) #/\                          % Number of filled cells constraints
        (NumBorderCells #= SquareBorder)                            % Number of border cells constraints
    ) #=> IsSquare.
