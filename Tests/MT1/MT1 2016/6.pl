:- consult('info.pl').
:- consult('4.pl').

similarity(Film1, Film2, Similarity) :-
    film(Film1, Categories1, Duration1, AvgScore1),
    film(Film2, Categories2, Duration2, AvgScore2),
    elemsComuns(Categories1, CommonCat, Categories2),                      % List with ommon categories
    length(CommonCat, NumberCommonCat),                                    % Number of common categories
    append(Categories1, Categories2, TotalCategories),                     % List with all categories
    sort(TotalCategories, NewTotalCategories),                             % Remove duplicates from list with all the categories
    length(NewTotalCategories, TotalNumberCategories),                     % Total number of categories
    PercentCommonCat is NumberCommonCat / TotalNumberCategories * 100,     % Percentage of common categories
    DurDif is abs(Duration2 - Duration1),                                  % Difference between movie durations
    ScoreDiff is abs(AvgScore2 - AvgScore1),                               % Difference between movie scores
    Similarity is PercentCommonCat - 3 * DurDif - 5 * ScoreDiff.           % Similarity between the two movies
