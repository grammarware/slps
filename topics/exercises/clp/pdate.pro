:- use_module(library(clpfd)).

pdate(Vars)
 :-
    Vars = [M1,M2,D1,D2,Y1,Y2,Y3,Y4],
    reverse(Vars,Vars),

    % Overall range
    Year in 1380..2010,
    Month in 1..12,
    Day in 1..31,
  
    % Determine digits by modulo arithmetic
    Y1 #= Year / 1000,
    Y2 #= (Year - 1000 * Y1) / 100,
    Y3 #= (Year - 1000 * Y1 - 100 * Y2) / 10,
    Y4 #= Year - 1000 * Y1 - 100 * Y2 - Y3 * 10,
    M1 #= Month / 10,
    M2 #= Month - M1 * 10,
    D1 #= Day / 10,
    D2 #= Day - D1 * 10,

    % Check validity
    valid(Month,Day).

valid(N,M) :- M #=< 31, member(N,[1,3,5,7,8,10,12]).
valid(N,M) :- M #=< 30, member(N,[4,6,9,11]).
valid(2,M) :- M #=< 29. % Actually, Feb 29 maps to year 9220...

/*

?- pdate(X).
X = [0, 1, 0, 2, 2, 0, 1, 0] ;
X = [0, 8, 3, 1, 1, 3, 8, 0] ;
X = [1, 0, 0, 2, 2, 0, 0, 1] ;
false.

*/
