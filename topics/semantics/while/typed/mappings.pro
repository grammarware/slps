% Function lookup for functions represented as lists of pairs
lookup(M,X,Y) :- append(_,[(X,Y)|_],M).
isdeclared(D,X) :- member(X,D).

% Fresh variables can be declared
declare([],X,[X]).
declare([H|T1],X,[H|T2]) :-
 \+ H = X,
 declare(T1,X,T2).

% Variable values can be updated
update([(X,_)|M],X,Y,[(X,Y)|M]).
update([(X1,Y1)|M1],X2,Y2,[(X1,Y1)|M2]) :-
 \+ X1 = X2,
 update(M1,X2,Y2,M2).


