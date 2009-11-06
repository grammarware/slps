% Function lookup for functions represented as lists of pairs
lookup(M,X,Y) :- append(_,[(X,Y)|_],M).

% Function update in one position
update([],X,Y,[(X,Y)]).
update([(X,_)|M],X,Y,[(X,Y)|M]).
update([(X1,Y1)|M1],X2,Y2,[(X1,Y1)|M2]) :- \+ X1 = X2, update(M1,X2,Y2,M2).


