% Big-step transition relation for statement execution

execute(skip,M,M).

execute(assign(X,A),M1,M2) :- evala(A,M1,Y), update(M1,X,Y,M2).

