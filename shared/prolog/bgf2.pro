g(Ps) :- maplist(p,Ps).

p(p(_,_,X)) :- x(X).

x(true).
x(t(T)) :- atom(T).
x(n(N)) :- atom(N).
x(','(Xs)) :- maplist(x,Xs).
x(';'(Xs)) :- maplist(x,Xs).
x('?'(X)) :- x(X).
x('*'(X)) :- x(X).
x('+'(X)) :- x(X).
