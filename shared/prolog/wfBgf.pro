grammar(Ps) :- maplist(prod,Ps).

prod(p(_,_,X)) :- expr(X).

expr(true).
expr(t(T)) :- atom(T).
expr(n(N)) :- atom(N).
expr(','(Xs)) :- maplist(expr,Xs).
expr(';'(Xs)) :- maplist(expr,Xs).
expr('?'(X)) :- expr(X).
expr('*'(X)) :- expr(X).
expr('+'(X)) :- expr(X).
