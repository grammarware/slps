% Type of grammars.
% This code is not invoked anywhere.
% It only serves as type declaration / documentation.
% Somewhat more idiosyncratic code to be found in modules *bgf*.pro.

grammar(Ps) :- maplist(prod,Ps).

prod(p(L,N,X)) :- mapopt(atom(L)), atom(N), expr(X).

expr(true).
expr(t(T)) :- atom(T).
expr(n(N)) :- atom(N).
expr(','(Xs)) :- maplist(expr,Xs).
expr(';'(Xs)) :- maplist(expr,Xs).
expr('?'(X)) :- expr(X).
expr('*'(X)) :- expr(X).
expr('+'(X)) :- expr(X).
