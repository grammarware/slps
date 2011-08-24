% This code is not invoked anywhere.
% It only serves as type declaration / documentation.
% Somewhat more idiosyncratic code to be found in btf2bgf.pro

tree(true).
tree(t(T)) :- atom(T).
tree(n(P,T)) :- prod(P), tree(T).
tree(','(Ts)) :- maplist(tree,Ts).
tree(';'(X,T)) :- expr(X), tree(T).
tree('?'(Ts)) :- mapopt(tree,Ts).
tree('*'(Ts)) :- maplist(tree,Ts).
tree('+'(Ts)) :- maplist1(tree,Ts).
