% See slide 157

:- ['Syntax.pro'].
:- ['FV.pro'].

substitute(N,X,var(X),N).
substitute(_,X,var(Y),var(Y)) :- \+ X == Y.
substitute(N,X,app(M1,M2),app(M3,M4)) :-
  substitute(N,X,M1,M3),
  substitute(N,X,M2,M4).
substitute(_,X,lam(X,M),lam(X,M)).
substitute(N,X,lam(Y,M1),lam(Y,M2)) :-
  \+ X == Y,
  fv(N,Xs),
  \+ member(Y,Xs),
  substitute(N,X,M1,M2).
