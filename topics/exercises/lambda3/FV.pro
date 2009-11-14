% See slide 153

:- ['Syntax.pro'].

fv(var(X),[X]).

fv(app(M,N),FV) :-
  fv(M,FV1),
  fv(N,FV2),
  union(FV1,FV2,FV).

fv(lam(X,M),FV) :-
  fv(M,FV1),
  subtract(FV1,[X],FV).
