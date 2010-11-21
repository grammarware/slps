freevars(var(X),[X]).

freevars(app(M,N),FV) :-
  freevars(M,FV1),
  freevars(N,FV2),
  union(FV1,FV2,FV).

freevars(lam(X,M),FV) :-
  freevars(M,FV1),
  subtract(FV1,[X],FV).
