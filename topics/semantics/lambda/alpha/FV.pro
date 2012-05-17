% See slide 153

:- ['Syntax.pro'].

fv(var(X),[X]).

fv(app(M,N),FV) :-
  fv(M,FV1),
  fv(N,FV2),
  union(FV1,FV2,FV).

fv(lam(X,_,M),FV) :-
  fv(M,FV1),
  subtract(FV1,[X],FV).


% Extension to deal with Prolog numbers and Booleans

fv(true,[]).
fv(false,[]).
fv(N,[]) :- number(N).
fv(pred(M),FV) :- fv(M,FV).
fv(succ(M),FV) :- fv(M,FV).
fv(iszero(M),FV) :- fv(M,FV).
fv(if(M1,M2,M3),FV) :- fv(M1,FV1), fv(M2,FV2), fv(M3,FV3),   union(FV1,FV2,FV4), union(FV4,FV3,FV).
