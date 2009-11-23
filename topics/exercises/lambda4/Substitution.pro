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

% Special case for alpha conversion

substitute(N,X,lam(Y,M1),lam(Z,M3)) :-
  \+ X == Y,
  fv(N,Xs),
  member(Y,Xs),
  freshvar(Xs,Z),
  substitute(var(Z),Y,M1,M2),
  substitute(N,X,M2,M3).

% Extension to deal with Prolog numbers and Booleans

substitute(_,_,true,true).
substitute(_,_,false,false).
substitute(_,_,N,N) :- number(N).
substitute(N,X,succ(M1),succ(M2)) :-
  substitute(N,X,M1,M2).
substitute(N,X,pred(M1),pred(M2)) :-
  substitute(N,X,M1,M2).
substitute(N,X,iszero(M1),iszero(M2)) :-
  substitute(N,X,M1,M2).
substitute(N,X,if(M1,M2,M3),if(M4,M5,M6)) :-
  substitute(N,X,M1,M4),
  substitute(N,X,M2,M5),
  substitute(N,X,M3,M6).

%
% freshvar(Xs,X): X is a variable not in Xs.
% We use numbers as generated variables.
% Variables and numbers can still not be confused (because variables are wrapped in var(...)). 
%

freshvar(Xs,X) :-
  freshvar(Xs,X,0).

freshvar(Xs,N,N) :- 
  \+ member(N,Xs).

freshvar(Xs,X,N1) :- 
  member(N1,Xs),
  N2 is N1 + 1,
  freshvar(Xs,X,N2).
