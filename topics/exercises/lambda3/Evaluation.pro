% See slide 174
%
% Note the use of meta-variables in the formal notation.
% Those meta-variables must be mapped to extra literals as shown below.
%

:- ['Syntax.pro'].
:- ['Substitution.pro'].

step(app(T1,T2),app(T3,T2)) :-
  step(T1,T3).

step(app(V,T1),app(V,T2)) :-
  value(V),
  step(T1,T2).

step(app(lam(X,T1),V),T2) :-
  value(V),
  susbtitute(V,X,T1,T2).
