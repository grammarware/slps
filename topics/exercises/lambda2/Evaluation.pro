%
% See slide 174
%
% Note the use of meta-variables in the formal notation.
% Those meta-variables must be mapped to extra literals as shown below.
%

:- ['Syntax.pro'].
:- ['Substitution.pro'].


% One-step transition relation

onestep(app(T1,T2),app(T3,T2)) :-
  onestep(T1,T3).

onestep(app(V,T1),app(V,T2)) :-
  value(V),
  onestep(T1,T2).

onestep(app(lam(X,T1),V),T2) :-
  value(V),
  substitute(V,X,T1,T2).


% Reflexive, transitive closure

manysteps(V,V) :- value(V).
manysteps(T1,T3) :- onestep(T1,T2), manysteps(T2,T3).
