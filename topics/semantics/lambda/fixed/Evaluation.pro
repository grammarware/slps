%
% See slide 174
%
% Note the use of meta-variables in the formal notation.
% Those meta-variables must be mapped to extra literals as shown below.
%

:- ['Syntax.pro'].
:- ['Substitution.pro'].


% One-step transition relation

% see slide 189
onestep(fix(T1),fix(T2)) :-
 onestep(T1,T2).

onestep(fix(lam(X,XT,T1)),T2) :-
 substitute(fix(lam(X,XT,T1)),X,T1,T2).

onestep(app(T1,T2),app(T3,T2)) :-
  onestep(T1,T3).

onestep(app(V,T1),app(V,T2)) :-
  value(V),
  onestep(T1,T2).

onestep(app(lam(X,_,T1),V),T2) :-
  value(V),
  substitute(V,X,T1,T2).


% Extension to deal with Prolog numbers and Booleans

onestep(succ(T1),succ(T2)) :- 
  onestep(T1,T2).

onestep(succ(V1),V2) :- 
  value(V1),
  V2 is V1 + 1.

onestep(pred(T1),pred(T2)) :- 
  onestep(T1,T2).

onestep(pred(V1),V2) :- 
  value(V1),
  V2 is V1 - 1.

onestep(iszero(T1),iszero(T2)) :- 
  onestep(T1,T2).

onestep(iszero(V),true) :- 
  value(V),
  V == 0.

onestep(iszero(V),false) :- 
  value(V),
  \+ V == 0.

onestep(if(T1,T2,T3),if(T4,T2,T3)) :- 
  onestep(T1,T4).

onestep(if(true,T,_),T).

onestep(if(false,_,T),T).

% Fixed point (recursion)
% Evaluation rules from slide 189
% onestep(fix(T1),T2) :- ...

% Reflexive, transitive closure

manysteps(V,V) :-
  value(V).

manysteps(T1,T3) :- 
  onestep(T1,T2),
  manysteps(T2,T3).
