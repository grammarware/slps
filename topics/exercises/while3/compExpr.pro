%% Evaluate arithmetic expressions

% Number is evaluated to its value
evala(number(V),_,V).

% Variable reference is evaluated to its current value
evala(identifier(X),M,Y) :- lookup(M,X,Y).

% Adddition
evala(add(A1,A2),M,V) :-
  evala(A1,M,V1),
  evala(A2,M,V2),
  V is V1 + V2.

% Subtraction
evala(sub(A1,A2),M,V) :-
  evala(A1,M,V1),
  evala(A2,M,V2),
  V is V1 - V2.

% Multiplication
evala(mul(A1,A2),M,V) :-
  evala(A1,M,V1),
  evala(A2,M,V2),
  V is V1 * V2.

%% Evaluate Boolean expressions

% True is tt
evalb(true,_,tt).

% False is ff
evalb(false,_,ff).

% Negation
evalb(not(B),M,V) :-
 evalb(B,M,V1),
 not(V1,V).

% Test for equality
evalb(equals(A1,A2),M,V) :-
  evala(A1,M,V1),
  evala(A2,M,V2),
  equals(V1,V2,V).

% Test for equality
evalb(lte(A1,A2),M,V) :-
  evala(A1,M,V1),
  evala(A2,M,V2),
  lte(V1,V2,V).

%% Basic operations
equals(V1,V2,tt) :- V1 == V2.
equals(V1,V2,ff) :- \+ V1 == V2.
lte(V1,V2,tt) :- V1 =< V2.
lte(V1,V2,ff) :- \+ V1 =< V2.
not(tt,ff).
not(ff,tt).

