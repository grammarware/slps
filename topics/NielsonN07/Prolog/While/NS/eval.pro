/*

(C) 2010, Ralf Laemmel
Expression evaluation for the While language

*/


% Number is evaluated to its value

evala(num(V),_,V).


% Variable reference is evaluated to its current value

evala(id(X),M,Y) :- lookup(M,X,Y).


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


% True is tt

evalb(true,_,tt).


% False is ff

evalb(false,_,ff).


% Negation

evalb(not(B),M,V) :-
 evalb(B,M,V1),
 not(V1,V).


% Conjunction

evalb(and(B1,B2),M,V) :-
 evalb(B1,M,V1),
 evalb(B2,M,V2),
 and(V1,V2,V).


% Test for equality

evalb(eq(A1,A2),M,V) :-
  evala(A1,M,V1),
  evala(A2,M,V2),
  eq(V1,V2,V).


% Test for being less than or equal

evalb(lte(A1,A2),M,V) :-
  evala(A1,M,V1),
  evala(A2,M,V2),
  lte(V1,V2,V).


% Basic operations

eq(V1,V2,tt) :- V1 == V2.
eq(V1,V2,ff) :- \+ V1 == V2.

lte(V1,V2,tt) :- V1 =< V2.
lte(V1,V2,ff) :- \+ V1 =< V2.

not(tt,ff).
not(ff,tt).

and(tt,tt,tt).
and(ff,_,ff).
and(_,ff,ff).
