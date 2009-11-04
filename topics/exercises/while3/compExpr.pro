% Evaluate Boolean expressions

evalb(true,_,tt).

evalb(false,_,ff).

evalb(equal(A1,A2),M,V) :-
  evala(A1,M,V1),
  evala(A2,M,V2),
  equal(V1,V2,V).


% Evaluate arithmetic expressions

evala(const(V),_,V).

evala(var(X),M,Y) :- lookup(M,X,Y).


% Basic operations

equal(V1,V2,tt) :- V1 == V2.
equal(V1,V2,ff) :- \+ V1 == V2.
