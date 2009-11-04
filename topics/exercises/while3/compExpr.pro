evalb(true,_,tt).

evalb(false,_,ff).

evalb(equal(A1,A2),M,V) :-
  evala(A1,M,V1),
  evala(A2,M,V2),
  equal(V1,V2,V).

equal(V1,V2,tt) :- V1 == V2.
equal(V1,V2,ff) :- \+ V1 == V2.
