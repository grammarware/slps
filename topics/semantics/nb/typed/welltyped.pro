welltyped(true,bool).
welltyped(false,bool).
welltyped(zero,nat).
welltyped(succ(T),nat) :- welltyped(T,nat).
welltyped(pred(T),nat) :- welltyped(T,nat).
welltyped(iszero(T),bool) :- welltyped(T,nat).
welltyped(if(T1,T2,T3),T) :-
 welltyped(T1,bool),
 welltyped(T2,T),
 welltyped(T3,T).
