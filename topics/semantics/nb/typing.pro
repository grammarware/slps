type(true,bool).
type(false,bool).
type(zero,nat).
type(succ(T),nat) :- type(T,nat).
type(pred(T),nat) :- type(T,nat).
type(iszero(T),bool) :- type(T,nat).

type(if(T1,T2,T3),T) :-
 type(T1,bool),
 type(T2,T),
 type(T3,T).

