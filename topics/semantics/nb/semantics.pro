% Transitive closure of small-step transition relation

eval(E,V,T) :-
 evalstep(E,V,T),
 isfinal(V).

eval(E1,V,T) :-
 evalstep(E1,E2,T),
 eval(E2,V,T).


% Small-step transition relation

evalstep(true,tt,bool).
evalstep(false,ff,bool).
evalstep(zero,0,nat).
evalstep(succ(E1),succ(E2),nat) :- evalstep(E1,E2,nat).
evalstep(pred(zero),zero,nat).
evalstep(pred(succ(N)),N,nat).
evalstep(pred(E1),pred(E2),nat) :- evalstep(E1,E2,nat).
evalstep(iszero(zero),tt,bool).
evalstep(iszero(T1),iszero(T2),bool) :- evalstep(T1,T2,nat).
evalstep(iszero(succ(_)),ff,bool).
evalstep(if(tt,T2,_),T2,_).
evalstep(if(ff,_,T3),T3,_).
evalstep(if(T1,T2,T3),if(T4,T2,T3),_) :- evalstep(T1,T4,bool).


% Values (normal forms)

value(tt).
value(ff).
value(0).
value(succ(T)) :- value(T).
