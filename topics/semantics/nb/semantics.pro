% Transitive closure of small-step transition relation

eval(E,V) :-
 evalstep(E,V),
 value(V).

eval(E1,V) :-
 evalstep(E1,E2),
 eval(E2,V).


% Small-step transition relation

evalstep(true,tt).
evalstep(false,ff).
evalstep(zero,0).
evalstep(succ(E1),succ(E2)) :- evalstep(E1,E2).
evalstep(pred(zero),zero).
evalstep(pred(succ(N)),N).
evalstep(pred(E1),pred(E2)) :- evalstep(E1,E2).
evalstep(iszero(zero),tt).
evalstep(iszero(T1),iszero(T2)) :- evalstep(T1,T2).
evalstep(iszero(succ(_)),ff).
evalstep(if(tt,T2,_),T2).
evalstep(if(ff,_,T3),T3).
evalstep(if(T1,T2,T3),if(T4,T2,T3)) :- evalstep(T1,T4).


% Values (normal forms)

value(tt).
value(ff).
value(0).
value(succ(T)) :- value(T).
