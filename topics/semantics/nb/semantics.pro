% Transitive, reflexive closure of small-step transition relation

eval(V,V) :- value(V).
eval(T1,V) :- evalstep(T1,T2), eval(T2,V).


% Small-step transition relation

evalstep(succ(T1),succ(T2)) :- evalstep(T1,T2).
evalstep(pred(zero),zero).
evalstep(pred(succ(N)),N).
evalstep(pred(T1),pred(T2)) :- evalstep(T1,T2).
evalstep(iszero(zero),true).
evalstep(iszero(T1),iszero(T2)) :- evalstep(T1,T2).
evalstep(iszero(succ(_)),false).
evalstep(if(true,T2,_),T2).
evalstep(if(false,_,T3),T3).
evalstep(if(T1,T2,T3),if(T4,T2,T3)) :- evalstep(T1,T4).
