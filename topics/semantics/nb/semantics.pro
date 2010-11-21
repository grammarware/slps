% Transitive, reflexive closure of small-step transition relation

manysteps(V,V) :- value(V).
manysteps(T1,V) :- onestep(T1,T2), manysteps(T2,V).


% Small-step transition relation

onestep(succ(T1),succ(T2)) :- onestep(T1,T2).
onestep(pred(zero),zero).
onestep(pred(succ(N)),N).
onestep(pred(T1),pred(T2)) :- onestep(T1,T2).
onestep(iszero(zero),true).
onestep(iszero(T1),iszero(T2)) :- onestep(T1,T2).
onestep(iszero(succ(_)),false).
onestep(if(true,T2,_),T2).
onestep(if(false,_,T3),T3).
onestep(if(T1,T2,T3),if(T4,T2,T3)) :- onestep(T1,T4).
