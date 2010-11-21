:- ensure_loaded('../shared/manysteps.pro').

eval(succ(T1),succ(T2)) :- eval(T1,T2).
eval(pred(zero),zero).
eval(pred(succ(N)),N).
eval(pred(T1),pred(T2)) :- eval(T1,T2).
eval(iszero(zero),true).
eval(iszero(T1),iszero(T2)) :- eval(T1,T2).
eval(iszero(succ(_)),false).
eval(if(true,T2,_),T2).
eval(if(false,_,T3),T3).
eval(if(T1,T2,T3),if(T4,T2,T3)) :- eval(T1,T4).
