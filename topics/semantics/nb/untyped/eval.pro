:- ensure_loaded('../../shared/manysteps.pro').

% eval(pred(zero),zero).
eval(pred(succ(NV)),NV) :- nvalue(NV).
eval(succ(T1),succ(T2)) :- eval(T1,T2).
eval(pred(T1),pred(T2)) :- eval(T1,T2).
eval(iszero(zero),true).
eval(iszero(succ(NV)),false) :- nvalue(NV).
eval(iszero(T1),iszero(T2)) :- eval(T1,T2).
eval(if(true,T2,_),T2).
eval(if(false,_,T3),T3).
eval(if(T1,T2,T3),if(T4,T2,T3)) :- eval(T1,T4).

