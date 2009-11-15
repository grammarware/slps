:- ['syntax.pro'].

%% Small-step transition relation: slide 120
eval(E,V,T) :-
 evalstep(E,V,T),
 isfinal(V).

eval(E1,V,T) :-
 evalstep(E1,E2,T),
 eval(E2,V,T).

evalstep(true,tt,bool).
evalstep(false,ff,bool).
evalstep(zero,0,nat).
% E-Succ
evalstep(succ(E1),succ(E2),nat) :- evalstep(E1,E2,nat).
% E-PredZero
evalstep(pred(zero),zero,nat).
% E-PredSucc
evalstep(pred(succ(N)),N,nat).
% E-Pred
evalstep(pred(E1),pred(E2),nat) :- evalstep(E1,E2,nat).
% E-Iszero-Zero
evalstep(iszero(zero),tt,bool).
% E-Iszero
evalstep(iszero(T1),iszero(T2),bool) :- evalstep(T1,T2,nat).
% E-IszeroSucc
evalstep(iszero(succ(_)),ff,bool).
% E-IfTrue
evalstep(if(tt,T2,_),T2,_).
% E-IfFalse
evalstep(if(ff,_,T3),T3,_).
% E-If
evalstep(if(T1,T2,T3),if(T4,T2,T3),_) :- evalstep(T1,T4,bool).

isfinal(tt).
isfinal(ff).
isfinal(0).
isfinal(succ(T)) :- isfinal(T).
