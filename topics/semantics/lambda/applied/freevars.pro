freevars(true,[]).
freevars(false,[]).
freevars(zero,[]).
freevars(succ(T),FV) :- freevars(T,FV).
freevars(pred(T),FV) :- freevars(T,FV).
freevars(iszero(T),FV) :- freevars(T,FV).
freevars(if(T1,T2,T3),FV) :-
  freevars(T1,FV1),
  freevars(T2,FV2),
  freevars(T3,FV3),
  union(FV1,FV2,FV12),
  union(FV12,FV3,FV).
