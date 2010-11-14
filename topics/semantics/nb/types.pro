:- ['syntax.pro'].

%% see slide 126
% T-True
type(true,bool).

% T-False
type(false,bool).

% T-If
type(if(T1,T2,T3),T) :-
 type(T1,bool),
 type(T2,T),
 type(T3,T).

% T-Zero
type(zero,nat).

% T-Succ
type(succ(T),nat) :- type(T,nat).

% T-Pred
type(pred(T),nat) :- type(T,nat).

% T-Iszero
type(iszero(T),bool) :- type(T,nat).

