:- ['Typing.pro'].
:- ['Evaluation.pro'].

%
% Testing typing rules
%
:- 
 % taken from slide 185
 % |-  (λf:bool→bool . f false) λg:bool . g  :  bool
 hastype([],app(lam(f,maps(bool,bool),app(var(f),false)),lam(g,bool,var(g))),T1),
 write(T1), nl,
 % |-  λx:nat . λy:nat if x==0 then succ(x) else pred(y)  :  nat
 If = lam(x,nat,lam(y,nat,if(iszero(var(x)),succ(var(x)),pred(var(y))))),
 hastype([],If,T2),
 write(T2), nl,
 % taken from slide 190
 % |-  λe:nat→bool . λx:nat if x==0 then true else if (pred x)==0 then false else (e (pred pred x))  :  (nat→bool)→nat→bool
 F = lam(e,maps(nat,bool),lam(x,nat,if(iszero(var(x)),true,if(iszero(pred(var(x))),false,app(var(e),pred(pred(var(x)))))))),
 hastype([],F,T3),
 write(T3), nl,
 IsEven = fix(F),
 hastype([],IsEven,T4),
 write(T4), nl,
 manysteps(app(IsEven,3),Q),
 write(Q), nl,
 halt.
