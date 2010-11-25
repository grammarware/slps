:- ['../../lambda/typed/erase.pro'].

erase(letrec(X,_,T1,T2),T4)
 :-
    freevars(T1,FV),
    freshvar(FV,Y),
    substitute(var(Y),X,T1,T3),
    substitute(fix(lam(Y,T3)),X,T2,T4).

