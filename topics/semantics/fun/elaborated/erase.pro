:- ['../../lambda/typed/erase.pro'].

/*

We do not need to provide a reduction semantics for letrec because we
can map the construct to a pattern based on the fixed point
combinator. To this end, we register an erase/2 clause. (The predicate
erase/2 is applied before the reduction semantics is invoked; see
modules shared/elaborate.pro and shared/main/elaborated.pro.) Given
letrec(X,_,T1,T2), erasure forms a fixed point expression of T1, and
substitutes X by that fixed point expression in T2.

*/

erase(letrec(X,_,T1,T2),T4)
 :-
    freevars(T1,FV),
    freshvar(FV,Y),
    substitute(var(Y),X,T1,T3),
    substitute(fix(lam(Y,T3)),X,T2,T4).
