:- ['../../lambda/typed/term.pro'].

term(letrec(X,A,T1,T2))
 :-
    variable(X),
    type(A),
    term(T1),
    term(T2).

