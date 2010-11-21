:- ['term.pro'].
:- ['value.pro'].
:- ['substitution.pro'].
:- ['fv.pro'].
:- ['evaluation.pro'].

:-
 % Church numerals 0 and 1 
 C0 = lam(s,lam(z,var(z))),
 C1 = lam(s,lam(z,app(var(s),var(z)))),

 % Church Booleans
 TRUE = lam(t,lam(f,var(t))),
 _FALSE = lam(t,lam(f,var(f))),
 TEST = lam(l,lam(m,lam(n,app(app(var(l),var(m)),var(n))))),

 % Successor function
 Succ = lam(n,lam(s,lam(z,app(var(s),app(app(var(n),var(s)),var(z)))))),

 % Testing reduction
 write('C1 is '), write(C1), nl,
 manysteps(app(Succ,C0),Q1),
 write('succ C0 is '),  write(Q1), nl,
 manysteps(app(app(app(TEST,TRUE),C0),C1),Q2),
 write('TEST TRUE C0 C1 is '),  write(Q2), nl.

:- halt.
