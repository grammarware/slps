:- ['syntax.pro'].
:- ['substitution.pro'].
:- ['fv.pro'].
:- ['evaluation.pro'].

:-
 % Church numerals 0 and 1 
 C0 = lam(s,lam(z,var(z))),
 C1 = lam(s,lam(z,app(var(s),var(z)))),

 % Successor function
 Succ = lam(n,lam(s,lam(z,app(var(s),app(app(var(n),var(s)),var(z)))))),

 % Testing reduction
 write('C1 is      '), write(C1), nl,
 manysteps(app(Succ,C0),Q),
 write('succ C0 is '),  write(Q), nl.

:- halt.
